# app.R
# BRFSS Shiny Dashboard - Fixed aggregation, NA handling, and CI display

library(shiny)
library(tidyverse)
library(scales)

# -----------------------------
# Helper functions
# -----------------------------
state_to_region <- function(location_abbr) {
  case_when(
    location_abbr %in% c("CT","ME","MA","NH","RI","VT") ~ "Northeast",
    location_abbr %in% c("IL","IN","MI","OH","WI") ~ "Midwest",
    location_abbr %in% c("DE","DC","MD","NJ","NY","PA") ~ "Mid-Atlantic",
    location_abbr %in% c("AL","AR","FL","GA","KY","LA","MS","NC","SC","TN","VA","WV") ~ "Southeast",
    location_abbr %in% c("AZ","NM","OK","TX") ~ "Southwest",
    location_abbr %in% c("IA","KS","MN","MO","NE","ND","SD") ~ "Plains",
    location_abbr %in% c("AK","CA","HI","OR","WA") ~ "West Coast",
    location_abbr %in% c("CO","ID","MT","NV","UT","WY") ~ "Mountain",
    TRUE ~ "Other/Territory"
  )
}

merge_ResponseID <- function(char_vec) {
  char_vec <- str_replace(char_vec, "RESP025", "RESP137")
  char_vec <- str_replace(char_vec, "RESP026", "RESP172")
  char_vec <- str_replace(char_vec, "RESP029", "RESP141")
  char_vec <- str_replace(char_vec, "RESP230", "RESP020")
  char_vec <- str_replace(char_vec, "RESP231", "RESP020")
  char_vec <- str_replace(char_vec, "RESP232", "RESP020")
  char_vec <- str_replace(char_vec, "RESP196", "RESP199")
  char_vec <- str_replace(char_vec, "RESP197", "RESP199")
  char_vec <- str_replace(char_vec, "RESP198", "RESP199")
  char_vec <- str_replace(char_vec, "RESP199", "RESP199")
  char_vec <- str_replace(char_vec, "RESP200", "RESP008")
  char_vec <- str_replace(char_vec, "RESP194", "RESP005")
  char_vec <- str_replace(char_vec, "RESP195", "RESP006")
  char_vec
}

merge_Response <- function(ResponseID, Response) {
  Response <- as.character(Response)
  idx <- str_detect(ResponseID, "RESP137"); Response[idx] <- "Employed"
  idx <- str_detect(ResponseID, "RESP172"); Response[idx] <- "Self-employed"
  idx <- str_detect(ResponseID, "RESP141"); Response[idx] <- "Homemaker"
  idx <- str_detect(ResponseID, "RESP020"); Response[idx] <- "$50,000+"
  idx <- str_detect(ResponseID, "RESP199"); Response[idx] <- "A/A Native, Asian,Other"
  idx <- str_detect(ResponseID, "RESP008"); Response[idx] <- "Multiracial"
  idx <- str_detect(ResponseID, "RESP005"); Response[idx] <- "White"
  idx <- str_detect(ResponseID, "RESP006"); Response[idx] <- "Black"
  Response <- tolower(Response)
  Response
}

merge_BreakoutID <- function(char_vec) {
  char_vec <- str_replace(char_vec, "INCOME01", "INCOME1")
  char_vec <- str_replace(char_vec, "INCOME02", "INCOME2")
  char_vec <- str_replace(char_vec, "INCOME03", "INCOME3")
  char_vec <- str_replace(char_vec, "INCOME04", "INCOME4")
  char_vec <- str_replace(char_vec, "INCOME05", "INCOME5")
  char_vec <- str_replace(char_vec, "INCOME06", "INCOME5")
  char_vec <- str_replace(char_vec, "INCOME07", "INCOME5")
  char_vec <- str_replace(char_vec, "RACE01", "RACE1")
  char_vec <- str_replace(char_vec, "RACE02", "RACE2")
  char_vec <- str_replace(char_vec, "RACE08", "RACE3")
  char_vec <- str_replace(char_vec, "RACE04", "RACE4")
  char_vec <- str_replace(char_vec, "RACE05", "RACE4")
  char_vec <- str_replace(char_vec, "RACE06", "RACE4")
  char_vec <- str_replace(char_vec, "RACE03", "RACE4")
  char_vec <- str_replace(char_vec, "RACE07", "RACE5")
  char_vec
}

merge_Break_Out <- function(BreakoutID, Break_Out) {
  Break_Out <- as.character(Break_Out)
  idx <- str_detect(BreakoutID, "INCOME5"); Break_Out[idx] <- "$50,000+"
  idx <- str_detect(BreakoutID, "RACE1"); Break_Out[idx] <- "White"
  idx <- str_detect(BreakoutID, "RACE2"); Break_Out[idx] <- "Black"
  idx <- str_detect(BreakoutID, "RACE3"); Break_Out[idx] <- "Hispanic"
  idx <- str_detect(BreakoutID, "RACE4"); Break_Out[idx] <- "A/A Native, Asian,Other"
  idx <- str_detect(BreakoutID, "RACE5"); Break_Out[idx] <- "Multiracial"
  # Convert to lowercase for consistency
  Break_Out <- tolower(Break_Out)
  Break_Out
}

safe_num <- function(x) as.numeric(as.character(x))

# -----------------------------
# Load CSV
# -----------------------------
load_data <- function(path = ".") {
  fileList <- list.files(path = path, pattern = "Prevalence.*\\.csv$", full.names = TRUE)
  if (length(fileList) == 0) stop("No Prevalence.*.csv file found; place it in the app directory.")
  df <- readr::read_csv(fileList[1], show_col_types = FALSE)
  names(df) <- names(df) %>% str_replace_all("\\s+", "_")
  layerQ <- df %>% select(any_of(c("Class","Topic","Question"))) %>% distinct() %>% arrange(Class, Topic, Question)
  list(df = df, layerQ = layerQ)
}

# -----------------------------
# FIXED Aggregation engine
# -----------------------------
aggregate_data <- function(data, breakout_category_id, breakout_col_name, apply_merges = TRUE, region_aggregate = FALSE) {
  # 1. Filter rows for the breakout category
  qDf <- data %>% filter(BreakOutCategoryID == breakout_category_id | breakout_category_id == "CAT1")
  qDf <- qDf %>% filter(!(Locationabbr %in% c("US","UW")) | is.na(Locationabbr))
  
  if (nrow(qDf) == 0) return(tibble())
  
  # 2. CRITICAL: Filter out NA values early
  qDf <- qDf %>% 
    filter(!is.na(Data_value), !is.na(Sample_Size), Data_value > 0, Sample_Size > 0)
  
  if (nrow(qDf) == 0) return(tibble())
  
  # 3. Normalize responses and convert to lowercase
  qDf <- qDf %>%
    mutate(
      ResponseID = merge_ResponseID(as.character(ResponseID)),
      Response = merge_Response(ResponseID, Response),
      # Also convert Break_Out to lowercase for consistent matching
      Break_Out = tolower(as.character(Break_Out))
    )
  
  # 4. Apply merges for compressed "Summary (Less)" mode
  # Only merge breakout categories if apply_merges is TRUE
  if (apply_merges && breakout_category_id %in% c("CAT3","CAT4","CAT5","CAT6")) {
    qDf <- qDf %>%
      mutate(
        BreakoutID = merge_BreakoutID(as.character(BreakoutID)),
        Break_Out = merge_Break_Out(BreakoutID, Break_Out)
      )
  }
  
  # Note: ResponseID merging happens regardless of detail level
  # because those are truly duplicate responses (e.g., "Emplyd" vs "Employed")
  
  # 5. Region aggregation?
  if (region_aggregate) {
    qDf <- qDf %>% mutate(National_Region = state_to_region(Locationabbr))
    breakout_col_name <- "National_Region"
  }
  
  # 6. Year column detection
  year_col <- intersect(names(qDf), c("Year","year","TimeFrameStart"))
  year_col_name <- if (length(year_col) > 0) year_col[1] else NULL
  
  # 7. Decide grouping vars
  if (breakout_category_id == "CAT1") {
    if (!is.null(year_col_name) && breakout_col_name == "Year") {
      group_vars <- c(year_col_name, "Response")
    } else if (breakout_col_name %in% c("Locationabbr", "National_Region")) {
      group_vars <- c(breakout_col_name, "Response")
    } else {
      group_vars <- c("Response")
    }
  } else {
    group_vars <- c("Break_Out", "Response")
  }
  
  # 8. CORRECTED AGGREGATION (following PDF methodology)
  # Approach: For each group (e.g., Age Group), we need to:
  # 1. Find all records for that group across all states/years
  # 2. For each response, sum the person counts
  # 3. The denominator is the total of ALL person counts in that group
  
  # First pass: calculate total persons in the entire grouping
  plotDf <- qDf %>%
    mutate(persons = Sample_Size) %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(
      response_persons = sum(persons, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Second pass: calculate the denominator (total across all responses in the group)
  if (breakout_category_id == "CAT1") {
    if (!is.null(year_col_name) && breakout_col_name == "Year") {
      denominator_groups <- c(year_col_name)
    } else if (breakout_col_name %in% c("Locationabbr", "National_Region")) {
      denominator_groups <- c(breakout_col_name)
    } else {
      denominator_groups <- NULL
    }
  } else {
    denominator_groups <- c("Break_Out")
  }
  
  if (!is.null(denominator_groups)) {
    plotDf <- plotDf %>%
      group_by(across(all_of(denominator_groups))) %>%
      mutate(total_persons = sum(response_persons, na.rm = TRUE)) %>%
      ungroup()
  } else {
    plotDf <- plotDf %>%
      mutate(total_persons = sum(response_persons, na.rm = TRUE))
  }
  
  # Calculate percentages and confidence intervals
  plotDf <- plotDf %>%
    mutate(
      agg_percent = if_else(total_persons > 0, response_persons * 100 / total_persons, NA_real_),
      agg_percent_sdev = sqrt(agg_percent * (100 - agg_percent) / total_persons),
      agg_low_ci_limit = agg_percent - 2 * agg_percent_sdev,
      agg_high_ci_limit = agg_percent + 2 * agg_percent_sdev
    ) %>%
    select(-response_persons, -total_persons, -agg_percent_sdev)
  
  # 9. Factor ordering for display
  if ("Break_Out" %in% names(plotDf)) {
    if (breakout_category_id == "CAT3") { # Age
      if (apply_merges) {
        # Summary mode: standard age groups
        age_order <- c("18-24","25-34","35-44","45-54","55-64","65+")
      } else {
        # Detailed mode: include all possible age breakouts
        age_order <- c("18-24","21-25","21-30","25-34","26-35","31-40","35-44","36-45",
                       "40-49","41-50","45-54","46-55","50-59","51-60","55-64","56-65",
                       "60-64","60-69","61-65","65+","65-74","65-75","70-74","70-75","70-80","75+")
      }
      ord <- intersect(age_order, unique(plotDf$Break_Out))
      if (length(ord) > 0) plotDf$Break_Out <- factor(plotDf$Break_Out, levels = ord, ordered = TRUE)
      
    } else if (breakout_category_id == "CAT6") { # Income
      if (apply_merges) {
        # Summary mode: 5 categories
        inc_order <- c("less than $15,000","$15,000-$24,999","$25,000-$34,999","$35,000-$49,999","$50,000+")
      } else {
        # Detailed mode: 7 categories
        inc_order <- c("less than $15,000","$15,000-$24,999","$25,000-$34,999","$35,000-$49,999",
                       "$50,000-$99,999","$100,000-$199,999","$200,000+")
      }
      ord <- intersect(inc_order, unique(plotDf$Break_Out))
      if (length(ord) > 0) plotDf$Break_Out <- factor(plotDf$Break_Out, levels = ord, ordered = TRUE)
      
    } else if (breakout_category_id == "CAT4") { # Race
      if (apply_merges) {
        # Summary mode: 5 merged categories
        race_order <- c("white","black","hispanic","a/a native, asian,other","multiracial")
      } else {
        # Detailed mode: 8 original categories
        race_order <- c("white, non-hispanic","black, non-hispanic","hispanic",
                        "american indian or alaskan native, non-hispanic",
                        "asian, non-hispanic",
                        "native hawaiian or other pacific islander, non-hispanic",
                        "other, non-hispanic","multiracial, non-hispanic")
      }
      ord <- intersect(race_order, unique(plotDf$Break_Out))
      if (length(ord) > 0) plotDf$Break_Out <- factor(plotDf$Break_Out, levels = ord, ordered = TRUE)
    }
  }
  
  # 10. Rename year column if needed
  if (!is.null(year_col_name) && year_col_name != "Year" && year_col_name %in% names(plotDf)) {
    plotDf <- plotDf %>% rename(Year = all_of(year_col_name))
  }
  
  plotDf
}

# -----------------------------
# UI
# -----------------------------
ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { font-family: Arial; background-color: #f7f7f7; }
    .panel-title { color: #0072b2; font-size: 26px; font-weight:bold; text-align:center; }
    .sidebar { background:white; padding:18px; border-radius:6px; }
    .main-content { background:white; padding:18px; border-radius:6px; }
    .ci-table { margin-top: 15px; font-size: 12px; }
  "))),
  titlePanel(div(class = "panel-title", "BRFSS Public Health Dashboard")),
  sidebarLayout(
    sidebarPanel(class = "sidebar", width = 3,
                 h4("1. Question Selection"),
                 uiOutput("class_ui"),
                 uiOutput("topic_ui"),
                 uiOutput("question_ui"),
                 hr(),
                 h4("2. View Settings"),
                 radioButtons("detail_level", "Demographic Detail Level:",
                              choices = c("Summary (Less)" = TRUE, "Detailed (More)" = FALSE),
                              selected = TRUE)
                 
    ),
    mainPanel(class = "main-content", width = 9,
              h3(textOutput("selected_question_title")),
              tabsetPanel(id = "main_tabs",
                          tabPanel("Overall", 
                                   plotOutput("overall_plot", height = "420px"),
                                   div(class = "ci-table", tableOutput("overall_ci_table"))),
                          tabPanel("By Gender", 
                                   plotOutput("gender_plot", height = "420px"),
                                   div(class = "ci-table", tableOutput("gender_ci_table"))),
                          tabPanel("By Age Group", 
                                   plotOutput("age_plot", height = "420px"),
                                   div(class = "ci-table", tableOutput("age_ci_table"))),
                          tabPanel("By Education", 
                                   plotOutput("education_plot", height = "420px"),
                                   div(class = "ci-table", tableOutput("education_ci_table"))),
                          tabPanel("By Income", 
                                   plotOutput("income_plot", height = "420px"),
                                   div(class = "ci-table", tableOutput("income_ci_table"))),
                          tabPanel("Temporal (Year)", 
                                   plotOutput("year_plot", height = "420px"),
                                   div(class = "ci-table", tableOutput("year_ci_table"))),
                          tabPanel("By Location",
                                   div(style="margin-top: 8px; margin-bottom: 8px;",
                                       radioButtons("loc_view_type", "Location View:",
                                                    choices = c("Highlight (Top/Bottom 5)" = "highlight",
                                                                "National Regions" = "regions",
                                                                "All States" = "all_states"),
                                                    selected = "highlight", inline = TRUE)
                                   ),
                                   plotOutput("location_plot", height = "520px"),
                                   div(class = "ci-table", tableOutput("location_ci_table"))
                          )
              )
    )
  )
)

# -----------------------------
# Server
# -----------------------------
server <- function(input, output, session) {
  
  # Load data
  data_list <- tryCatch(load_data("."), error = function(e) stop(e$message))
  df_raw <- data_list$df
  layerQ <- data_list$layerQ
  
  # Dynamic UI
  output$class_ui <- renderUI({ req(layerQ); selectInput("class_select", "Class", choices = unique(layerQ$Class)) })
  output$topic_ui <- renderUI({ req(input$class_select); selectInput("topic_select", "Topic", choices = layerQ %>% filter(Class == input$class_select) %>% pull(Topic) %>% unique()) })
  output$question_ui <- renderUI({ req(input$class_select, input$topic_select); selectInput("question_select", "Question", choices = layerQ %>% filter(Class == input$class_select, Topic == input$topic_select) %>% pull(Question) %>% unique()) })
  
  output$selected_question_title <- renderText({ req(input$question_select); paste("Analysis for:", str_trim(str_extract(input$question_select,"[^(]+"))) })
  
  filtered_q_data <- reactive({ req(input$question_select); df_raw %>% filter(Question == input$question_select) })
  
  apply_merge <- reactive({ as.logical(input$detail_level) })
  
  # Aggregated reactives - pass apply_merge() for categories that have detail levels
  overall_data <- reactive({ aggregate_data(filtered_q_data(), "CAT1", "Break_Out", apply_merges = FALSE) })
  gender_data  <- reactive({ aggregate_data(filtered_q_data(), "CAT2", "Break_Out", apply_merges = FALSE) }) # Gender has no merges
  age_data     <- reactive({ aggregate_data(filtered_q_data(), "CAT3", "Break_Out", apply_merges = apply_merge()) })
  education_data <- reactive({ aggregate_data(filtered_q_data(), "CAT5", "Break_Out", apply_merges = FALSE) }) # Education has no merges
  income_data  <- reactive({ aggregate_data(filtered_q_data(), "CAT6", "Break_Out", apply_merges = apply_merge()) })
  year_data    <- reactive({ aggregate_data(filtered_q_data(), "CAT1", "Year", apply_merges = FALSE) })
  location_data_raw <- reactive({ aggregate_data(filtered_q_data(), "CAT1", "Locationabbr", apply_merges = FALSE) })
  location_data_regions <- reactive({ aggregate_data(filtered_q_data(), "CAT1", "Locationabbr", apply_merges = FALSE, region_aggregate = TRUE) })
  
  # Base plotting function
  base_plot <- function(df, x_var_name, x_axis_title, is_dense = FALSE) {
    if (nrow(df) == 0 || all(is.na(df$agg_percent))) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No sufficient data.", size = 5) + theme_void())
    }
    x_sym <- sym(x_var_name)
    
    response_levels <- unique(df$Response) %>% as.character()
    
    # Consistent stacking order first
    if ("no" %in% response_levels && "yes" %in% response_levels) {
      order_vec <- c("no", response_levels[!response_levels %in% c("no","yes")], "yes")
    } else {
      order_vec <- response_levels
    }
    
    # Create colors AFTER determining order
    n_responses <- length(order_vec)
    pal <- RColorBrewer::brewer.pal(max(3, min(8, n_responses)), "Set2")
    colors <- setNames(pal[1:n_responses], order_vec)
    
    # Override specific colors
    if ("no" %in% order_vec) colors["no"] <- "#d9d9d9"
    if ("yes" %in% order_vec) colors["yes"] <- "#0072B2"
    
    # Apply factor levels
    df$Response <- factor(df$Response, levels = order_vec)
    
    ggplot(df, aes(x = !!x_sym, y = agg_percent, fill = Response)) +
      geom_col(position = position_stack(reverse = TRUE), color = "white", linewidth = 0.3, width = ifelse(is_dense, 0.95, 0.85)) +
      scale_y_continuous(labels = percent_format(scale = 1), limits = c(0,100)) +
      scale_fill_manual(values = colors) +
      labs(title = paste("Response Distribution"), x = x_axis_title, y = "Percent (%)") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "bottom",
        legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.x = if (is_dense) element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8) else element_text(angle = 0, hjust = 0.5, size = 10)
      )
  }
  
  # CI table function
  make_ci_table <- function(df, group_col = NULL) {
    if (nrow(df) == 0) return(NULL)
    
    if (!is.null(group_col) && group_col %in% names(df)) {
      df %>%
        select(all_of(group_col), Response, agg_percent, agg_low_ci_limit, agg_high_ci_limit) %>%
        mutate(
          agg_percent = round(agg_percent, 2),
          agg_low_ci_limit = round(agg_low_ci_limit, 2),
          agg_high_ci_limit = round(agg_high_ci_limit, 2)
        ) %>%
        rename(
          Group = all_of(group_col),
          `Response` = Response,
          `Percent (%)` = agg_percent,
          `CI Low` = agg_low_ci_limit,
          `CI High` = agg_high_ci_limit
        )
    } else {
      df %>%
        select(Response, agg_percent, agg_low_ci_limit, agg_high_ci_limit) %>%
        mutate(
          agg_percent = round(agg_percent, 2),
          agg_low_ci_limit = round(agg_low_ci_limit, 2),
          agg_high_ci_limit = round(agg_high_ci_limit, 2)
        ) %>%
        rename(
          `Response` = Response,
          `Percent (%)` = agg_percent,
          `CI Low` = agg_low_ci_limit,
          `CI High` = agg_high_ci_limit
        )
    }
  }
  
  # Render plots
  output$overall_plot <- renderPlot({
    df <- overall_data()
    if (nrow(df) > 0) df$Overall <- "National"
    base_plot(df, "Overall", "Overall")
  })
  output$overall_ci_table <- renderTable({ make_ci_table(overall_data()) })
  
  output$gender_plot <- renderPlot({ base_plot(gender_data(), "Break_Out", "Gender") })
  output$gender_ci_table <- renderTable({ make_ci_table(gender_data(), "Break_Out") })
  
  output$age_plot <- renderPlot({ base_plot(age_data(), "Break_Out", "Age Group") })
  output$age_ci_table <- renderTable({ make_ci_table(age_data(), "Break_Out") })
  
  output$education_plot <- renderPlot({ base_plot(education_data(), "Break_Out", "Education Level") })
  output$education_ci_table <- renderTable({ make_ci_table(education_data(), "Break_Out") })
  
  output$income_plot <- renderPlot({ base_plot(income_data(), "Break_Out", "Household Income") })
  output$income_ci_table <- renderTable({ make_ci_table(income_data(), "Break_Out") })
  
  output$year_plot <- renderPlot({ base_plot(year_data(), "Year", "Survey Year", is_dense = TRUE) })
  output$year_ci_table <- renderTable({ make_ci_table(year_data(), "Year") })
  
  output$location_plot <- renderPlot({
    req(input$loc_view_type)
    if (input$loc_view_type == "regions") {
      base_plot(location_data_regions(), "National_Region", "CDC National Region")
    } else {
      df <- location_data_raw()
      if (nrow(df) == 0) return(NULL)
      if (input$loc_view_type == "highlight") {
        primary_response <- df %>% count(Response, wt = agg_percent) %>% arrange(desc(n)) %>% slice(1) %>% pull(Response)
        rank_df <- df %>% filter(Response == primary_response) %>% arrange(desc(agg_percent))
        top_states <- head(rank_df$Locationabbr, 5)
        bot_states <- tail(rank_df$Locationabbr, 5)
        selected_states <- c(top_states, bot_states)
        df_filtered <- df %>% filter(Locationabbr %in% selected_states)
        df_filtered$Locationabbr <- factor(df_filtered$Locationabbr, levels = selected_states)
        base_plot(df_filtered, "Locationabbr", "State/Territory (Highs & Lows)", is_dense = FALSE)
      } else {
        base_plot(df, "Locationabbr", "State/Territory", is_dense = TRUE)
      }
    }
  })
  
  output$location_ci_table <- renderTable({
    req(input$loc_view_type)
    if (input$loc_view_type == "regions") {
      make_ci_table(location_data_regions(), "National_Region")
    } else {
      df <- location_data_raw()
      if (input$loc_view_type == "highlight" && nrow(df) > 0) {
        primary_response <- df %>% count(Response, wt = agg_percent) %>% arrange(desc(n)) %>% slice(1) %>% pull(Response)
        rank_df <- df %>% filter(Response == primary_response) %>% arrange(desc(agg_percent))
        top_states <- head(rank_df$Locationabbr, 5)
        bot_states <- tail(rank_df$Locationabbr, 5)
        selected_states <- c(top_states, bot_states)
        df <- df %>% filter(Locationabbr %in% selected_states)
      }
      make_ci_table(df, "Locationabbr")
    }
  })
}

# Run app
shinyApp(ui = ui, server = server)