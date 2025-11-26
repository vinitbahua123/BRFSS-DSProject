# loads the *.csv into `df`,
# defines the functions,
# creates Question hierarchy in `layerQ`,

# functions defined below
merge_ResponseID <- function(char_vec) {
    char_vec <- str_replace(char_vec,"RESP025","RESP137")
    char_vec <- str_replace(char_vec,"RESP026","RESP172")
    char_vec <- str_replace(char_vec,"RESP029","RESP141")
    char_vec <- str_replace(char_vec,"RESP230","RESP020")
    char_vec <- str_replace(char_vec,"RESP231","RESP020")
    char_vec <- str_replace(char_vec,"RESP232","RESP020")
    char_vec <- str_replace(char_vec,"RESP196","RESP199")
    char_vec <- str_replace(char_vec,"RESP197","RESP199")
    char_vec <- str_replace(char_vec,"RESP198","RESP199")
    char_vec <- str_replace(char_vec,"RESP199","RESP199")
    char_vec <- str_replace(char_vec,"RESP200","RESP008")
    char_vec <- str_replace(char_vec,"RESP194","RESP005")
    char_vec <- str_replace(char_vec,"RESP195","RESP006")
    return(char_vec)
}

merge_Response <- function(ResponseID,Response) {
idx <- str_detect(ResponseID,"RESP137")
Response[idx] <- "Employed"
idx <- str_detect(ResponseID,"RESP172")
Response[idx] <- "Self-employed"
idx <- str_detect(ResponseID,"RESP141")
Response[idx] <- "Homemaker"
idx <- str_detect(ResponseID,"RESP020")
Response[idx] <- "$50,000+"
idx <- str_detect(ResponseID,"RESP199")
Response[idx] <- "A/A Native, Asian,Other"
idx <- str_detect(ResponseID,"RESP008")
Response[idx] <- "Multiracial"
idx <- str_detect(ResponseID,"RESP005")
Response[idx] <- "White"
idx <- str_detect(ResponseID,"RESP006")
Response[idx] <- "Black"
# make all responses lower case
Response <- tolower(Response)
return(Response)
}

merge_BreakoutID <- function(char_vec) {
    char_vec <- str_replace(char_vec,"INCOME01","INCOME1")
    char_vec <- str_replace(char_vec,"INCOME02","INCOME2")
    char_vec <- str_replace(char_vec,"INCOME03","INCOME3")
    char_vec <- str_replace(char_vec,"INCOME04","INCOME4")
    char_vec <- str_replace(char_vec,"INCOME05","INCOME5")
    char_vec <- str_replace(char_vec,"INCOME06","INCOME5")
    char_vec <- str_replace(char_vec,"INCOME07","INCOME5")
    char_vec <- str_replace(char_vec,"RACE01","RACE1")
    char_vec <- str_replace(char_vec,"RACE02","RACE2")
    char_vec <- str_replace(char_vec,"RACE08","RACE3")
    char_vec <- str_replace(char_vec,"RACE04","RACE4")
    char_vec <- str_replace(char_vec,"RACE05","RACE4")
    char_vec <- str_replace(char_vec,"RACE06","RACE4")
    char_vec <- str_replace(char_vec,"RACE03","RACE4")
    char_vec <- str_replace(char_vec,"RACE07","RACE5")
return(char_vec) 
}


merge_Break_Out <- function(BreakoutID,Break_Out) {
idx <- str_detect(BreakoutID,"INCOME5")
Break_Out[idx] <- "$50,000+"
idx <- str_detect(BreakoutID,"RACE1")
Break_Out[idx] <- "White"
idx <- str_detect(BreakoutID,"RACE2")
Break_Out[idx] <- "Black"
idx <- str_detect(BreakoutID,"RACE3")
Break_Out[idx] <- "Hispanic"
idx <- str_detect(BreakoutID,"RACE4")
Break_Out[idx] <- "A/A Native, Asian,Other"
idx <- str_detect(BreakoutID,"RACE5")
Break_Out[idx] <- "Multiracial"
return(Break_Out)
}


# load data


library(tidyverse)
library(utils)
options(dplyr.summarise.inform=FALSE)



fileList <- dir(recursive=TRUE)
fileName <- str_subset(fileList,"Prevalence.*[.]csv")
df <- read_csv(fileName,show_col_types=FALSE)


# creates question hierarchy in layerQ
layerQ <- df |>
    select(Class,Topic,Question) |>
    distinct() 
