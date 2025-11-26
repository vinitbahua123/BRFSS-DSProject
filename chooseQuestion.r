#prompts user for question
# stores question as *.RDS


# prompt user for class
option <- unique(layerQ$Class)
idx <- utils::menu(option,title="Which class?")
my_class <- option[idx]
slayerQ <- layerQ |>
    filter(Class==my_class)
# prompt user for topic
option <- unique(slayerQ$Topic)
idx <- utils::menu(option,title="Which topic?")
my_topic <- option[idx]
vslayerQ <- slayerQ |>
    filter(Topic==my_topic)
# prompt user for question
option <- unique(vslayerQ$Question)
idx <- utils::menu(option,title="Which question?")
my_q <- option[idx]

# write to RDS
saveRDS(my_q,file="my_q.RDS")

