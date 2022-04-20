library(tidyverse)
library(wordcloud)
library(tidytext)

data <- tidytuesdayR::tt_load("2022-04-19")
#big_dave <- data$big_dave
times <- data$times
rm(data)



