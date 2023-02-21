
library(dplyr)
library(janitor)
library(tidyverse)

ors <- read.csv("/Users/brenna/Documents/Work/Bakian/occ ind/ORS dataset.csv")

names(ors) <- tolower(names(ors))

ors$estimate[ors$estimate == "-"] <- NA
ors$estimate <- as.numeric(ors$estimate)

ors <- subset(ors, select = c(soc.2018.code, occupation, #requirement,
                              #category, 
                              estimate.text, estimate))

ors_spread <- ors %>%
  group_by(soc.2018.code)  %>%
  filter(!grepl("Percentile", estimate.text)) %>% # only want means, not percentiles
  filter(!grepl("is not", estimate.text)) %>%
  filter(!grepl("are not", estimate.text)) %>%
  filter(!grepl("without", estimate.text)) %>% # removing inverse questions
  spread(estimate.text, estimate) %>%
  fill(1:6, .direction = c("downup")) %>%
  distinct() %>%
  clean_names() # for rf, cleanliness

# if missing, impute with 0
#     LOGIC: if people in an occupation weren't surveyed with a question, it wasn't relevant to them > 0
ors_spread[ , 1:ncol(ors_spread)][is.na(ors_spread[ , 1:ncol(ors_spread)])] <- 0

ors_spread$occupation <- as.factor(ors_spread$occupation)

write.csv(ors_spread, "data/ors_spread.csv", row.names = FALSE)

