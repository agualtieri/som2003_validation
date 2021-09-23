## dataset validation
rm(list=ls())

# library
library(tidyverse)
library(openxlsx)
library(cleaninginspectoR)
library(cluster)

source("./check_log.R")
source("./data_falsification.R")
source("./check_time.R")

# load files
tool <- read.xlsx("./inputs/jmmi_tool_2021may_v2.1.xlsx")
data <-"./inputs/SOM2003 JMMI Clean Data V1.2 VT 090621.xlsx"

sheets <- getSheetNames(data)
SheetList <- lapply(sheets, read.xlsx, xlsxFile = data)
names(SheetList) <- sheets


clean <- SheetList[[2]]
log <- SheetList[[3]]
del <- SheetList[[4]]

names(clean)[names(clean) == "X_uuid"] <-  "uuid"
names(clean)[names(clean) == "X_index"] <-  "index"
names(del)[names(del) == "X_uuid"] <- "uuid"

## Issues
issues <- inspect_all(clean) %>% filter(!is.na(index)) %>% mutate(uuid = clean[.$index,"uuid",drop=T]) %>% filter(uuid %in% log$uuid)
write.csv(issues, paste0("./outputs/outliers_",lubridate::today(),".csv"))

## Deletions
delc <- del %>% filter(uuid %in% clean$uuid)

## Falsification
similar.surveys <- calculateDifferences(clean, tool)
write.csv(similar.surveys, paste0("./outputs/similar surveys_",lubridate::today(),".csv"))

enumerator.similarity <- calculateEnumeratorSimilarity(clean, tool, "enumerator", "call_location")
x <- clean %>% filter(enumerator == "9971")
x.similar <- calculateDifferences(x, tool)

## Check cleaning log
log <- log %>% filter(question.name %in% names(clean))
log <- log %>% filter(uuid %in% clean$uuid)

log.c <- check_log(clean, log, old_log_var = "old_value", new_log_var = "new_value") %>% mutate(., check = ifelse(new_value == value_extracted, "Log applied correctly", "Please check log")) %>%
  filter(check == "Please check log")
write.csv(log.c, paste0("./outputs/log check_",lubridate::today(),".csv"))
