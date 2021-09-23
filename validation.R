## SOM2003 JMMI - Validation

## Rounds: Sep20

rm(list = ls())

today <- Sys.Date()

## library
library(tidyverse)
library(cleaninginspectoR)
library(openxlsx)
library(cluster)

## Utilities
`%nin%` <- Negate(`%in%`)

## Source
source("./R/check_log.R")
source("./R/check_time.R")
source("./R/data_falsification.R")

## load inputs
df <- read.xlsx("./inputs/SOM2003_JMMI_Clean_Data_0821.xlsx", sheet = "data_checked")
log <- read.xlsx("./inputs/SOM2003_JMMI_Clean_Data_0821.xlsx", sheet = "data_log")
del <- read.xlsx("./inputs/SOM2003_JMMI_Clean_Data_0821.xlsx", sheet = "deletions")
tool <- read.xlsx("./inputs/jmmi_tool_2021_August.xlsx")

## Wrangling
names(df)[names(df) == "X_uuid"] <- "uuid"
names(df)[names(df) == "X_index"] <- "index"
df$index <- mutate(df, index = 1:nrow(df))

tool$n <- NULL

## check that all uuids in log are removed from df
del$X_uuid %in% df$uuid

## check log
## Check for uuid in log but not in data
uuid.not.in.data <- log %>% filter(uuid %nin% df$uuid)
uuid.not.in.data <- uuid.not.in.data %>% filter(uuid %in% del$X_uuid)

write.xlsx(uuid.not.in.data, "./outputs/uuid_not_in_dataset.xlsx")

## Check for questions in log but not in data
questions.not.in.data <- log %>% filter(Question %nin% names(df))

## Remove uuid and questions not in df from log
log <- log %>% filter(uuid %in% df$uuid)
log <- log %>% filter(Question %in% names(df))

log.i <- check_log(data = df, log = log, variable = "Question", old_log_var = "Old.Value", new_log_var = "New.Value", uuid_data = "uuid", uuid_log = "uuid") %>%
                    mutate(., check = ifelse(New.Value == value_extracted, "Log applied correctly", "Please check log")) %>%
                    filter(check == "Please check log")

write.xlsx(log.i, "./outputs/cleaning_log_not_applied.xlsx")

## check issues
issues <- inspect_all(df) %>% filter(!str_detect(issue_type, "'other' response. may need recoding."))%>% 
                          mutate(uuid=df[.$index,"uuid",drop=TRUE], ngo=df[.$index,"enumerator_ong", drop=TRUE])

issues <- issues %>% filter(uuid %nin% log$uuid)

write.xlsx(issues, paste0("./outputs/cleaning_check_",today,".xlsx"))

## falsification
false1 <- calculateEnumeratorSimilarity(df, tool, "enumerator_org", "call_location") ## nothing above 0.5
false2 <- calculateDifferences(data = df, tool.survey = tool) %>% filter(number.different.columns < 5) ## nothing lower than 5


