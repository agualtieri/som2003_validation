## SOM2003 JMMI - Validation

## Rounds: Sep20

rm(list = ls())

today <- Sys.Date()

## library
library(tidyverse)
library(cleaninginspectoR)
library(openxlsx)
library(cluster)

## Source
source("./check_log.R")
source("./data_falsification.R")

## load inputs
df <- read.csv("./inputs/SOM2003_JMMI_Clean_Data_0821 v2.csv")

names(df)[names(df) == "_uuid"] <- "uuid"
names(df)[names(df) == "_index"] <- "index" ## renaming the uuid col


## check that all uuids in log are removed from df
del2 <- intersect(del$uuid, del$df) ## empty vector is good


## check issues
issues <- inspect_all(df) %>% filter(!str_detect(issue_type, "'other' response. may need recoding."))%>% 
                          mutate(uuid=df[.$index,"uuid",drop=TRUE], ngo=df[.$index,"enumerator_ong", drop=TRUE])

issues2 <- issues[!(issues$uuid %in% log$uuid),]

write.xlsx(issues, paste0("./outputs/cleaning_check_",today,".xlsx"))

## falsification




