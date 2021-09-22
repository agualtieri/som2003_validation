## compare analysis files
rm(list = ls())

## lib
library(tidyverse)
library(openxlsx)
library(arsenal)


## Load inputs
data_hq <-"./outputs/JMMI_2020NOV_analysed_all_2021-03-17.xlsx"
data_team <- "./outputs/JMMI_2021FEB_analysed_all_2021-03-15.xlsx"

# hq
sheets <- openxlsx::getSheetNames(data_hq)
SheetList <- lapply(sheets,openxlsx::read.xlsx,xlsxFile=data_hq)
names(SheetList) <- sheets


# team
sheets_t <- openxlsx::getSheetNames(data_team)
SheetList_t <- lapply(sheets,openxlsx::read.xlsx,xlsxFile=data_team)
names(SheetList_t) <- sheets_t

# compare df using arsenal package: https://cran.r-project.org/web/packages/arsenal/vignettes/comparedf.html
items_hq <- SheetList[[1]]
items_team <- SheetList_t[[1]]

comparedf(items_hq, items_team)
summary(comparedf(items_hq, items_team))

stock_hq <- SheetList[[2]]
stock_team <- SheetList_t[[2]]

summary(comparedf(stock_hq, stock_team))

bar_hq <- SheetList[[3]]
bar_team <- SheetList_t[[3]]

summary(comparedf(bar_hq, bar_team))

pay_hq <- SheetList[[4]]
pay_team <- SheetList_t[[4]]

summary(comparedf(pay_hq, pay_team))


sup1_hq <- SheetList[[5]]
sup1_team <- SheetList_t[[5]]

summary(comparedf(sup1_hq, sup1_team))


sup2_hq <- SheetList[[6]]
sup2_team <- SheetList_t[[6]]

summary(comparedf(sup2_hq, sup2_team))


cred1_hq <- SheetList[[7]]
cred1_team <- SheetList_t[[7]]

summary(comparedf(cred1_hq, cred1_team))


cred2_hq <- SheetList[[8]]
cred2_team <- SheetList_t[[8]]

summary(comparedf(cred2_hq, cred2_team))
