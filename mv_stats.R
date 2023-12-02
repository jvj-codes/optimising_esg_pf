### LOAD MV-RETURNS AND CALCULATE STATISTICS
library(RODBC)
library(tidyverse)
library(PerformanceAnalytics)
library(Rblpapi)
library(RCurl)
library(readxl)
library(rugarch)
library(tseries)
library(readxl)
library(grid)
library(ggplot2)
library(lubridate)
library(zoo)
library("writexl")
library(nloptr)
library(quadprog)
library(patchwork)
library(lpSolve)

mv_df_rtn <-read.csv("pct_chg_mv.csv",sep = ",")
colnames(mv_df_rtn)[1] <- "date" 
colnames(mv_df_rtn)[2] <- "pf_rtn" 
mv_df_rtn$date <- as.Date(mv_df_rtn$date, format = "%d/%m/%Y")


stats_opt3 <- perf_stats(mv_df_rtn, pct_chg_index, ff_data)
