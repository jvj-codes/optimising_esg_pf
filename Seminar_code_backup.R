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

######################## Connect to Bloomberg and extract prices for benchmark and portfolio
connect_bb <- blpConnect()
end_date <- as.Date("2023-07-31")
start_date <- as.Date(end_date-365*10)

index_memb <- bds("MXWD INDEX",c("INDX_MEMBERS"),con = connect_bb)
index_memb$Ticker <- paste(index_memb$`Member Ticker and Exchange Code`, "Equity", sep = " ")

info <- c("MSCI_ESG_RATING", "GICS_SECTOR_NAME", "CNTRY_OF_RISK")
esg_df <- bdp(index_memb$Ticker, info, con = connect_bb)
esg_df <- esg_df %>% rownames_to_column(var = "stock")
#prices <- bdh(index_memb$Ticker, "PX_LAST",options = c("currency"="USD"), start.date = start_date, end.date = end_date, con = connect_bb)
prices <- bdh(index_memb$Ticker, "PX_LAST",options = c("currency"="USD", "nonTradingDayFillMethod" = "PREVIOUS_VALUE"), start.date = start_date, end.date = end_date, con = connect_bb)
price_index <- bdh("MXWD INDEX", "PX_LAST",options = c("currency"="USD", "nonTradingDayFillMethod" = "PREVIOUS_VALUE"), start.date = start_date, end.date = end_date, con = connect_bb)

#read frama fench data
ff_data_load <-read.csv("Developed_5_Factors_Daily.csv",sep = ",")
colnames(ff_data_load)[1] <- "date" 
ff_data_load$date <- paste(
  substr(ff_data_load$date, 1, 4),   # Year
  substr(ff_data_load$date, 5, 6),   # Month
  substr(ff_data_load$date, 7, 8),   # Day
  sep = "-"
)

ff_data_load$date <- as.Date(ff_data_load$date, format = "%Y-%m-%d")
#rownames(ff_data) <- ff_data$date
#ff_data$date <- NULL  # Remove the "date" column
#ff_data[, -1] <- lapply(ff_data[, -1], as.numeric)

#### HANDLE PRICE DATA  
combined_df <- prices[[1]]
colnames(combined_df)[2] <- names(prices[1])

# Loop through the remaining dataframes and merge them by date
for (i in 2:length(prices)) {
  # Extract the dataframe for the current equity
  equity_df <- prices[[i]]
  colnames(equity_df)[2] <- names(prices[i])
  
  # Merge the dataframes by the "Date" column, using a full outer join
  combined_df <- merge(combined_df, equity_df, by = "date", all = TRUE)
}

filtered_df <- combined_df[!apply(combined_df[-1], 1, function(row) all(is.na(row))), ]

filled_df <- filtered_df %>%
  mutate(across(-date, ~ifelse(is.na(.), lag(., default = NA), .)))

for (i in 0:100) {
filled_df <- filled_df %>%
  mutate(across(-date, ~ifelse(is.na(.), lag(., default = NA), .)))
}


# Calculate percentage changes
calculate_pct_change <- function(x) {
  pct_change <- c(NA, diff(log(x)))
  return(pct_change)
}


# Apply the custom function to each column (except "date")
percentage_change_df <- filled_df %>%
  arrange(date) %>%
  mutate(across(-date, ~calculate_pct_change(.), .names = "{.col}"))

pct_chg_index  <- price_index%>%
  arrange(date) %>%
  mutate(across(-date, ~calculate_pct_change(.), .names = "{.col}"))
  
#Calculate annualized returns 
annualized_returns <- as.data.frame(Return.annualized(percentage_change_df, scale = 252 ))

#calculate annualized risk
annualized_risk <- as.data.frame(StdDev.annualized(percentage_change_df, scale = 252))

#calculate cumulative returns
cumulative_returns <- as.data.frame(Return.cumulative(percentage_change_df))


#making sure the date column is the correct format
percentage_change_df$date <- as.Date(percentage_change_df$date, format = "%Y-%m-%d")
pct_chg_index$date <- as.Date(pct_chg_index$date, format = "%Y-%m-%d")
ff_data_load$date <- as.Date(ff_data_load$date, format = "%Y-%m-%d")

matching_dates <- Reduce(intersect, list(ff_data_load$date, pct_chg_index$date, percentage_change_df$date))
matching_dates <- as.Date(matching_dates, origin = "1970-01-01")

beta_results_list <- list()
ann_rtn_results_list <- list()
ann_risk_results_list <- list()
cum_rtn_results_list <- list()
beta_results_list <- list()
te_results_list <- list()
sr_results_list <- list()
tr_results_list <- list()
ir_results_list <- list()

for (col_name in colnames(percentage_change_df)) {
  if (col_name != "date") {  # Skip the date column
    
    # Extract values without NA values
    stock_returns <- percentage_change_df[!is.na(percentage_change_df[, col_name]), c("date", col_name)]
    market_returns <- pct_chg_index[!is.na(pct_chg_index$PX_LAST), c("date", "PX_LAST")]
    rf_returns <- ff_data_load[!is.na(ff_data_load$RF), c("date", "RF")]
    
    stock_returns$date <- as.Date(stock_returns$date, format = "%Y-%m-%d")
    market_returns$date <- as.Date(market_returns$date, format = "%Y-%m-%d")
    rf_returns$date <- as.Date(rf_returns$date, format = "%Y-%m-%d")
    
    #convert risk free annualized returns to daily
    rf_returns$RF <- (1 + rf_returns$RF)^(1 / 252) - 1
    
    # Get the common date index for this stock, market, and risk-free data
    common_dates <- Reduce(intersect, list(stock_returns$date, market_returns$date, rf_returns$date))
    common_dates <- as.Date(common_dates, origin = "1970-01-01")
    
    matching_data <- stock_returns  %>%
      filter(date %in% common_dates) %>%
      left_join(market_returns %>% select(date, PX_LAST), by = "date") %>%
      left_join(rf_returns %>% select(date, RF), by = "date")
    
    # Rename columns for clarity
    colnames(matching_data) <- c("date", "stock", "market", "rf")
    
    matching_data$date <- as.Date(matching_data$date, origin = "1970-01-01")
    
    matching_data <- matching_data %>%
      mutate_all(~ifelse(is.na(.), 0, .))
    
    
    
    matching_data$date <- as.Date(matching_data$date)
    #matching_data$RF <- as.numeric(matching_data$RF)
    #stock_vector <- as.numeric(matching_data$stock)
    #market_vector <- as.numeric(matching_data$market)
    
    
    #initialize NA values for all calculations
    rf_mean <- NA
    stock_mean <- NA
    market_mean <- NA
    beta <- NA
    rf_annualized_rtn <- NA
    market_annualized_rtn <- NA
    stock_annualized_rtn <- NA
    rf_annualized_risk <- NA
    market_annualized_risk <- NA
    stock_annualized_risk <- NA
    te <- NA
    stock_cumu_rtn <- NA
    sr <- NA
    tr <- NA
    
    
    
    tryCatch({
      rf_mean <- mean(matching_data$rf, na.rm = TRUE)
    }, error = function(e) {
      cat("An error occurred while calculating rf_mean:", conditionMessage(e), "\n")
    })
    
    tryCatch({
      stock_mean <- mean(matching_data$stock, na.rm = TRUE)
    }, error = function(e) {
      cat("An error occurred while calculating stock_mean:", conditionMessage(e), "\n")
    })
    

    tryCatch({
      market_mean <- mean(matching_data$market, na.rm = TRUE)
    }, error = function(e) {
      cat("An error occurred while calculating market_mean:", conditionMessage(e), "\n")
    })
    
    tryCatch({
      beta <- sum(((matching_data$stock - matching_data$rf) - (stock_mean - rf_mean)) *
                    ((matching_data$market - matching_data$rf) - (market_mean - rf_mean))) /
        sum(((matching_data$market - matching_data$rf) - (market_mean - rf_mean))^2)
    }, error = function(e) {
      cat("An error occurred while calculating beta:", conditionMessage(e), "\n")
    })
    

    tryCatch({
      rf_annualized_rtn <- Return.annualized(matching_data$rf, scale = 252)
    }, error = function(e) {
      cat("An error occurred while calculating rf_annualized_rtn:", conditionMessage(e), "\n")
    })
    

    tryCatch({
      market_annualized_rtn <- Return.annualized(matching_data$market, scale = 252)
    }, error = function(e) {
      cat("An error occurred while calculating market_annualized_rtn:", conditionMessage(e), "\n")
    })
    
    tryCatch({
      stock_annualized_rtn <- Return.annualized(matching_data$stock, scale = 252)
    }, error = function(e) {
      cat("An error occurred while calculating stock_annualized_rtn:", conditionMessage(e), "\n")
    })
    
    
    tryCatch({
      rf_annualized_risk <- StdDev.annualized(matching_data$rf, scale = 252)
    }, error = function(e) {
      cat("An error occurred while calculating rf_annualized_risk:", conditionMessage(e), "\n")
    })
    
    tryCatch({
      market_annualized_risk <- StdDev.annualized(matching_data$market, scale = 252)
    }, error = function(e) {
      cat("An error occurred while calculating market_annualized_risk:", conditionMessage(e), "\n")
    })
    
    tryCatch({
      stock_annualized_risk <- StdDev.annualized(matching_data$stock, scale = 252)
    }, error = function(e) {
      cat("An error occurred while calculating stock_annualized_risk:", conditionMessage(e), "\n")
    })
    
    tryCatch({
      te <- StdDev.annualized((matching_data$stock - matching_data$market), scale = 252)
    }, error = function(e) {
      cat("An error occurred while calculating te:", conditionMessage(e), "\n")
    })
    
    tryCatch({
      stock_cumu_rtn <- Return.cumulative(matching_data$stock)
    }, error = function(e) {
      cat("An error occurred while calculating stock_cumu_rtn:", conditionMessage(e), "\n")
    })
    

    tryCatch({
      sr <- (stock_annualized_rtn - rf_annualized_rtn) / stock_annualized_risk
    }, error = function(e) {
      cat("An error occurred while calculating sr:", conditionMessage(e), "\n")
    })
    
  
    tryCatch({
      tr <- (stock_annualized_rtn - rf_annualized_rtn) / beta
    }, error = function(e) {
      cat("An error occurred while calculating tr:", conditionMessage(e), "\n")
    })
    
    tryCatch({
      ir <- (stock_annualized_rtn - market_annualized_rtn) / te
    }, error = function(e) {
      cat("An error occurred while calculating tr:", conditionMessage(e), "\n")
    })
    #add everything to a list
    ann_rtn_results_list[[col_name]] <- stock_annualized_rtn
    ann_risk_results_list[[col_name]] <- stock_annualized_risk
    cum_rtn_results_list[[col_name]] <- stock_cumu_rtn
    beta_results_list[[col_name]] <- beta
    te_results_list[[col_name]] <- te
    sr_results_list[[col_name]] <- sr
    tr_results_list[[col_name]] <- tr
    ir_results_list[[col_name]] <- ir
    
  }
}

# Convert the list of beta results into a data frame
results_df <- data.frame(stock = names(beta_results_list), ann_rtn = unlist(ann_rtn_results_list),
                              ann_risk = unlist(ann_risk_results_list), cum_rtn = unlist(cum_rtn_results_list),
                              beta = unlist(beta_results_list), te = unlist(te_results_list), sr = unlist(sr_results_list), 
                              tr = unlist(tr_results_list), ir = unlist(ir_results_list), stringsAsFactors = FALSE)

#merge esg dataframe and results df
results_addons_df <- results_df %>%
  left_join(esg_df, by = c("stock"))
  
#fill empty esg scores with N.S. = No SCORE
results_addons_df <- results_addons_df  %>%
  mutate(MSCI_ESG_RATING = ifelse(is.na(MSCI_ESG_RATING ) | MSCI_ESG_RATING  == "", "N.S.", MSCI_ESG_RATING))

#omit rows that contains na, possibly error in data provided
result_df_clean <- na.omit(results_addons_df)

######## FAMA FRENCH MODEL
ff_data <- ff_data_load %>%
  mutate(across(-date, ~ . / 100))

coefficients_df <- data.frame(stock = character(0), alpha = numeric(0)
                              , Mkt.RF = numeric(0), SMB = numeric(0), HML = numeric(0),
                              RMW = numeric(0), CMA = numeric(0))

for (col_name in colnames(percentage_change_df)) {
  if (col_name != "date") {  # Skip the date column
    # Extract values without NA values
    stock_returns <- percentage_change_df[!is.na(percentage_change_df[, col_name]), c("date", col_name)]
    stock_returns$date <- as.Date(stock_returns$date, format = "%Y-%m-%d")
    
    ff_data$date <- as.Date(ff_data$date, format = "%Y-%m-%d")
    
    # Get the common date index for this stock, market, and risk-free data
    common_dates <- Reduce(intersect, list(stock_returns$date, ff_data$date))
    common_dates <- as.Date(common_dates, origin = "1970-01-01")
    
    ff_rtn_df <- stock_returns  %>%
      filter(date %in% common_dates) %>%
      left_join(ff_data, by = "date") 
    
    if (nrow(ff_rtn_df) > 0) {
      ff_rtn_df <- ff_rtn_df %>%
        mutate(excess_rtn = .data[[col_name]] - RF)  # Create the excess_rtn column
      
      model <- lm(excess_rtn ~  Mkt.RF + SMB + HML + RMW + CMA, data = ff_rtn_df)
      
      coef <- coef(model)
      
      row <- data.frame(stock = col_name, alpha = coef[1], 
                        Mkt.RF = coef[2], SMB = coef[3], 
                        HML = coef[4], RMW = coef[5], CMA = coef[6])
      
      coefficients_df <- rbind(coefficients_df, row)
    }
  }  
}

result_df_clean <- result_df_clean %>%
  left_join(coefficients_df, by = c("stock"))

######## PORTFOLIOS CONSTRUCTON
beta_results_list <- list()
ann_rtn_results_list <- list()
ann_risk_results_list <- list()
cum_rtn_results_list <- list()
te_results_list <- list()
sr_results_list <- list()
tr_results_list <- list()
ir_results_list <- list()
n_results_list <- list()
alpha_results_list  <- list()
Mkt.RF_results_list  <- list()
SMB_results_list  <- list()
HML_results_list  <- list()
RMW_results_list  <- list()
CMA_results_list  <- list()

for (gics in unique(result_df_clean$GICS_SECTOR_NAME)) {
  gics <- as.character(gics)
  pf <- result_df_clean %>% 
    filter(GICS_SECTOR_NAME == gics, MSCI_ESG_RATING %in% c("AAA", "AA", "A", "BBB"))
  
  if (nrow(pf) > 0) {  # Check if pf has rows
    count_eq <- 1/nrow(pf)
    pf$weight <- count_eq
    #print(pf$weight)
  
    beta <- sum(pf$beta * pf$weight)
    ann_rtn <- sum(pf$ann_rtn * pf$weight)
    ann_risk <- sum(pf$ann_risk * pf$weight)
    cum_rtn <- sum(pf$cum_rtn * pf$weight)
    te <- sum(pf$te * pf$weight)
    sr <- sum(pf$sr * pf$weight)
    ir <- sum(pf$ir * pf$weight)
    tr <- sum(pf$tr * pf$weight)
    alpha <- sum(pf$alpha * pf$weight)
    Mkt.RF <- sum(pf$Mkt.RF * pf$weight)
    SMB <- sum(pf$SMB * pf$weight)
    HML <- sum(pf$HML * pf$weight)
    RMW <- sum(pf$RMW * pf$weight)
    CMA <- sum(pf$CMA * pf$weight)
    
    beta_results_list[[gics]] <- beta
    ann_rtn_results_list[[gics]] <- ann_rtn
    ann_risk_results_list[[gics]] <- ann_risk
    cum_rtn_results_list[[gics]] <- cum_rtn
    te_results_list[[gics]] <- te
    sr_results_list[[gics]] <- sr
    tr_results_list[[gics]] <- tr
    ir_results_list[[gics]] <- ir
    alpha_results_list[[gics]] <- alpha
    Mkt.RF_results_list[[gics]] <- Mkt.RF
    SMB_results_list[[gics]] <- SMB
    HML_results_list[[gics]] <- HML
    RMW_results_list[[gics]] <- RMW
    CMA_results_list[[gics]] <- CMA
    n_results_list[[gics]] <-nrow(pf)
  }
}

pf_df <- data.frame(pf = names(beta_results_list), ann_rtn = unlist(ann_rtn_results_list),
                    ann_risk = unlist(ann_risk_results_list), cum_rtn = unlist(cum_rtn_results_list),
                    beta = unlist(beta_results_list), te = unlist(te_results_list), sr = unlist(sr_results_list), 
                    tr = unlist(tr_results_list), ir = unlist(ir_results_list), N = unlist(n_results_list),
                    alpha = unlist(alpha_results_list), Mkt.RF = unlist(Mkt.RF_results_list),
                    SMB = unlist(SMB_results_list), HML = unlist(HML_results_list),
                    RMW = unlist(RMW_results_list), CMA = unlist(CMA_results_list), stringsAsFactors = FALSE)

######## PORTFOLIOS CONSTRUCTON NEW
beta_results_list <- list()
ann_rtn_results_list <- list()
ann_risk_results_list <- list()
cum_rtn_results_list <- list()
te_results_list <- list()
sr_results_list <- list()
tr_results_list <- list()
ir_results_list <- list()
n_results_list <- list()
alpha_results_list  <- list()
Mkt.RF_results_list  <- list()
SMB_results_list  <- list()
HML_results_list  <- list()
RMW_results_list  <- list()
CMA_results_list  <- list()
r2_list <- list()
pval_list <- list()

mean_list <- list()
std_list <- list()

#cumulative_plots <- list()

ff_data <- ff_data_load %>%
  mutate(across(-date, ~ . / 100))

#pf_rtn_df <- data.frame()


# Iterate through unique GICS sectors
for (gics in unique(result_df_clean$GICS_SECTOR_NAME)) {
  gics <- as.character(gics)
  
  # Filter the data frame based on sector and MSCI ESG rating
  pf <- result_df_clean %>% 
    filter(GICS_SECTOR_NAME == gics, MSCI_ESG_RATING %in% c("AAA", "AA", "A", "BBB"))
  
  pct_chg_sec_pf <- data.frame(date = as.Date(character(0)), rtn = numeric(0))
  
  if (nrow(pf) > 0) {  # Check if pf has rows
    stocks <- pf$stock
    filter_columns <- c("date", stocks)
    filter_df <- percentage_change_df[, filter_columns]
    
    # Iterate through the rows in the filtered data frame
    for (i in 3:nrow(filter_df)) {
      rows <- filter_df[i, ]
      
      cols_with_na <- which(apply(rows, 2, function(col) any(is.na(col))))
      non_na_rows <- rows[, -cols_with_na]
      if (ncol(non_na_rows) > 0) {
        wght <- 1 / (length(non_na_rows) - 1)  
        dly_rtn <- round(sum(non_na_rows[-1] * wght),8)
        row <- data.frame(date = rows[[1]], rtn = dly_rtn)
        pct_chg_sec_pf <- rbind(pct_chg_sec_pf, row)
    }
      
    }

  }
  
  
  # Extract values without NA values
  pf_returns <- pct_chg_sec_pf[!is.na(pct_chg_sec_pf$rtn), c("date", "rtn")]
  market_returns <- pct_chg_index[!is.na(pct_chg_index$PX_LAST), c("date", "PX_LAST")]
  
  pf_returns$date <- as.Date(pf_returns$date, format = "%Y-%m-%d")
  market_returns$date <- as.Date(market_returns$date, format = "%Y-%m-%d")
  ff_data$date <- as.Date(ff_data$date, format = "%Y-%m-%d")
  
  #convert risk free annualized returns to daily
  
  # Get the common date index for this stock, market, and risk-free data
  common_dates <- Reduce(intersect, list(ff_data$date, market_returns$date, pf_returns$date))
  common_dates <- as.Date(common_dates, origin = "1970-01-01")
  
  matching_data <- pf_returns  %>%
    filter(date %in% common_dates) %>%
    left_join(market_returns %>% select(date, PX_LAST), by = "date") %>%
    left_join(ff_data, by = "date")
  
  # Rename columns for clarity
  colnames(matching_data) <- c("date", "pf", "market", "mkt.rf", "smb", "hml", "rmw", "cma", "rf")
  
  matching_data$date <- as.Date(matching_data$date, origin = "1970-01-01")
  
  matching_data <- matching_data %>%
    mutate_all(~ifelse(is.na(.), 0, .))
  
  matching_data$date <- as.Date(matching_data$date)
  
  # Add daily return column to df
  #pf_rtn_df <- rbind(pf_rtn_df, matching_data$pf)
  #colnames(pf_rtn_df)[ncol(pf_rtn_df)] <- gics
  
  # Calculate statistics
  rf_mean <- mean(matching_data$rf, na.rm = TRUE)
  pf_mean <- mean(matching_data$pf, na.rm = TRUE)
  pf_std <- sd(matching_data$pf, na.rm = TRUE)
  
  market_mean <- mean(matching_data$market, na.rm = TRUE)
  
  rf_annualized_rtn <- Return.annualized(matching_data$rf, scale = 252)
  pf_annualized_rtn <- Return.annualized(matching_data$pf, scale = 252)
  market_annualized_rtn <- Return.annualized(matching_data$market, scale = 252)
  
  rf_annualized_risk <- StdDev.annualized(matching_data$rf, scale = 252)
  market_annualized_risk <- StdDev.annualized(matching_data$market, scale = 252)
  pf_annualized_risk <- StdDev.annualized(matching_data$pf, scale = 252)
  
  pf_cumu_rtn <- Return.cumulative(matching_data$pf)
  
  te <- StdDev.annualized((matching_data$pf - matching_data$market), scale = 252)
  beta <- beta <- sum(((matching_data$pf - matching_data$rf) - (pf_mean - rf_mean)) *
                        ((matching_data$market - matching_data$rf) - (market_mean - rf_mean))) /
    sum(((matching_data$market - matching_data$rf) - (market_mean - rf_mean))^2)
  market_annualized_rtn <- Return.annualized(matching_data$market, scale = 252)
  
  sr <- (pf_annualized_rtn - rf_annualized_rtn) / pf_annualized_risk
  tr <- (pf_annualized_rtn - rf_annualized_rtn) / beta
  ir <- (pf_annualized_rtn - market_annualized_rtn) / te
  
  ann_rtn <- pf_annualized_rtn
  ann_risk <- pf_annualized_risk
  cum_rtn <- pf_cumu_rtn
  
  
  matching_data <- matching_data %>%
    mutate(excess_rtn = pf - rf)  # Create the excess_rtn column
  
  model <- lm(excess_rtn ~  mkt.rf + smb + hml + rmw + cma, data = matching_data)
  
  coef <- coef(model)
  
  p_val <- summary(model)$coefficients[,4] 
  r2 <- summary(model)$r.squared
  
  alpha <- coef[1]
  Mkt.RF <- coef[2]
  SMB <- coef[3]
  HML <- coef[4]
  RMW <- coef[5]
  CMA <- coef[6]
  
  beta_results_list[[gics]] <- beta
  ann_rtn_results_list[[gics]] <- ann_rtn
  ann_risk_results_list[[gics]] <- ann_risk
  cum_rtn_results_list[[gics]] <- cum_rtn
  te_results_list[[gics]] <- te
  sr_results_list[[gics]] <- sr
  tr_results_list[[gics]] <- tr
  ir_results_list[[gics]] <- ir
  alpha_results_list[[gics]] <- alpha
  Mkt.RF_results_list[[gics]] <- Mkt.RF
  SMB_results_list[[gics]] <- SMB
  HML_results_list[[gics]] <- HML
  RMW_results_list[[gics]] <- RMW
  CMA_results_list[[gics]] <- CMA
  n_results_list[[gics]] <-nrow(pf)
  r2_list[[gics]] <- r2
  pval_list[[gics]] <- p_val
  mean_list[[gics]] <- pf_mean
  std_list[[gics]] <- pf_std
}

pf_df_new <- data.frame(pf = names(beta_results_list), ann_rtn = unlist(ann_rtn_results_list),
                    ann_risk = unlist(ann_risk_results_list), cum_rtn = unlist(cum_rtn_results_list),
                    beta = unlist(beta_results_list), te = unlist(te_results_list), sr = unlist(sr_results_list), 
                    tr = unlist(tr_results_list), ir = unlist(ir_results_list), N = unlist(n_results_list),
                    alpha = unlist(alpha_results_list), Mkt.RF = unlist(Mkt.RF_results_list),
                    SMB = unlist(SMB_results_list), HML = unlist(HML_results_list),
                    RMW = unlist(RMW_results_list), CMA = unlist(CMA_results_list), r2 = unlist(r2_list), stringsAsFactors = FALSE)
pf_df_new_stats <- data.frame(pf = names(mean_list), mean = unlist(mean_list)*100, std = unlist(std_list)*100, stringsAsFactors =  FALSE)


mean_acwi <- mean(pct_chg_index$PX_LAST, na.rm = TRUE)*100
std_acwi <- sd(pct_chg_index$PX_LAST, na.rm = TRUE)*100

acwi_row_stat <- c("ACWI", mean_acwi, std_acwi)
pf_df_new_stats <- rbind(pf_df_new_stats, acwi_row_stat)
pf_df_new_stats$pf <- gsub("Communication Services", "Comm. Serv.", pf_df_new_stats$pf)
pf_df_new_stats$pf <- gsub("Information Technology", "IT", pf_df_new_stats$pf)
pf_df_new_stats$pf <- gsub("Consumer Discretionary", "Cons. Discr.", pf_df_new_stats$pf)
pf_df_new_stats$pf <- gsub("Consumer Staples", "Cons. Staples", pf_df_new_stats$pf)
pf_df_new_stats$pf <- gsub("Real Estate", "Real Est.", pf_df_new_stats$pf)
pf_df_new_stats[, -which(names(pf_df_new_stats) == "pf")] <- apply(pf_df_new_stats[, -which(names(pf_df_new_stats) == "pf")], 2, as.numeric)

pf_df_new_stats<-pf_df_new_stats %>% mutate(across(where(is.numeric), ~ round(., 2)))

##########################################
### add perf stats for acwi aswell
market_returns <- pct_chg_index[!is.na(pct_chg_index$PX_LAST), c("date", "PX_LAST")]
market_ann_rtn <- Return.annualized(market_returns$PX_LAST, scale = 252)
market_ann_risk <- StdDev.annualized(market_returns$PX_LAST, scale = 252)
market_cum_rtn <- Return.cumulative(market_returns$PX_LAST)

ff_data$date <- as.Date(ff_data$date, format = "%Y-%m-%d")

# Get the common date index for this stock, market, and risk-free data
common_dates <- Reduce(intersect, list(market_returns$date, ff_data$date))
common_dates <- as.Date(common_dates, origin = "1970-01-01")

ff_rtn_df <- market_returns  %>%
  filter(date %in% common_dates) %>%
  left_join(ff_data, by = "date") 

if (nrow(ff_rtn_df) > 0) {
  ff_rtn_df <- ff_rtn_df %>%
    mutate(excess_rtn = .data[["PX_LAST"]] - RF)  # Create the excess_rtn column
  
  model <- lm(excess_rtn ~  Mkt.RF + SMB + HML + RMW + CMA, data = ff_rtn_df)
  
  p_val <- summary(model)$coefficients[,4] 
  r2 <- summary(model)$r.squared
  
  coef_bm <- coef(model)
  alpha_bm <- coef_bm[1]
  Mkt.RF_bm <-coef_bm[2]
  SMB_bm <- coef_bm[3]
  HML_bm<- coef_bm[4]
  RMW_bm <- coef_bm[5]
  CMA_bm <- coef_bm[6]
}

acwi_row <- c("ACWI", market_ann_rtn, market_ann_risk, market_cum_rtn, 1,
              NA, NA, NA, NA, 2500, alpha_bm,
              Mkt.RF_bm, SMB_bm, HML_bm, RMW_bm, CMA_bm, r2)

# Add the "ACWI" row to pf_df
pf_df_new <- rbind(pf_df_new , acwi_row)

# Update the row names
new_row_names <- 1:nrow(pf_df)
row.names(pf_df) <- new_row_names

new_row_names <- 1:nrow(pf_df_new)
row.names(pf_df_new) <- new_row_names

msci_filt_df <- result_df_clean %>% 
  filter(MSCI_ESG_RATING %in% c("AAA", "AA", "A", "BBB"))


#### Retrieve expected returns, covariance matrix of avaliable ESG stocks
stock_returns_esg <- percentage_change_df %>%
  select(date, msci_filt_df$stock)

reduce_universe <- stock_returns_esg[, colSums(!is.na(stock_returns_esg)) >= 3000]

red_uni <- na.omit(reduce_universe)

matrix_rtns <- as.matrix(red_uni[,-1])

cov_mat <- var(matrix_rtns*1000)

exp_rtn <- colMeans(red_uni[,-1]*1000)

names <- names(red_uni[,-1])

msci_filt_df_new <- msci_filt_df %>%
  filter(stock %in% names)



min_w_sec <- 0.00
max_w_sec <- 0.20

min_w_cntry <- 0.00
max_w_cntry <- 0.1

min_w_stck <- 0.00
max_w_stck <- 0.05

# sector constraints
number_of_sectors <- length(unique(msci_filt_df_new$GICS_SECTOR_NAME))
sector <- data.frame(Constraints = "GICS_SECTOR_NAME", Weight = 0.00, Min = min_w_sec, Max = max_w_sec)

for (name in unique(msci_filt_df_new$GICS_SECTOR_NAME)) {
  w <- 0 
  minw <- min_w_sec
  maxw <- max_w_sec
  sector <- rbind(sector, data.frame(Constraints = name, Weight = w, Min = minw, Max = maxw))
}

# country Constraints
number_of_cntry <- length(unique(msci_filt_df_new$CNTRY_OF_RISK))
country <- data.frame(Constraints = "CNTRY_OF_RISK", Weight = 0.00, Min = min_w_cntry, Max = max_w_cntry)

for (name in unique(msci_filt_df_new$CNTRY_OF_RISK)) {
  w <- 0
  minw <- min_w_cntry
  maxw <- max_w_cntry
  country <- rbind(country, data.frame(Constraints = name, Weight = w, Min = minw, Max = maxw))
}

# individual stock
stock <- data.frame(Constraints = "stock", Weight = 0.00, Min = min_w_stck, Max = max_w_stck)

for (name in unique(msci_filt_df_new$stock)) {
  w <- 0 
  minw <- min_w_stck
  maxw <- max_w_stck
  stock <- rbind(stock, data.frame(Constraints = name, Weight = w, Min = minw, Max = maxw))
}


# Combine Constraints
#constraints <- list(country, sector, stock)
constraints <- list(stock)

constraints_list <- list()
for (arr in constraints) {
  head <- arr[1, 1]
  names <- arr[-1, 1]
  min_w <- as.numeric(arr[-1, 3])
  max_w <- as.numeric(arr[-1, 4])
  constraints_list[[head]] <- list(names, min_w, max_w)
}

N <- nrow(msci_filt_df_new)
R <- 2 * sum(sapply(constraints_list, function(vals) length(vals[[1]]))) # Total number of inequality constraints

A_ub <- matrix(0, nrow = R, ncol = N)
b_ub <- numeric(R)
r <- 0

for (head in names(constraints_list)) {
  vals <- constraints_list[[head]]
  names <- vals[[1]]
  min_w <- vals[[2]]
  max_w <- vals[[3]]
  #print(vals)
  # Indicator matrix; mat_ij = 1 if i'th stock is of type j; else 0
  mat <- t(sapply(names, function(name) as.numeric(msci_filt_df_new[[head]] == name)))
  l <- nrow(mat)
  
  # Upper bounds for the k'th constraint
  A_ub[(r + 1):(r + l),] <- -mat
  b_ub[(r + 1):(r + l)] <- -max_w

  A_ub[(r + l + 1):(r + 2 * l), ] <- mat
  b_ub[(r + l + 1):(r + 2 * l)] <- min_w
  
  r <- r + 2 * l  # Increment by 2 * l 
  print(r)
}

Sigma <- cov_mat
lambda <- 1  # Adjust this based on your risk-return preferences

# Define the constraint matrices and vectors
A <- A_ub  # Extract the inequality constraints (sector, country, stock constraints)
b <- b_ub    # Extract the right-hand side of the inequality constraints

Aeq <- matrix(1, nrow = 1, ncol = ncol(A))  # 1's for the equality constraint
beq <- 1  # Sum of portfolio weights must equal 1

#beat market
amkt <- t(exp_rtn)
bmkt <- 0.24
# Concatenate the row of ones to the constraint matrix A
A <- rbind(Aeq, amkt, A)

# Concatenate the right-hand side value to the constraint vector b
b <- c(beq, bmkt, b)

# Define the objective function to maximize expected return
Dmat <- 2 * lambda * Sigma
dvec <- numeric(nrow(Dmat))

# Solve the quadratic programming problem using quadprog
result <- solve.QP(Dmat = Dmat, dvec = dvec, Amat = t(A), bvec = b, meq = 1, factorized = FALSE)

# Portfolio weights (solution)
optimal_weights  <- result$solution

msci_filt_df_new$w_opt_1 <- optimal_weights 

filtered_stocks <- msci_filt_df_new %>%
  filter(w_opt_1 > 0) %>%
  select(stock, w_opt_1)

filtered_return <- percentage_change_df %>%
  select(date, filtered_stocks$stock)

filtered_return$pf_rtn <- rowSums(filtered_return[,-1] * filtered_stocks$w_opt_1, na.rm = TRUE)

stats_opt1 <- perf_stats(filtered_return, filtered_stocks$w_opt_1, pct_chg_index, ff_data)

test1 <- msci_filt_df_new %>%
  group_by(GICS_SECTOR_NAME) %>%
  summarise(Total_Weight = sum(w_opt_1))

test2 <- msci_filt_df_new %>%
  group_by(CNTRY_OF_RISK) %>%
  summarise(Total_Weight = sum(w_opt_1))


####
ranking <- function(data, input_col, sign = "plus") {
  
  ranking_list <- list()
  
  if (sign == "plus") { 
    max_val <- max(data[[input_col]])
    min_val <- min(data[[input_col]])
    
    for (n in 1:nrow(data)) {
      stock <- data[["stock"]][n]
      input_value <- data[[input_col]][n] 
      rank <- (input_value - min_val) / (max_val - min_val)
      ranking_list[[stock]] <- rank * 100
    }
  } else if (sign == "minus") {
    max_val <- -min(data[[input_col]])
    min_val <- -max(data[[input_col]])
    
    for (n in 1:nrow(data)) {
      stock <- data[["stock"]][n]
      input_value <- -data[[input_col]][n] 
      rank <- (input_value - min_val) / (max_val - min_val)
      ranking_list[[stock]] <- rank * 100
    }
  }
  
  return(ranking_list)
}

get_perf_stats <- function(weights) {
    #weights need to be an array
    #assuming that we already have a dataframe of market returns
    #and a dataframe of stock returns
    
    #calculate portfolio performance statistics
    
    ann_rtn <- sum(weights*msci_filt_df$ann_rtn)
    ann_risk <- sum(weights*msci_filt_df$ann_risk)
    cum_rtn <- sum(weights*msci_filt_df$cum_rtn)
  
    ir <- sum(weights*msci_filt_df$ir)
    te <- sum(weights*msci_filt_df$te)
    sr <- sum(weights*msci_filt_df$sr)
    beta <- sum(weights*msci_filt_df$beta)
    tr <- sum(weights*msci_filt_df$tr)
    
    alpha <- sum(msci_filt_df$alpha * weights)
    Mkt.RF <- sum(msci_filt_df$Mkt.RF * weights)
    SMB <- sum(msci_filt_df$SMB * weights)
    HML <- sum(msci_filt_df$HML * weights)
    RMW <- sum(msci_filt_df$RMW * weights)
    CMA <- sum(msci_filt_df$CMA * weights)
    
    N <- length(weights[weights>0])
    
    stats_df <- data.frame(ann_rtn, ann_risk, 
                           cum_rtn, beta, te, 
                           sr, tr, ir,N,
                           alpha, Mkt.RF, SMB, HML, RMW, CMA)
    
    return(stats_df)
}

perf_stats <- function(pf_rtns,pct_chg_index, ff_data) {
  pf_returns <- pf_rtns[!is.na(pf_rtns[, "pf_rtn"]), c("date", "pf_rtn")]
  market_returns <- pct_chg_index[!is.na(pct_chg_index$PX_LAST), c("date", "PX_LAST")]
  rf_returns <- ff_data[!is.na(ff_data$RF), c("date", "RF")]
  
  pf_returns$date <- as.Date(pf_returns$date, format = "%Y-%m-%d")
  market_returns$date <- as.Date(market_returns$date, format = "%Y-%m-%d")
  rf_returns$date <- as.Date(rf_returns$date, format = "%Y-%m-%d")
  
  # Get the common date index for this stock, market, and risk-free data
  common_dates <- Reduce(intersect, list(pf_returns$date, market_returns$date, rf_returns$date))
  common_dates <- as.Date(common_dates, origin = "1970-01-01")
  
  matching_data <- pf_returns  %>%
    filter(date %in% common_dates) %>%
    left_join(market_returns %>% select(date, PX_LAST), by = "date") %>%
    left_join(ff_data, by = "date")
  
  # Rename columns for clarity
  colnames(matching_data) <- c("date", "pf", "market", "mkt.rf", "smb", "hml", "rmw", "cma", "rf")
  
  matching_data$date <- as.Date(matching_data$date, origin = "1970-01-01")
  
  matching_data <- matching_data %>%
    mutate_all(~ifelse(is.na(.), 0, .))
  
  
  
  matching_data$date <- as.Date(matching_data$date)
  #matching_data$RF <- as.numeric(matching_data$RF)
  #stock_vector <- as.numeric(matching_data$stock)
  #market_vector <- as.numeric(matching_data$market)
  
  
  #initialize NA values for all calculations
  rf_mean <- NA
  pf_mean <- NA
  market_mean <- NA
  beta <- NA
  rf_annualized_rtn <- NA
  market_annualized_rtn <- NA
  pf_annualized_rtn <- NA
  rf_annualized_risk <- NA
  market_annualized_risk <- NA
  stock_annualized_risk <- NA
  te <- NA
  pf_cumu_rtn <- NA
  sr <- NA
  tr <- NA
  
  
  tryCatch({
    rf_mean <- mean(matching_data$rf, na.rm = TRUE)
  }, error = function(e) {
    cat("An error occurred while calculating rf_mean:", conditionMessage(e), "\n")
  })
  
  tryCatch({
    pf_mean <- mean(matching_data$pf, na.rm = TRUE)
  }, error = function(e) {
    cat("An error occurred while calculating pf_mean:", conditionMessage(e), "\n")
  })
  
  
  tryCatch({
    market_mean <- mean(matching_data$market, na.rm = TRUE)
  }, error = function(e) {
    cat("An error occurred while calculating market_mean:", conditionMessage(e), "\n")
  })
  
  tryCatch({
    beta <- sum(((matching_data$pf - matching_data$rf) - (pf_mean - rf_mean)) *
                  ((matching_data$market - matching_data$rf) - (market_mean - rf_mean))) /
      sum(((matching_data$market - matching_data$rf) - (market_mean - rf_mean))^2)
  }, error = function(e) {
    cat("An error occurred while calculating beta:", conditionMessage(e), "\n")
  })
  
  
  tryCatch({
    rf_annualized_rtn <- Return.annualized(matching_data$rf, scale = 252)
  }, error = function(e) {
    cat("An error occurred while calculating rf_annualized_rtn:", conditionMessage(e), "\n")
  })
  
  
  tryCatch({
    market_annualized_rtn <- Return.annualized(matching_data$market, scale = 252)
  }, error = function(e) {
    cat("An error occurred while calculating market_annualized_rtn:", conditionMessage(e), "\n")
  })
  
  tryCatch({
    pf_annualized_rtn <- Return.annualized(matching_data$pf, scale = 252)
  }, error = function(e) {
    cat("An error occurred while calculating pf_annualized_rtn:", conditionMessage(e), "\n")
  })
  
  
  tryCatch({
    rf_annualized_risk <- StdDev.annualized(matching_data$rf, scale = 252)
  }, error = function(e) {
    cat("An error occurred while calculating rf_annualized_risk:", conditionMessage(e), "\n")
  })
  
  tryCatch({
    market_annualized_risk <- StdDev.annualized(matching_data$market, scale = 252)
  }, error = function(e) {
    cat("An error occurred while calculating market_annualized_risk:", conditionMessage(e), "\n")
  })
  
  tryCatch({
    pf_annualized_risk <- StdDev.annualized(matching_data$pf, scale = 252)
  }, error = function(e) {
    cat("An error occurred while calculating pf_annualized_risk:", conditionMessage(e), "\n")
  })
  
  tryCatch({
    te <- StdDev.annualized((matching_data$pf - matching_data$market), scale = 252)
  }, error = function(e) {
    cat("An error occurred while calculating te:", conditionMessage(e), "\n")
  })
  
  tryCatch({
    pf_cumu_rtn <- Return.cumulative(matching_data$pf)
  }, error = function(e) {
    cat("An error occurred while calculating pf_cumu_rtn:", conditionMessage(e), "\n")
  })
  
  
  tryCatch({
    sr <- (pf_annualized_rtn - rf_annualized_rtn) / pf_annualized_risk
  }, error = function(e) {
    cat("An error occurred while calculating sr:", conditionMessage(e), "\n")
  })
  
  
  tryCatch({
    tr <- (pf_annualized_rtn - rf_annualized_rtn) / beta
  }, error = function(e) {
    cat("An error occurred while calculating tr:", conditionMessage(e), "\n")
  })
  
  tryCatch({
    ir <- (pf_annualized_rtn - market_annualized_rtn) / te
  }, error = function(e) {
    cat("An error occurred while calculating tr:", conditionMessage(e), "\n")
  })

  
  ann_rtn <- pf_annualized_rtn
  ann_risk <- pf_annualized_risk
  cum_rtn <- pf_cumu_rtn
  
  matching_data <- matching_data %>%
    mutate(excess_rtn = pf - rf)  # Create the excess_rtn column
  
  model <- lm(excess_rtn ~  mkt.rf + smb + hml + rmw + cma, data = matching_data)
  
  coef <- coef(model)
  
  p_val <- summary(model)$coefficients[,4] 
  r2 <- summary(model)$r.squared
  
  alpha <- coef[1]
  Mkt.RF <- coef[2]
  SMB <- coef[3]
  HML <- coef[4]
  RMW <- coef[5]
  CMA <- coef[6]
  
  #N <- length(weights[weights>0])
  
  stats_df <- data.frame(ann_rtn, ann_risk, 
                         cum_rtn, beta, te, 
                         sr, tr, ir,N,
                         alpha, Mkt.RF, SMB, HML, RMW, CMA, r2)
  print(p_val)
  return(stats_df)
}

### RANK FACTOR RISKS
msci_filt_df$non_factor <- as.numeric(ranking(msci_filt_df,"alpha", "plus"))
msci_filt_df$market <- as.numeric(ranking(msci_filt_df,"Mkt.RF", "plus"))
msci_filt_df$size <- as.numeric(ranking(msci_filt_df,"SMB", "minus"))
msci_filt_df$value <- as.numeric(ranking(msci_filt_df,"HML", "plus"))
msci_filt_df$quality <- as.numeric(ranking(msci_filt_df,"RMW", "plus"))
msci_filt_df$investment <- as.numeric(ranking(msci_filt_df,"CMA", "plus"))
### Optimize portfolio,
## Portfolio 1 - Single constraint on individual stocks
#objective vector to maximize
#obj <- msci_filt_df$ann_rtn
obj <- msci_filt_df$non_factor

#set the number of constraints
num_assets <- nrow(msci_filt_df)
num_constraints <- num_assets + 1

#set the constraint matrix
const_mat <- matrix(0, nrow = num_constraints, ncol = num_assets)
for (i in 1:num_assets) {
  const_mat[i, i] <- 1
}
const_mat[num_constraints, ] <- 1

#set the constraint types (<= for individual weights and == for the sum of weights)
const_types <- c(rep("<=", num_assets), "==")

#set the right-hand side values for the constraints (0.05 for individual weights and 1 for the sum of weights)
const_rhs <- c(rep(0.03, num_assets), 1)

lp_solution <- lp("max", obj, const_mat, const_types, const_rhs)

optimal_weights <- lp_solution$solution

#get the daily returns for this portfolio

cat("Sum of Weights:", sum(optimal_weights), "\n")

#stats_opt1 <- get_perf_stats(optimal_weights)

#initial opt esg perf, look into weights in groupings
msci_filt_df$w_opt_1 <- optimal_weights 

filtered_stocks <- msci_filt_df %>%
  filter(w_opt_1 > 0) %>%
  select(stock, w_opt_1)

filtered_return <- percentage_change_df %>%
  select(date, filtered_stocks$stock)

filtered_return$pf_rtn <- rowSums(filtered_return[,-1] * filtered_stocks$w_opt_1, na.rm = TRUE)

stats_opt1 <- perf_stats(filtered_return, filtered_stocks$w_opt_1, pct_chg_index, ff_data)

test1 <- msci_filt_df %>%
  group_by(GICS_SECTOR_NAME) %>%
  summarise(Total_Weight = sum(w_opt_1))

test2 <- msci_filt_df %>%
  group_by(CNTRY_OF_RISK) %>%
  summarise(Total_Weight = sum(w_opt_1))

## Portfolio 2 - Constraint sector, country and individual stock + portfolio fully invested
obj <- msci_filt_df$alpha
pf <- "alpha"

min_w_sec <- 0
max_w_sec <- 0.1

min_w_cntry <- 0
max_w_cntry <- 0.05

min_w_stck <- 0
max_w_stck <- 0.03

# sector constraints
number_of_sectors <- length(unique(msci_filt_df$GICS_SECTOR_NAME))
sector <- data.frame(Constraints = "GICS_SECTOR_NAME", Weight = 0.00, Min = min_w_sec, Max = max_w_sec)

for (name in unique(msci_filt_df$GICS_SECTOR_NAME)) {
  w <- 0 
  minw <- min_w_sec
  maxw <- max_w_sec
  sector <- rbind(sector, data.frame(Constraints = name, Weight = w, Min = minw, Max = maxw))
}

# country Constraints
number_of_cntry <- length(unique(msci_filt_df$CNTRY_OF_RISK))
country <- data.frame(Constraints = "CNTRY_OF_RISK", Weight = 0.00, Min = min_w_cntry, Max = max_w_cntry)

for (name in unique(msci_filt_df$CNTRY_OF_RISK)) {
  w <- 0
  minw <- min_w_cntry
  maxw <- max_w_cntry
  country <- rbind(country, data.frame(Constraints = name, Weight = w, Min = minw, Max = maxw))
}

# individual stock
stock <- data.frame(Constraints = "stock", Weight = 0.00, Min = min_w_stck, Max = max_w_stck)

for (name in unique(msci_filt_df$stock)) {
  w <- 0 
  minw <- min_w_stck
  maxw <- max_w_stck
  stock <- rbind(stock, data.frame(Constraints = name, Weight = w, Min = minw, Max = maxw))
}


# Combine Constraints
constraints <- list(country, sector, stock)

constraints_list <- list()
for (arr in constraints) {
  head <- arr[1, 1]
  names <- arr[-1, 1]
  min_w <- as.numeric(arr[-1, 3])
  max_w <- as.numeric(arr[-1, 4])
  constraints_list[[head]] <- list(names, min_w, max_w)
  #issubset <- all(msci_filt_df[[head]] %in% names)
  #diff <- setdiff(names, msci_filt_df[[head]])
}

N <- nrow(msci_filt_df)
R <- 2 * sum(sapply(constraints_list, function(vals) length(vals[[1]]))) # Total number of inequality constraints

A_ub <- matrix(0, nrow = R + 1, ncol = N)
b_ub <- numeric(R + 1)
r <- 0

for (head in names(constraints_list)) {
  vals <- constraints_list[[head]]
  names <- vals[[1]]
  min_w <- vals[[2]]
  max_w <- vals[[3]]
  #print(vals)
  # Indicator matrix; mat_ij = 1 if i'th stock is of type j; else 0
  mat <- t(sapply(names, function(name) as.numeric(msci_filt_df[[head]] == name)))
  l <- nrow(mat)
  
  # Upper bounds for the k'th constraint
  A_ub[(r + 1):(r + l),] <- mat
  b_ub[(r + 1):(r + l)] <- max_w
  
  # Lower bounds for the k'th constraint
  A_ub[(r + l + 1):(r + 2 * l), ] <- -mat
  b_ub[(r + l + 1):(r + 2 * l)] <- -min_w
  
  r <- r + 2 * l  # Increment by 2 * l 
  print(r)
}

A_ub[R + 1, ] <- 1  # This enforces that the sum of weights equals 1
b_ub[R + 1] <- 1    # This specifies the RHS of the full investment constraint

dir <- c(rep("<=", R), "==")
# Create the linear programming model
lp_model <- lp("max",obj, const.mat = A_ub, const.dir = dir, const.rhs = b_ub)


# Check if the optimization was successful
if (lp_model$status == 0) {
  # Get the optimized portfolio weights
  optimized_weights <- lp_model$solution
  print("Optimization successful!")
  #print(optimized_weights)
} else {
  print("Optimization failed.")
}

optimal_weights2 <- lp_model$solution

cat("Sum of Weights:", sum(optimal_weights2), "\n")

msci_filt_df$w_opt_2 <- optimal_weights2 

filtered_stocks <- msci_filt_df %>%
  filter(w_opt_2 > 0) %>%
  select(stock, w_opt_2)

filtered_return <- percentage_change_df %>%
  select(date, filtered_stocks$stock)

filtered_return$pf_rtn <- rowSums(filtered_return[,-1] * filtered_stocks$w_opt_2, na.rm = TRUE)

stats_opt2 <- perf_stats(filtered_return, filtered_stocks$w_opt_2, pct_chg_index, ff_data)

test3 <- msci_filt_df %>%
  group_by(GICS_SECTOR_NAME) %>%
  summarise(Total_Weight = sum(w_opt_2))

test4 <- msci_filt_df %>%
  group_by(CNTRY_OF_RISK) %>%
  summarise(Total_Weight = sum(w_opt_2))

###
# add to list
pf_list[[pf]] <- stats_opt2

factor_pf_df <- do.call(rbind, lapply(names(pf_list), function(factor_name) {
  data <- as.data.frame(pf_list[[factor_name]])
  data$pf <- factor_name
  data
}))

# Reset row names
row.names(factor_pf_df) <- NULL



# Reorder columns
factor_pf_df <- factor_pf_df[, c("pf", "ann_rtn", "ann_risk", "cum_rtn", "beta", "te", "sr", "tr", "ir", "N", "alpha",
                   "Mkt.RF", "SMB", "HML", "RMW", "CMA")]

###Group things
pf_list <- list()


sector_ow <- msci_filt_df_new %>%
  group_by(GICS_SECTOR_NAME) %>%
  summarise(PF1 = sum(w_opt_1)*100, PF2 = sum(w_opt_2)*100)

country_ow <- msci_filt_df_new %>%
  group_by(CNTRY_OF_RISK) %>%
  summarise(PF1 = sum(w_opt_1)*100, PF2 = sum(w_opt_2)*100)

# Step 1: Create new portfolios for each sector with equal weights
sector_portfolios <- msci_filt_df %>%
  group_by(GICS_SECTOR_NAME) %>%
  mutate(Equal_Weight = 1 / n()) %>%
  ungroup()

# Calculate the total exposure weight to the country for each unique GICS_SECTOR_NAME
country_ow_sec <- sector_portfolios %>%
  group_by(GICS_SECTOR_NAME, CNTRY_OF_RISK) %>%
  summarise(Total_Weight = sum(Equal_Weight), .groups = "drop")

# Pivot the data to have columns for each GICS_SECTOR_NAME
country_ow_pivot <- country_ow_sec %>%
  pivot_wider(names_from = GICS_SECTOR_NAME, values_from = Total_Weight, values_fill = 0)

country_ow_pivot <- country_ow_pivot %>%
  left_join(country_ow, by = c("CNTRY_OF_RISK" = "CNTRY_OF_RISK"))

# overview of all performance stats for all portfolios

pf1_df <- data.frame(pf = "PF1", stats_opt1)
pf2_df <- data.frame(pf = "PF2", stats_opt2)
merge_df <- bind_rows(pf1_df, pf2_df)

merge_df<- merge_df %>%
  select(pf = pf, ann_rtn, ann_risk, cum_rtn, te, beta, ir, sr, tr, N, alpha, Mkt.RF, SMB, HML, RMW, CMA, r2)


#merge_df$pf <- as.character(merge_df$pf)
#pf_df$pf <- as.character(pf_df$pf)
# Bind the data frames together
final_df <-rbind(pf_df_new,merge_df)

final_df$ann_rtn <- as.numeric(final_df$ann_rtn) * 100
final_df$ann_risk <- as.numeric(final_df$ann_risk) * 100
final_df$cum_rtn <- as.numeric(final_df$cum_rtn) * 100
final_df$te <- as.numeric(final_df$te) * 100
final_df$alpha <- as.numeric(final_df$alpha)


# Assuming df is your dataframe
final_df[, -which(names(final_df) == "pf")] <- apply(final_df[, -which(names(final_df) == "pf")], 2, as.numeric)

final_df<-final_df %>% mutate(across(where(is.numeric), ~ round(., 2)))


# Shorten the pf names
final_df$pf <- gsub("Communication Services", "Comm. Serv.", final_df$pf)
final_df$pf <- gsub("Information Technology", "IT", final_df$pf)
final_df$pf <- gsub("Consumer Discretionary", "Cons. Discr.", final_df$pf)
final_df$pf <- gsub("Consumer Staples", "Cons. Staples", final_df$pf)
final_df$pf <- gsub("Real Estate", "Real Est.", final_df$pf)

final_df <- final_df[order(final_df$ann_rtn), ]

print(xtable(final_df, caption = "Portfolio Performance"), include.rownames = FALSE)
print(xtable(country_ow[order(-country_ow$PF1),][1:11, ], caption = "Country Allocation"), include.rownames = FALSE)
print(xtable(sector_ow[order(-sector_ow$PF1),], caption = "Sector Allocation"), include.rownames = FALSE)
