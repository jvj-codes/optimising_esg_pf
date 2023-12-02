# Load "quadprog" package
suppressMessages(library(PortfolioAnalytics))
suppressMessages(library(foreach)) 
suppressMessages(library(iterators))
suppressMessages(library(ROI))
suppressMessages(library(ROI.plugin.quadprog)) 
suppressMessages(library(ROI.plugin.glpk))
suppressMessages(library(dplyr))


static_data <- msci_filt_df_new %>%
  select(stock, GICS_SECTOR_NAME, CNTRY_OF_RISK)

sector_list <- list()
country_list <- list()
max_list <- list()
min_list <- list()

min_w_sec <- 0.00
max_w_sec <- 0.20

min_w_cntry <- 0.00
max_w_cntry <- 0.20

# Iterate through each row in the dataframe
for (i in 1:nrow(static_data)) {
  stock <- static_data$stock[i]
  sector <- static_data$GICS_SECTOR_NAME[i]
  country <- static_data$CNTRY_OF_RISK[i]
  
  # Create sector groupings
  if (!is.null(sector_list[[sector]])) {
    sector_list[[sector]] <- c(sector_list[[sector]], i)  # Append row number
  } else {
    sector_list[[sector]] <- i  # Create a new entry
    min_list <- c(min_list, min_w_sec)
    max_list <- c(max_list, max_w_sec)
  }
}
  
for (i in 1:nrow(static_data)) {
  stock <- static_data$stock[i]
  country <- static_data$CNTRY_OF_RISK[i]
  if (!is.null(country_list[[country]])) {
    country_list[[country]] <- c(country_list[[country]], i)  # Append row number
  } else {
    country_list[[country]] <- i  # Create a new entry
    min_list <- append(min_list, min_w_cntry)
    max_list <- append(max_list, max_w_cntry)
  }
}


row.names(red_uni) <- red_uni[, 1]
returns <- red_uni[, -1] * 100
row.names(returns) <- red_uni[, 1]
stocks <- colnames(returns)

portf_maxret <- portfolio.spec(assets = stocks)
portf_maxret<-add.constraint(portfolio = portf_maxret, type = "weight_sum", min_sum=0.99, max_sum = 1.01)

min <- numeric(length(stocks))
max <- c(rep(1, length(stocks)))
portf_maxret<-add.constraint(portfolio=portf_maxret,type="box", 
              min=min, 
              max=max)

group_names<- names(c(sector_list,country_list))
portf_maxret <- add.constraint(portfolio = portf_maxret,
                               type = "group",
                               groups = c(sector_list,country_list),
                               group_labels = group_names,
                               group_min =  unlist(min_list),
                               group_max = unlist(max_list))
portf_maxret<-add.objective(portfolio=portf_maxret,type="return",name="mean")
print(portf_maxret)

opt_maxret<-optimize.portfolio(R=returns,portfolio=portf_maxret, optimize_method="ROI",trace=FALSE)
print(opt_maxret$weights)
sum(opt_maxret$weights)

#add to msci_filt_df_new
msci_filt_df_new$w_opt_1 <- opt_maxret$weights

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


#### minvar, target mean of acwi
portf_minvar <- portfolio.spec(assets=stocks)
portf_minvar <- add.constraint(portfolio = portf_minvar, type = "weight_sum", min_sum=0.99, max_sum = 1.01)
portf_minvar <- add.constraint(portfolio=portf_minvar,type="box", 
                             min=min, 
                             max=max)
portf_minvar <- add.constraint(portfolio=portf_minvar, type = "target", return_target = 0.02397893)
portf_minvar <- add.constraint(portfolio = portf_minvar,
                               type = "group",
                               groups = c(sector_list,country_list),
                               group_labels = group_names,
                               group_min =  unlist(min_list),
                               group_max = unlist(max_list))
portf_minvar <- add.objective(portfolio=portf_minvar, type="risk", name="StdDev")

opt_minvar <- optimize.portfolio(R=returns,portfolio=portf_minvar, optimize_method="ROI",trace=TRUE)
sum(opt_minvar$weights)
msci_filt_df_new$w_opt_2 <- opt_minvar$weights

## add to msci_filt

filtered_stocks <- msci_filt_df_new %>%
  filter(w_opt_2 > 0) %>%
  select(stock, w_opt_2)

filtered_return <- percentage_change_df %>%
  select(date, filtered_stocks$stock)

filtered_return$pf_rtn <- rowSums(filtered_return[,-1] * filtered_stocks$w_opt_2, na.rm = TRUE)

stats_opt2 <- perf_stats(filtered_return, filtered_stocks$w_opt_2, pct_chg_index, ff_data)

test3 <- msci_filt_df_new %>%
  group_by(GICS_SECTOR_NAME) %>%
  summarise(Total_Weight = sum(w_opt_2))

test4 <- msci_filt_df_new %>%
  group_by(CNTRY_OF_RISK) %>%
  summarise(Total_Weight = sum(w_opt_2))



#### BACKTESTING

bt_maxret<-optimize.portfolio.rebalancing(R=returns,portfolio=portf_maxret,optimize_method="random", rebalance_on="years", training_period=252*3, rolling_window =63, trace=FALSE)
bt_minvar<-optimize.portfolio.rebalancing(R=returns,portfolio=portf_minvar,optimize_method="ROI", rebalance_on="quarters", training_period=36)

