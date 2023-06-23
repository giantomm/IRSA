
# Retrieve interes rates --------------------------------------------------

# Required libraries
library(xts)
library(quantmod)
library(lubridate)

retrieve_rates <- function(){
  # Set the FRED API key
  fred_api_key <- "c26226691feffdfaced45c83539150ca"
  
  curve_sections <- c("FEDFUNDS", "GS1M", "GS3M", "GS6M", "GS1", "GS2", "GS3", "GS5", 
                      "GS7", "GS10", "GS20", "GS30")
  rate_names <- c("1D", "1M", "3M", "6M", "1Y", "2Y", "3Y", "5Y",
                  "7Y", "10Y", "20Y", "30Y")
  
  options(quandl.api_key = fred_api_key)
  
  # Create an empty xts object
  merged_xts <- xts(order.by = as.Date(character(0)))
  
  # Loop over curve_sections and merge each xts object with merged_xts
  for (rates in curve_sections){
    rate_xts <- getSymbols(rates, src = "FRED", auto.assign = FALSE)
    rate_xts <- setNames(rate_xts, rate_names[match(rates, curve_sections)])
    merged_xts <- merge.xts(merged_xts, rate_xts)
  }
  
  merged_xts <- na.omit(merged_xts)
  colnames(merged_xts) <- rate_names
  return(merged_xts)
}

# Date limit --------------------------------------------------------------


date_limit <- function(date_string, xts_object){
  end_date <- as.Date(date_string)
  start_date <- end_date - years(15)
  
  # Subset the xts object to keep data between start_date and end_date
  subset_xts <- xts_object[start_date <= index(xts_object) & index(xts_object) <= end_date]
  
  return(subset_xts)
}


# Yield Curve -------------------------------------------------------------

YC_generator <- function(date_string, xts_object){
  YC_date <- as.Date(date_string)
  
  subset_xts <- xts_object[index(xts_object) == YC_date]
  curve <- as.data.frame(subset_xts)
  return(subset_xts)
}

# FedFunds for plot -------------------------------------------------------

fedfunds_xts <- function(xts_object){
  fed_funds <- xts_object[,1]
  colnames(fed_funds) <- c("FedFunds")
  return(fed_funds)
}



# Scenario part generator -------------------------------------------------

scenario_part <- function(nmonths, yield_change, xts_object){
  last_date <- index(xts_object[dim(xts_object)[1]])
  last_rate <- as.numeric(xts_object[dim(xts_object)[1]])
  
  date_start <- last_date + months(1)
  date_end <- date_start + months(nmonths-1)
  
  avg_change <- yield_change/nmonths
  set.seed(100)
  if (yield_change != 0){
    rate_vec <- abs(rnorm(nmonths, mean = avg_change, sd = sqrt(nmonths/12)))
    rate_vec <- rate_vec/sum(rate_vec)*yield_change
  }
  else{
    rate_vec <- rate_vec <- rnorm(nmonths, mean = avg_change, sd = 0.01)
  }
  
  # rate dates for n years
  dates <- seq(date_start, date_end, by = "months")
  rates <- cumsum(rate_vec)+last_rate
  
  scen1 <- xts(rates, order.by = dates)
  colnames(scen1) <- colnames(xts_object)
  df_s1 <- rbind(xts_object, scen1)
  return(df_s1)
  
}


# FedFunds base scenario --------------------------------------------------

basescenario <- function(xts_object){
  with_p1 <- scenario_part(24, 0.1, xts_object)
  with_p2 <- scenario_part(12, 0, with_p1)
  with_p3 <- scenario_part(24, -0.1, with_p2)
  return(list("curve" = with_p3, title = "Federal Funds Effective Rate with Base Scenario", scale = "Rate in %"))
}

# FedFunds for Scenario 1 -------------------------------------------------

scenario1 <- function(xts_object){
  with_p1 <- scenario_part(16, 4, xts_object)
  with_p2 <- scenario_part(24, 1, with_p1)
  with_p3 <- scenario_part(20, 0, with_p2)
  return(list("curve" = with_p3, title = "Federal Funds Effective Rate with Stress Scenario 1", scale = "Rate in %"))
}



# FedFunds for Scenario 2 -------------------------------------------------

scenario2 <- function(xts_object){
  with_p1 <- scenario_part(24, 2.5, xts_object)
  with_p2 <- scenario_part(36, 0, with_p1)
  return(list("curve" = with_p2, title = "Federal Funds Effective Rate with Stress Scenario 2", scale = "Rate in %"))
}


# Shifted Curve -----------------------------------------------------------

shifted_YC <- function(xts_yield_curve, nyears, xts_object){
  YC_date <- as.Date(index(xts_yield_curve))
  shiftedYC_date <- YC_date + years(nyears)
  
  YC_base_rate <- as.numeric(xts_object[YC_date == index(xts_object)])
  shiftedYC_base_rate <- as.numeric(xts_object[shiftedYC_date == index(xts_object)])
  base_rate_diff <- shiftedYC_base_rate - YC_base_rate
  
  shiftedYC <- xts_yield_curve + base_rate_diff
  index(shiftedYC) <- shiftedYC_date
  # print(base_rate_diff)
  return(shiftedYC)
}



# to YC object ------------------------------------------------------------

YCvector2YC <- function(YCvector){
  YCvector <- YCvector/100
  tenors <- length(YCvector)
  df_names <- numeric()
  for (i in 1:tenors) {
    tenor <- paste0("tenor.", i)
    rate <- paste0("rate.", i)
    df_names <- c(df_names, tenor, rate)
  }
  YCdf <- as.data.frame(t(rep(0, length(df_names))))
  colnames(YCdf) <- df_names
  for (i in seq(1, ncol(YCdf), 2)) {
    for (j in 1:ncol(YCvector)) {
      if (paste("tenor.", index(colnames(YCvector))[j], sep = "") == colnames(YCdf)[i]) {
        YCdf[i] <- colnames(YCvector)[j]
        YCdf[i+1] <- as.vector(YCvector)[j]
      }
    }
  }
  YCdf$rfType <- "YieldCurve"
  refDate <- index(YCvector)
  YCdf$label <- paste("YC_", substr(as.character(refDate), start = 1, stop = 4), sep = "")
  YCdf$referenceDate <- as.character(refDate)
  YCdf <- YCdf[c("rfType", "label", "referenceDate", df_names)]
  YCobject <- riskFactors_df2list(YCdf)
  return(YCobject)
}


# Merge Balance Sheet -----------------------------------------------------

merge_bs <- function(bs_list, column_names = NULL) {
  # Create a new data frame with row names from the first data frame
  merged_bs <- data.frame(row.names = row.names(bs_list[[1]]))
  
  # Merge the columns from each data frame into the new data frame
  for (i in seq_along(bs_list)) {
    column <- bs_list[[i]][1]
    if (is.null(column_names)) {
      merged_bs <- cbind(merged_bs, column)
    } else {
      colnames(merged_bs)[i] <- column_names[i]
      merged_bs <- cbind(merged_bs, column)
    }
  }
  
  return(merged_bs[,-1]) # -1 because first year is just 0
}



# Ratios ------------------------------------------------------------------

current_ratio <- function(balance_sheet){
  ratio <- balance_sheet[3,]/balance_sheet[17,]*(-1)
  return(ratio)
}

loan_to_deposit_ratio <- function(balance_sheet){
  deposits <- balance_sheet[18,] + balance_sheet[19,]
  ratio <- balance_sheet[9,]/deposits*(-1)
  return(ratio)
}

debt_to_equity_ratio <- function(balance_sheet){
  liabilities <- balance_sheet[17,] + balance_sheet[21,]
  ratio <- liabilities*(-1)/balance_sheet[25,]*(-1)
  return(ratio)
}

equity_ratio <- function(balance_sheet){
  ratio <- balance_sheet[25,]*(-1)/balance_sheet[2,]
  return(ratio)
}

return_on_equity <- function(net_income, balance_sheet){
  ratio <- net_income/balance_sheet[25,]*(-1)
  return(ratio)
}

return_on_assets <- function(net_income, balance_sheet){
  ratio <- net_income/balance_sheet[2,]
  return(ratio)
}

net_income_pred <- function(last_net_income, balance_sheet, growth_rate){
  ni_vector <- rep(last_net_income, dim(balance_sheet)[2])
  for (i in seq_along(balance_sheet)){
    ni_vector[i] <- ni_vector[i]*((1+growth_rate)^i)
  }
  return(ni_vector)
}

# Ratio Dataframe ---------------------------------------------------------

ratios_old <- function(last_net_income, balance_sheet, growth_rate){
  # net income prediction
  net_income <- net_income_pred(last_net_income, balance_sheet, growth_rate)
  
  # calculation of ratios
  cr <- current_ratio(balance_sheet)
  ldr <- loan_to_deposit_ratio(balance_sheet)
  roa <- return_on_assets(net_income, balance_sheet)
  roe <- return_on_equity(net_income, balance_sheet)
  der <- debt_to_equity_ratio(balance_sheet)
  er <- equity_ratio(balance_sheet)
  fl <- roe/roa
  
  # creation of dataframe
  out <- rbind(cr, ldr, roa, roe, der, er, fl)
  rownames(out) <- c("Current ratio", "Loan-to-deposit ratio",
                     "Return on assets", "Return on equity",
                     "Debt-to-equity ratio", "Equity ratio",
                     "Financial leverage")
  return(out)
}



# Merge fair value and nominal value statements ---------------------------

merge_fv_nv <- function(market, nominal){
  new_afs <- market[5,]
  new_htm <- market[6,]
  old_afs <- nominal[5,]
  old_htm <- nominal[6,]
  bs_reduction <-  old_afs+old_htm-new_afs-new_htm
  
  # merge new balance sheet
  nominal[2,] <- nominal[2,]-bs_reduction
  nominal[3,] <- nominal[3,]-bs_reduction
  nominal[5,] <- new_afs
  nominal[6,] <- new_htm
  
  nominal[16,] <- nominal[16,]+bs_reduction
  nominal[25,] <- nominal[25,]+bs_reduction
  return(nominal)
}


merge_svb <- function(market, nominal){
  new_afs <- market[5,]
  old_afs <- nominal[5,]
  bs_reduction <-  old_afs-new_afs
  
  # merge new balance sheet
  nominal[2,] <- nominal[2,]-bs_reduction
  nominal[3,] <- nominal[3,]-bs_reduction
  nominal[5,] <- new_afs
  
  nominal[16,] <- nominal[16,]+bs_reduction
  nominal[25,] <- nominal[25,]+bs_reduction
  return(nominal)
}


# Withdrawal scenarios ----------------------------------------------------

# Demand deposit decrease
demand_decrease <- function(percent, balance_sheet){
  # reduce relevant positions on liabilities side
  decrease <- balance_sheet[18,]*percent
  new_dd <- balance_sheet[18,]-decrease
  new_stl <- balance_sheet[17,]-decrease
  new_liab <- balance_sheet[16,]-decrease
  # reduce assets side
  for (i in seq_along(balance_sheet)){
    assets_dec <- decrease[i]*(-1)
    if (assets_dec <= balance_sheet[4,i]){
      new_cash <- balance_sheet[4,i]-assets_dec
      new_afs <- balance_sheet[5,i]
      new_htm <- balance_sheet[6,i]
      new_sta <- balance_sheet[3,i]-assets_dec
      new_as <- balance_sheet[2,i]-assets_dec
    }
    else if (assets_dec <= balance_sheet[4,i] + balance_sheet[5,i]){
      assets_dec <- assets_dec-balance_sheet[4,i]
      new_cash <- 0
      new_afs <- balance_sheet[5,i]-assets_dec
      new_htm <- balance_sheet[6,i]
      new_sta <- balance_sheet[3,i]-assets_dec-balance_sheet[4,i]
      new_as <- balance_sheet[2,i]-assets_dec-balance_sheet[4,i]
    }
    else if (assets_dec <= balance_sheet[3,i]){
      assets_dec <- assets_dec-balance_sheet[4,i]-balance_sheet[5,i]
      new_cash <- 0
      new_afs <- 0
      new_htm <- balance_sheet[6,i]-assets_dec
      new_sta <- balance_sheet[3,i]-assets_dec-balance_sheet[4,i]-balance_sheet[5,i]
      new_as <- balance_sheet[2,i]-assets_dec-balance_sheet[4,i]-balance_sheet[5,i]
    }
    else{
      return("Failed, not enough liquid assets")
    }

    # asjust balance sheet's assets
    balance_sheet[6,i] <- new_htm
    balance_sheet[5,i] <- new_afs
    balance_sheet[4,i] <- new_cash
    balance_sheet[3,i] <- new_sta
    balance_sheet[2,i] <- new_as
  }
  # adjust the balance sheet's liabilities
  balance_sheet[18,] <- new_dd
  balance_sheet[17,] <- new_stl
  balance_sheet[16,] <- new_liab
  return(balance_sheet)
}

# deposit decrease
deposit_decrease <- function(percent, balance_sheet){
  # reduce relevant positions on liabilities side
  decrease <- balance_sheet[19,]*percent
  new_dp <- balance_sheet[19,]-decrease
  new_stl <- balance_sheet[17,]-decrease
  new_liab <- balance_sheet[16,]-decrease
  # reduce assets side
  for (i in seq_along(balance_sheet)){
    assets_dec <- decrease[i]*(-1)
    if (assets_dec <= balance_sheet[4,i]){
      new_cash <- balance_sheet[4,i]-assets_dec
      new_afs <- balance_sheet[5,i]
      new_htm <- balance_sheet[6,i]
      new_sta <- balance_sheet[3,i]-assets_dec
      new_as <- balance_sheet[2,i]-assets_dec
    }
    else if (assets_dec <= balance_sheet[4,i] + balance_sheet[5,i]){
      assets_dec <- assets_dec-balance_sheet[4,i]
      new_cash <- 0
      new_afs <- balance_sheet[5,i]-assets_dec
      new_htm <- balance_sheet[6,i]
      new_sta <- balance_sheet[3,i]-assets_dec-balance_sheet[4,i]
      new_as <- balance_sheet[2,i]-assets_dec-balance_sheet[4,i]
    }
    else if (assets_dec <= balance_sheet[3,i]){
      assets_dec <- assets_dec-balance_sheet[4,i]-balance_sheet[5,i]
      new_cash <- 0
      new_afs <- 0
      new_htm <- balance_sheet[6,i]-assets_dec
      new_sta <- balance_sheet[3,i]-assets_dec-balance_sheet[4,i]-balance_sheet[5,i]
      new_as <- balance_sheet[2,i]-assets_dec-balance_sheet[4,i]-balance_sheet[5,i]
    }
    else{
      return("Failed, not enough liquid assets")
    }
    
    # asjust balance sheet's assets
    balance_sheet[6,i] <- new_htm
    balance_sheet[5,i] <- new_afs
    balance_sheet[4,i] <- new_cash
    balance_sheet[3,i] <- new_sta
    balance_sheet[2,i] <- new_as
  }
  # adjust the balance sheet's liabilities
  balance_sheet[19,] <- new_dp
  balance_sheet[17,] <- new_stl
  balance_sheet[16,] <- new_liab
  return(balance_sheet)
}


# Loss on securities if sold  ---------------------------------------------

securities_loss <- function(market, nominal){
  afs_loss <- market[5,]-nominal[5,]
  htm_loss <- market[6,]-nominal[6,]
  total_loss <- afs_loss+htm_loss
  rownames(total_loss) <- "Gain if realized (to nominal)"
  return(total_loss)
}

# New withdrawal scenarios ------------------------------------------------

withdrawals <- function(demand_dec, deposit_dec, market, nominal, income_s){
  balance_sheet <- merge_svb(market, nominal)
  
  # store relevant variables
  demand_sub <- balance_sheet[18,]*demand_dec
  deposit_sub <- balance_sheet[19,]*deposit_dec
  total_sub <- (demand_sub+deposit_sub)
  old_afs <- balance_sheet[5,]
  old_htm <- balance_sheet[6,]
  
  # reduce liabilities side
  new_demand <- balance_sheet[18,]-demand_sub
  new_deposit <- balance_sheet[19,]-deposit_sub
  new_stl <- balance_sheet[17,]-total_sub
  new_liab <- balance_sheet[16,]-total_sub
  
  # reduce assets side
  for (i in seq_along(balance_sheet)){
    assets_dec <- total_sub[i]*(-1)
    if (assets_dec <= balance_sheet[4,i]){
      new_cash <- balance_sheet[4,i]-assets_dec
      new_afs <- balance_sheet[5,i]
      new_htm <- balance_sheet[6,i]
      new_sta <- balance_sheet[3,i]-assets_dec
      new_as <- balance_sheet[2,i]-assets_dec
    }
    else if (assets_dec <= balance_sheet[4,i] + balance_sheet[5,i]){
      assets_dec <- assets_dec-balance_sheet[4,i]
      new_cash <- 0
      new_afs <- balance_sheet[5,i]-assets_dec
      new_htm <- balance_sheet[6,i]
      new_sta <- balance_sheet[3,i]-assets_dec-balance_sheet[4,i]
      new_as <- balance_sheet[2,i]-assets_dec-balance_sheet[4,i]
    }
    else if (assets_dec <= balance_sheet[3,i]){
      assets_dec <- assets_dec-balance_sheet[4,i]-balance_sheet[5,i]
      new_cash <- 0
      new_afs <- 0
      new_htm <- balance_sheet[6,i]-assets_dec
      new_sta <- balance_sheet[3,i]-assets_dec-balance_sheet[4,i]-balance_sheet[5,i]
      new_as <- balance_sheet[2,i]-assets_dec-balance_sheet[4,i]-balance_sheet[5,i]
    }
    else{
      return("Failed, not enough liquid assets")
    }
    
    # asjust balance sheet's assets
    balance_sheet[6,i] <- new_htm
    balance_sheet[5,i] <- new_afs
    balance_sheet[4,i] <- new_cash
    balance_sheet[3,i] <- new_sta
    balance_sheet[2,i] <- new_as
  }
  # adjust the balance sheet's liabilities
  balance_sheet[19,] <- new_deposit
  balance_sheet[18,] <- new_demand
  balance_sheet[17,] <- new_stl
  balance_sheet[16,] <- new_liab
  
  # losses for securities
  max_afs_loss <- market[5,]-nominal[5,]
  afs_perc <- balance_sheet[5,]/old_afs
  afs_loss <- max_afs_loss*(1-afs_perc)
  
  max_htm_loss <- market[6,]-nominal[6,]
  htm_perc <- balance_sheet[6,]/old_htm
  htm_loss <- max_htm_loss*(1-htm_perc)
  
  max_l <- max_afs_loss+max_htm_loss
  rownames(max_l) <- "Maximum possible gain/loss"
  securities_l <- afs_loss+htm_loss
  rownames(securities_l) <- "Realized gain/loss"
  
  starting <- income_s[1,][-6]
  net_income <- starting
  net_income[2,] <- securities_l
  net_income[3,] <- starting+securities_l
  rownames(net_income) <- c("Starting net income", "Realized gain/loss", "Net income for the year")
  
  balance_sheet <- rebalance(balance_sheet, net_income)
  out <- list("BS" = round(balance_sheet,0), "MaxLoss" = round(max_l,2), "Loss" = round(securities_l,2), "NI" = round(net_income,2))
  return(out)
}

rebalance <- function(balance_sheet, net_income){
  for (i in seq_along(balance_sheet)){
    ni <- net_income[3,i]
    if (ni >= 0){
      balance_sheet[2,i] <- balance_sheet[2,i]+ni
      balance_sheet[3,i] <- balance_sheet[3,i]+ni
      balance_sheet[4,i] <- balance_sheet[4,i]+ni
      balance_sheet[16,i] <- balance_sheet[16,i]-ni
      balance_sheet[25,i] <- balance_sheet[25,i]-ni
    }
    else {
      balance_sheet[17,i] <- balance_sheet[17,i]+ni
      balance_sheet[20,i] <- balance_sheet[20,i]+ni
      balance_sheet[25,i] <- balance_sheet[25,i]-ni
    }
  }
  return(balance_sheet)
}



# Revised ratios function --------------------------------------------------
ratios <- function(balance_sheet, income_statement){
  # net income prediction
  net_income <- as.vector(income_statement[3,])
  
  # calculation of ratios
  cr <- current_ratio(balance_sheet)
  ldr <- loan_to_deposit_ratio(balance_sheet)
  roa <- return_on_assets(net_income, balance_sheet)
  roe <- return_on_equity(net_income, balance_sheet)
  der <- debt_to_equity_ratio(balance_sheet)
  er <- equity_ratio(balance_sheet)
  fl <- roe/roa
  
  # creation of dataframe
  out <- rbind(cr, ldr, roa, roe, der, er, fl)
  rownames(out) <- c("Current ratio", "Loan-to-deposit ratio",
                     "Return on assets", "Return on equity",
                     "Debt-to-equity ratio", "Equity ratio",
                     "Financial leverage")
  return(round(out, 6))
}

