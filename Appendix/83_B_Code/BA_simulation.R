## BA
# devtools::install_github("hamzavig/FRSA")
library(FRSA)
library(timeSeries)
# The path needs to be adjusted to the "BA_functions_only.R" file
source("C:/Users/giana/OneDrive - ZHAW/Bachelorarbeit/Attachments/83_B_Code/BA_functions_only.R")


# Create institution ------------------------------------------------------
bank <- createInstitution("SVB")

bank$Assets$ShortTermAssets$AddChild("Cash and cash equivalents")
bank$Assets$ShortTermAssets$AddChild("AFS securities")
bank$Assets$ShortTermAssets$AddChild("HTM securities")

bank$Assets$LongTermAssets$AddChild("Non-marketable securities")
bank$Assets$LongTermAssets$AddChild("Loans")
bank$Assets$LongTermAssets$AddChild("Other assets")

bank$Assets$FixedAssets$AddChild("Premises and equipment")
bank$Assets$FixedAssets$AddChild("Goodwill")
bank$Assets$FixedAssets$AddChild("Other intangible assets")
bank$Assets$FixedAssets$AddChild("Lease right-of-use assets")

bank$Liabilities$ShortTermLiabilities$AddChild("Demand deposits")
bank$Liabilities$ShortTermLiabilities$AddChild("Deposits")
bank$Liabilities$ShortTermLiabilities$AddChild("Short-term borrowings")

bank$Liabilities$LongTermLiabilities$AddChild("Lease liabilities")
bank$Liabilities$LongTermLiabilities$AddChild("Long-term debt")
bank$Liabilities$LongTermLiabilities$AddChild("Other liabilities")

bank


# Add contracts -----------------------------------------------------------
# The path needs to be adjusted to the corresponding CSV files
ann_path <- "C:/Users/giana/OneDrive - ZHAW/Bachelorarbeit/Data/SVB/Final/csv/ann_ptf_2021.csv"
pam_path <- "C:/Users/giana/OneDrive - ZHAW/Bachelorarbeit/Data/SVB/Final/csv/pam_ptf_2021_edited_real.csv"
ops_path <- "C:/Users/giana/OneDrive - ZHAW/Bachelorarbeit/Data/SVB/Final/csv/operations_2021_edited_real.csv"

ann_ptf <- samplePortfolio(ann_path, "contracts")
pam_ptf <- samplePortfolio(pam_path, "contracts")
ops_ptf <- samplePortfolio(ops_path, "operations")

bank <- assignContracts2Tree(bank, ann_ptf)
bank <- assignContracts2Tree(bank, pam_ptf)
bank <- assignContracts2Tree(bank, ops_ptf)

# Scenarios ------------------------------------------------------------

# Get FRED data
rates <- retrieve_rates()

# Limit the FRED data
split_date <- "2022-01-01"
limited_data <- date_limit(split_date, rates)

# Create default YC as vector
YCv_2022 <- YC_generator(split_date, limited_data)

# Create the interest rate Scenarios
FedFunds <- fedfunds_xts(limited_data)
base_scen <- basescenario(FedFunds)
stress_scen1 <- scenario1(FedFunds)
stress_scen2 <- scenario2(FedFunds)

# Plot scenarios
plot(base_scen$curve, main = base_scen$title, ylab = base_scen$scale)
plot(stress_scen1$curve, main = stress_scen1$title, ylab = stress_scen1$scale)
plot(stress_scen2$curve, main = stress_scen2$title, ylab = stress_scen2$scale)


# Yield Curves ------------------------------------------------------------

# Create YCs as vector for each scenarios

# Base Scenario
YCvb_2023 <- shifted_YC(YCv_2022, 1, base_scen$curve)
YCvb_2024 <- shifted_YC(YCv_2022, 2, base_scen$curve)
YCvb_2025 <- shifted_YC(YCv_2022, 3, base_scen$curve)
YCvb_2026 <- shifted_YC(YCv_2022, 4, base_scen$curve)
YCvb_2027 <- shifted_YC(YCv_2022, 5, base_scen$curve)

# Stress Scenario 1
YCv1_2023 <- shifted_YC(YCv_2022, 1, stress_scen1$curve)
YCv1_2024 <- shifted_YC(YCv_2022, 2, stress_scen1$curve)
YCv1_2025 <- shifted_YC(YCv_2022, 3, stress_scen1$curve)
YCv1_2026 <- shifted_YC(YCv_2022, 4, stress_scen1$curve)
YCv1_2027 <- shifted_YC(YCv_2022, 5, stress_scen1$curve)

# Stress Scenario 2
YCv2_2023 <- shifted_YC(YCv_2022, 1, stress_scen2$curve)
YCv2_2024 <- shifted_YC(YCv_2022, 2, stress_scen2$curve)
YCv2_2025 <- shifted_YC(YCv_2022, 3, stress_scen2$curve)
YCv2_2026 <- shifted_YC(YCv_2022, 4, stress_scen2$curve)
YCv2_2027 <- shifted_YC(YCv_2022, 5, stress_scen2$curve)

# Conversion of all curves to YC objects
YC_2022 <- YCvector2YC(YCv_2022)
plot(YC_2022[[1]])

# Base Scenario
YCb_2023 <- YCvector2YC(YCvb_2023)
YCb_2024 <- YCvector2YC(YCvb_2024)
YCb_2025 <- YCvector2YC(YCvb_2025)
YCb_2026 <- YCvector2YC(YCvb_2026)
YCb_2027 <- YCvector2YC(YCvb_2027)

plot(YCb_2025[[1]])

# Stress Scenario 1
YC1_2023 <- YCvector2YC(YCv1_2023)
YC1_2024 <- YCvector2YC(YCv1_2024)
YC1_2025 <- YCvector2YC(YCv1_2025)
YC1_2026 <- YCvector2YC(YCv1_2026)
YC1_2027 <- YCvector2YC(YCv1_2027)

plot(YC1_2025[[1]])

# Stress Scenario 2
YC2_2023 <- YCvector2YC(YCv2_2023)
YC2_2024 <- YCvector2YC(YCv2_2024)
YC2_2025 <- YCvector2YC(YCv2_2025)
YC2_2026 <- YCvector2YC(YCv2_2026)
YC2_2027 <- YCvector2YC(YCv2_2027)

plot(YC2_2025[[1]])


# RF container ------------------------------------------------------------
rfb_list <- list(YC_2022, YCb_2023, YCb_2024, YCb_2025, YCb_2026, YCb_2027) 
rf1_list <- list(YC_2022, YC1_2023, YC1_2024, YC1_2025, YC1_2026, YC1_2027)
rf2_list <- list(YC_2022, YC2_2023, YC2_2024, YC2_2025, YC2_2026, YC2_2027)

RFb <- RFConn(rfb_list)
RF1 <- RFConn(rf1_list)
RF2 <- RFConn(rf2_list)


# Creation of scenario events ---------------------------------------------

bankb <- events(object = bank, riskFactors = RFb)
bank1 <- events(object = bank, riskFactors = RF1)
bank2 <- events(object = bank, riskFactors = RF2)


# Balance Sheets 2022 -----------------------------------------------------

# Date range in timebuckets (tb)
t0 <- as.character(as.Date('2022-01-01'))
tn <- as.character(as.Date('2027-01-01'))

n <- yearFraction(t0, tn)
t0Year <- as.numeric(substr(t0,1,4))
tnYear <- as.numeric(substr(tn,1,4))
by <- timeSequence(t0, by="1 years", length.out=n+2)
tb <- timeBuckets(by, bucketLabs=t0Year:tnYear, 
                  breakLabs=substr(as.character(by),3,10))

marketb_2022 <- value(bankb, tb, type = 'market', method = DcEngine(RFConn(RFb$riskfactors$YC_2022)), digits = 2)
market1_2022 <- value(bank1, tb, type = 'market', method = DcEngine(RFConn(RF1$riskfactors$YC_2022)), digits = 2)
market2_2022 <- value(bank2, tb, type = 'market', method = DcEngine(RFConn(RF2$riskfactors$YC_2022)), digits = 2)

nominalb_2022 <- value(bankb, tb, type = 'nominal', method = DcEngine(), digits = 2)
nominal1_2022 <- value(bank1, tb, type = 'nominal', method = DcEngine(), digits = 2)
nominal2_2022 <- value(bank2, tb, type = 'nominal', method = DcEngine(), digits = 2)

incomeb_2022 <- income(bankb, tb, "marginal")
income1_2022 <- income(bank1, tb, "marginal")
income2_2022 <- income(bank2, tb, "marginal")

# Balance Sheets 2023 -----------------------------------------------------

# Date range in timebuckets (tb)
t0 <- as.character(as.Date('2023-01-01'))
tn <- as.character(as.Date('2027-01-01'))

n <- yearFraction(t0, tn)
t0Year <- as.numeric(substr(t0,1,4))
tnYear <- as.numeric(substr(tn,1,4))
by <- timeSequence(t0, by="1 years", length.out=n+2)
tb <- timeBuckets(by, bucketLabs=t0Year:tnYear, 
                  breakLabs=substr(as.character(by),3,10))

marketb_2023 <- value(bankb, tb, type = 'market', method = DcEngine(RFConn(RFb$riskfactors$YC_2022)), digits = 2)
market1_2023 <- value(bank1, tb, type = 'market', method = DcEngine(RFConn(RF1$riskfactors$YC_2023)), digits = 2)
market2_2023 <- value(bank2, tb, type = 'market', method = DcEngine(RFConn(RF2$riskfactors$YC_2023)), digits = 2)

nominalb_2023 <- value(bankb, tb, type = 'nominal', method = DcEngine(), digits = 2)
nominal1_2023 <- value(bank1, tb, type = 'nominal', method = DcEngine(), digits = 2)
nominal2_2023 <- value(bank2, tb, type = 'nominal', method = DcEngine(), digits = 2)

incomeb_2023 <- income(bankb, tb, "marginal")
income1_2023 <- income(bank1, tb, "marginal")
income2_2023 <- income(bank2, tb, "marginal")
# Balance Sheets 2024 -----------------------------------------------------

# Date range in timebuckets (tb)
t0 <- as.character(as.Date('2024-01-01'))
tn <- as.character(as.Date('2027-01-01'))

n <- yearFraction(t0, tn)
t0Year <- as.numeric(substr(t0,1,4))
tnYear <- as.numeric(substr(tn,1,4))
by <- timeSequence(t0, by="1 years", length.out=n+2)
tb <- timeBuckets(by, bucketLabs=t0Year:tnYear, 
                  breakLabs=substr(as.character(by),3,10))

marketb_2024 <- value(bankb, tb, type = 'market', method = DcEngine(RFConn(RFb$riskfactors$YC_2024)), digits = 2)
market1_2024 <- value(bank1, tb, type = 'market', method = DcEngine(RFConn(RF1$riskfactors$YC_2024)), digits = 2)
market2_2024 <- value(bank2, tb, type = 'market', method = DcEngine(RFConn(RF2$riskfactors$YC_2024)), digits = 2)

nominalb_2024 <- value(bankb, tb, type = 'nominal', method = DcEngine(), digits = 2)
nominal1_2024 <- value(bank1, tb, type = 'nominal', method = DcEngine(), digits = 2)
nominal2_2024 <- value(bank2, tb, type = 'nominal', method = DcEngine(), digits = 2)

incomeb_2024 <- income(bankb, tb, "marginal")
income1_2024 <- income(bank1, tb, "marginal")
income2_2024 <- income(bank2, tb, "marginal")
# Balance Sheets 2025 -----------------------------------------------------

# Date range in timebuckets (tb)
t0 <- as.character(as.Date('2025-01-01'))
tn <- as.character(as.Date('2027-01-01'))

n <- yearFraction(t0, tn)
t0Year <- as.numeric(substr(t0,1,4))
tnYear <- as.numeric(substr(tn,1,4))
by <- timeSequence(t0, by="1 years", length.out=n+2)
tb <- timeBuckets(by, bucketLabs=t0Year:tnYear, 
                  breakLabs=substr(as.character(by),3,10))

marketb_2025 <- value(bankb, tb, type = 'market', method = DcEngine(RFConn(RFb$riskfactors$YC_2025)), digits = 2)
market1_2025 <- value(bank1, tb, type = 'market', method = DcEngine(RFConn(RF1$riskfactors$YC_2025)), digits = 2)
market2_2025 <- value(bank2, tb, type = 'market', method = DcEngine(RFConn(RF2$riskfactors$YC_2025)), digits = 2)

nominalb_2025 <- value(bankb, tb, type = 'nominal', method = DcEngine(), digits = 2)
nominal1_2025 <- value(bank1, tb, type = 'nominal', method = DcEngine(), digits = 2)
nominal2_2025 <- value(bank2, tb, type = 'nominal', method = DcEngine(), digits = 2)

incomeb_2025 <- income(bankb, tb, "marginal")
income1_2025 <- income(bank1, tb, "marginal")
income2_2025 <- income(bank2, tb, "marginal")

# Balance Sheets 2026 -----------------------------------------------------

# Date range in timebuckets (tb)
t0 <- as.character(as.Date('2026-01-01'))
tn <- as.character(as.Date('2027-01-01'))

n <- yearFraction(t0, tn)
t0Year <- as.numeric(substr(t0,1,4))
tnYear <- as.numeric(substr(tn,1,4))
by <- timeSequence(t0, by="1 years", length.out=n+2)
tb <- timeBuckets(by, bucketLabs=t0Year:tnYear, 
                  breakLabs=substr(as.character(by),3,10))

marketb_2026 <- value(bankb, tb, type = 'market', method = DcEngine(RFConn(RFb$riskfactors$YC_2026)), digits = 2)
market1_2026 <- value(bank1, tb, type = 'market', method = DcEngine(RFConn(RF1$riskfactors$YC_2026)), digits = 2)
market2_2026 <- value(bank2, tb, type = 'market', method = DcEngine(RFConn(RF2$riskfactors$YC_2026)), digits = 2)

nominalb_2026 <- value(bankb, tb, type = 'nominal', method = DcEngine(), digits = 2)
nominal1_2026 <- value(bank1, tb, type = 'nominal', method = DcEngine(), digits = 2)
nominal2_2026 <- value(bank2, tb, type = 'nominal', method = DcEngine(), digits = 2)

incomeb_2026 <- income(bankb, tb, "marginal")
income1_2026 <- income(bank1, tb, "marginal")
income2_2026 <- income(bank2, tb, "marginal")

# Balance Sheets 2027 -----------------------------------------------------

# Date range in timebuckets (tb)
t0 <- as.character(as.Date('2027-01-01'))
tn <- as.character(as.Date('2027-01-01'))

n <- yearFraction(t0, tn)
t0Year <- as.numeric(substr(t0,1,4))
tnYear <- as.numeric(substr(tn,1,4))
by <- timeSequence(t0, by="1 years", length.out=n+2)
tb <- timeBuckets(by, bucketLabs=t0Year:tnYear, 
                  breakLabs=substr(as.character(by),3,10))

marketb_2027 <- value(bankb, tb, type = 'market', method = DcEngine(RFConn(RFb$riskfactors$YC_2027)), digits = 2)
market1_2027 <- value(bank1, tb, type = 'market', method = DcEngine(RFConn(RF1$riskfactors$YC_2027)), digits = 2)
market2_2027 <- value(bank2, tb, type = 'market', method = DcEngine(RFConn(RF2$riskfactors$YC_2027)), digits = 2)

nominalb_2027 <- value(bankb, tb, type = 'nominal', method = DcEngine(), digits = 2)
nominal1_2027 <- value(bank1, tb, type = 'nominal', method = DcEngine(), digits = 2)
nominal2_2027 <- value(bank2, tb, type = 'nominal', method = DcEngine(), digits = 2)

# Lists for Balance Sheets of each Scenario -------------------------------
marketb_list <- list(marketb_2022, marketb_2023, marketb_2024, marketb_2025,
                     marketb_2026, marketb_2027)

market1_list <- list(market1_2022, market1_2023, market1_2024, market1_2025,
                     market1_2026, market1_2027)

market2_list <- list(market2_2022, market2_2023, market2_2024, market2_2025,
                     market2_2026, market2_2027)

nominalb_list <- list(nominalb_2022, nominalb_2023, nominalb_2024,
                      nominalb_2025, nominalb_2026, nominalb_2027)

nominal1_list <- list(nominal1_2022, nominal1_2023, nominal1_2024,
                      nominal1_2025, nominal1_2026, nominal1_2027)

nominal2_list <- list(nominal2_2022, nominal2_2023, nominal2_2024,
                      nominal2_2025, nominal2_2026, nominal2_2027)

# Adjusted Balance Sheets
marketb <- merge_bs(marketb_list)
market1 <- merge_bs(market1_list)
market2 <- merge_bs(market2_list)

nominalb <- merge_bs(nominalb_list)
nominal1 <- merge_bs(nominal1_list)
nominal2 <- merge_bs(nominal2_list)


# Results -----------------------------------------------------------------

# General
income_statement <- income1_2022
income_statement
# base scenario

base_svb <- merge_svb(marketb, nominalb)
base_svb

base_fv <- merge_fv_nv(marketb, nominalb)
base_fv

base_w1 <- withdrawals(0.2, 0.1, marketb, nominalb, income_statement)
base_w1$BS
base_w1$MaxLoss
base_w1$Loss
base_w1$NI

ratios(base_w1$BS, base_w1$NI)

base_w2 <- withdrawals(0.4, 0.2, marketb, nominalb, income_statement)
base_w2$BS
base_w2$NI

ratios(base_w2$BS, base_w2$NI)

# stress scenario 1
scen1_svb <- merge_svb(market1, nominal1)
scen1_svb

scen1_fv <- merge_fv_nv(market1, nominal1)
scen1_fv

scen1_w1 <- withdrawals(0.2, 0.1, market1, nominal1, income_statement)
scen1_w1$BS
scen1_w1$MaxLoss
scen1_w1$NI

ratios(scen1_w1$BS, scen1_w1$NI)

scen1_w2 <- withdrawals(0.4, 0.2, market1, nominal1, income_statement)
scen1_w2$BS
scen1_w2$NI

ratios(scen1_w2$BS, scen1_w2$NI)
# stress scenario 2
scen2_svb <- merge_svb(market2, nominal2)
scen2_svb

scen2_fv <- merge_fv_nv(market2, nominal2)
scen2_fv

scen2_w1 <- withdrawals(0.2, 0.1, market2, nominal2, income_statement)
scen2_w1$BS
scen2_w1$MaxLoss
scen2_w1$NI

ratios(scen2_w1$BS, scen2_w1$NI)

scen2_w2 <- withdrawals(0.4, 0.2, market2, nominal2, income_statement)
scen2_w2$BS
scen2_w2$NI

ratios(scen2_w2$BS, scen2_w2$NI)
