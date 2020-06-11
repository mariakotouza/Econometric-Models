#install.packages(c("tidyquant", "tidyverse", "timetk", "broom"))

library(tidyquant)
library(tidyverse)
library(timetk)
library(broom)
library(glue)

source("functions.R")

estimation_ids <- -270:-21

# Set windows for testing 
window_pre <- matrix(0, nrow = 4, ncol = 2)
window_pre[1,] <- c(-20,0)
window_pre[2,] <- c(-20,2)
window_pre[3,] <- c(-10,0)
window_pre[4,] <- c(-5,0)

window_an <- matrix(0, nrow = 6, ncol = 2)
window_an[1,] <- c(-20,20)
window_an[2,] <- c(-10,10)
window_an[3,] <- c(-5,5)
window_an[4,] <- c(-3,3)
window_an[5,] <- c(-1,1)
window_an[6,] <- c(0,0)

window_post <- matrix(0, nrow = 5, ncol = 2)
window_post[1,] <- c(0,20)
window_post[2,] <- c(2,20)
window_post[3,] <- c(0,10)
window_post[4,] <- c(0,5)
window_post[5,] <- c(0,1)

# Read asset returns 
returns <- read.csv("params/RETURNS_2003 2018 FULL DATA.csv", sep = ";", stringsAsFactors = F)
colnames(returns)[1] <- "Date"
returns <- data.frame(lapply(returns, function(x) {gsub(",", ".", x)}), stringsAsFactors = F)
returns <- data.frame(lapply(returns, function(x) {gsub("na", NA, x)}), stringsAsFactors = F)
returns[,2:ncol(returns)] <- lapply(returns[,2:ncol(returns)], function(x) {as.numeric(x)})
returns[,1] <- as.Date(returns[,1],"%d/%m/%Y")
colnames(returns)[2:ncol(returns)] <- str_replace(colnames(returns)[2:ncol(returns)], "X", "")

# Read announcement dates
announcements <- read.csv("params/2003 2018 EVENTS  ANNOUNCEMENT DATA.csv", sep = ";", stringsAsFactors = F)
announcements$DATE <- as.Date(announcements$DATE,"%d/%m/%Y")

companies <- colnames(returns)[2:ncol(returns)]

# Read factors for the market model
marketsp500us <- read.csv("params/marketsp500us.csv", sep = ";", stringsAsFactors = F)
colnames(marketsp500us) <- c("Date", "Mkt")
marketsp500us[,1] <- as.Date(marketsp500us[,1])

marketbanksus <- read.csv("params/banksusreturns.csv", sep = ";", stringsAsFactors = F)
colnames(marketbanksus) <- c("Date", "Mkt")
marketbanksus[,1] <- as.Date(marketbanksus[,1],"%d/%m/%Y")

# Read factors for the 4-factor model
Global_4_Factors <- read.csv("params/4FACTOR RETURNS 2003 2018 DATA FULL PERIOD.csv", sep = ";", stringsAsFactors = F)
colnames(Global_4_Factors)[1] <- "Date"
Global_4_Factors <- data.frame(lapply(Global_4_Factors, function(x) {gsub(",", ".", x)}), stringsAsFactors = F)
Global_4_Factors <- data.frame(lapply(Global_4_Factors, function(x) {gsub("na", NA, x)}), stringsAsFactors = F)
Global_4_Factors[,2:ncol(Global_4_Factors)] <- lapply(Global_4_Factors[,2:ncol(Global_4_Factors)], function(x) {as.numeric(x)})
Global_4_Factors[,1] <- as.Date(Global_4_Factors[,1],"%d/%m/%Y")
colnames(Global_4_Factors)[2] <- "Mkt" 
colnames(Global_4_Factors)[3] <- "RF" 

# Read factors for the 5-factor model
Global_5_Factors <- read.csv("params/5factor returns 1990-2018.csv", sep = ";", stringsAsFactors = F)
colnames(Global_5_Factors)[1] <- "Date"
Global_5_Factors <- data.frame(lapply(Global_5_Factors, function(x) {gsub(",", ".", x)}), stringsAsFactors = F)
Global_5_Factors <- data.frame(lapply(Global_5_Factors, function(x) {gsub("na", NA, x)}), stringsAsFactors = F)
Global_5_Factors[,2:ncol(Global_5_Factors)] <- lapply(Global_5_Factors[,2:ncol(Global_5_Factors)], function(x) {as.numeric(x)})
Global_5_Factors[,1] <- as.Date(Global_5_Factors[,1],"%d/%m/%Y")
colnames(Global_5_Factors)[2] <- "Mkt" 

# Metrics 
models <- c("mkt_sp500us", "mkt_banksus", "FF4", "FF5")
metrics_per_model <- c("Mean", "Median", "Std_Dev", "Pos", "BMP", "Corrado")
metrics_tests <- c("Mean", "Median", "t_test", "MWU")

# Initialize lists for the training results
beta_assets_market_model_sp500us <- list()
beta_assets_market_model_banksus <- list()
beta_assets_4_factor <- list()
beta_assets_5_factor <- list()
estimation_start_end <- list()
event_start_end <- list()
training_set_list <- list()
testing_set_list <- list()

returns_all <- as.data.frame(matrix(0, nrow = 291, ncol = 1), stringsAsFactors = F)
returns_all[,1] <- (-270):20
colnames(returns_all) <- "Date"

ret <- 0 

# Train a model for each announcement (per company and announcement date
#                                     - each company has more than 1 announcement dates)
for (id in 1:length(companies)){
  beta_assets_market_model_sp500us[[id]] <- list()
  beta_assets_market_model_banksus[[id]] <- list()
  beta_assets_4_factor[[id]] <- list()
  beta_assets_5_factor[[id]] <- list()
  
  # Find announcement dates of company-id
  ann <- which(announcements$ID == companies[id])
  j <- 0
  names_list <- c()
  
  # For each announcement of company-id
  for (a in ann){
    ret <- ret + 1
    training_ids <- which(returns$Date==announcements$DATE[a])
    #if the announcement is on Sunday 
    if (length(training_ids) == 0){ 
      training_ids <- which(returns$Date==(announcements$DATE[a] + 1))
    }
    #if the announcement is on Satarday
    if (length(training_ids) == 0){
      training_ids <- which(returns$Date==(announcements$DATE[a] + 2))
    }
    if (length(training_ids) == 0){
      print(paste0("Not valid announcement date for asset ", companies[id], " and announcement date ", announcements$DATE[a]))
      next
    }
    #if ((training_ids + 259) <= nrow(returns)){
      #training_ids <- training_ids:(training_ids + 259)
    #}else{
      #training_ids <- training_ids:nrow(returns)
    #}
    training_ids <- (training_ids - 270):(training_ids - 21)
    training_set <- returns[training_ids,c(1,(id+1))]
    colnames(training_set)[2] <- "returns"
    
    #if (length(which(is.na(training_set$returns))) > 0){
    #  print(paste0("Missing values for asset ", companies[id], " and announcement date ", announcements$DATE[a], ": ",
    #               length(which(is.na(training_set$returns))), " missing values."))
    #}
    
    # Model training 
    j <- j +1
    estimation_start_end[[ret]] <- training_set$Date
    training_set_list[[ret]] <- training_set
    testing_set <- returns[(max(training_ids) + 1):(max(training_ids) + 41),c(1,(id+1))]
    colnames(testing_set)[2] <- "returns"
    testing_set_list[[ret]] <- testing_set
    event_start_end[[ret]] <- testing_set$Date
    returns_all$temp <- c(training_set$returns, testing_set$returns)
    colnames(returns_all)[(ret+1)] <- paste0(ret, " ", companies[id], " ", announcements$DATE[a])
    
    # Market model data sp500us
    ff_returns <- 
      training_set %>% 
      left_join(marketsp500us, by = "Date") %>% 
      mutate(R_excess = round(returns, 4))
    # Train the market model
    beta_assets_market_model_sp500us[[id]][[j]] <- lm(R_excess ~ Mkt, data = ff_returns)
    
    # Market model data banksus
    ff_returns <- 
      training_set %>% 
      left_join(marketbanksus, by = "Date") %>% 
      mutate(R_excess = round(returns, 4))
    # Train the market model
    beta_assets_market_model_banksus[[id]][[j]] <- lm(R_excess ~ Mkt, data = ff_returns)
    
    # 4 factor model data
    ff_returns <- 
      training_set %>% 
      left_join(Global_4_Factors, by = "Date") %>% 
      mutate(MKT_RF = Mkt-RF,
             R_excess = round(returns - RF, 4))
    # Train the 4 factor model
    beta_assets_4_factor[[id]][[j]] <- lm(R_excess ~ MKT_RF + SMB + HML + MOM, data = ff_returns)
    
    # 5 factor model data
    ff_returns <- 
      training_set %>% 
      left_join(Global_5_Factors, by = "Date") %>% 
      mutate(MKT_RF = Mkt-RF,
             R_excess = round(returns - RF, 4))
    # Train the model
    beta_assets_5_factor[[id]][[j]] <- lm(R_excess ~ MKT_RF + SMB + HML + RMW + CMA, data = ff_returns)
      
    names_list <- c(names_list, a)
  }
  names(beta_assets_market_model_sp500us[[id]]) <- as.character(names_list)
  names(beta_assets_market_model_banksus[[id]]) <- as.character(names_list)
  names(beta_assets_4_factor[[id]]) <- as.character(names_list)
  names(beta_assets_5_factor[[id]]) <- as.character(names_list)
}
write.table(returns_all, "results/returns_initial.csv", sep = ",", row.names = F)


### Create a matrix with all AR values per company in the window [-270,20]

# Market model sp500us - AR for each company 
AR_all_mkt_sp500us <- as.data.frame(matrix(0, nrow = 291, ncol = ncol(returns_all)), stringsAsFactors = F)
AR_all_mkt_sp500us[,1] <- (-270):20
colnames(AR_all_mkt_sp500us) <- colnames(returns_all)
ret <- 0

for (id in 1:length(companies)){
  ann <- which(announcements$ID == companies[id])
  j <- 0
  names_list <- c()
  
  for (a in ann){
    ret <- ret + 1
    j <- j + 1
    test_dates <- as.Date(estimation_start_end[[ret]][1]:(estimation_start_end[[ret]][length(estimation_start_end[[ret]])] + 41))
    index_ff <- which(marketsp500us$Date %in% test_dates)
    AR_all_mkt_sp500us[,(ret+1)] <- returns_all[,(ret+1)] - predict(beta_assets_market_model_sp500us[[id]][[j]],
                                                                    data.frame(Mkt = marketsp500us$Mkt[index_ff]))
  }
}
write.table(AR_all_mkt_sp500us, "results/AR_market_model_sp500us.csv", sep = ",", row.names = F)

# Market model banksus - AR for each company 
AR_all_mkt_banksus <- as.data.frame(matrix(0, nrow = 291, ncol = ncol(returns_all)), stringsAsFactors = F)
AR_all_mkt_banksus[,1] <- (-270):20
colnames(AR_all_mkt_banksus) <- colnames(returns_all)
ret <- 0

for (id in 1:length(companies)){
  ann <- which(announcements$ID == companies[id])
  j <- 0
  names_list <- c()
  
  for (a in ann){
    ret <- ret + 1
    j <- j + 1
    test_dates <- as.Date(estimation_start_end[[ret]][1]:(estimation_start_end[[ret]][length(estimation_start_end[[ret]])] + 41))
    index_ff <- which(marketbanksus$Date %in% test_dates)
    AR_all_mkt_banksus[,(ret+1)] <- returns_all[,(ret+1)] - predict(beta_assets_market_model_banksus[[id]][[j]],
                                                            data.frame(Mkt = marketbanksus$Mkt[index_ff]))
  }
}
write.table(AR_all_mkt_banksus, "results/AR_market_model_banksus.csv", sep = ",", row.names = F)

# 4 factor model AR for each company 
AR_all_4ff <- as.data.frame(matrix(0, nrow = 291, ncol = ncol(returns_all)), stringsAsFactors = F)
AR_all_4ff[,1] <- (-270):20
colnames(AR_all_4ff) <- colnames(returns_all)
ret <- 0

for (id in 1:length(companies)){
  ann <- which(announcements$ID == companies[id])
  j <- 0
  names_list <- c()
  
  for (a in ann){
    ret <- ret + 1
    j <- j + 1
    test_dates <- as.Date(estimation_start_end[[ret]][1]:(estimation_start_end[[ret]][length(estimation_start_end[[ret]])] + 41))
    index_ff <- which(Global_4_Factors$Date %in% test_dates)
    AR_all_4ff[,(ret+1)] <- returns_all[,(ret+1)] - predict(beta_assets_4_factor[[id]][[j]],
                                                            data.frame(MKT_RF = (Global_4_Factors$Mkt[index_ff] - Global_4_Factors$RF[index_ff]), 
                                                                       SMB = Global_4_Factors$SMB[index_ff], HML = Global_4_Factors$HML[index_ff],
                                                                       MOM = Global_4_Factors$MOM[index_ff]))
  }
}
write.table(AR_all_4ff, "results/AR_4ff.csv", sep = ",", row.names = F)

# 5 factor model AR for each company 
AR_all_5ff <- as.data.frame(matrix(0, nrow = 291, ncol = ncol(returns_all)), stringsAsFactors = F)
AR_all_5ff[,1] <- (-270):20
colnames(AR_all_5ff) <- colnames(returns_all)
ret <- 0

for (id in 1:length(companies)){
  ann <- which(announcements$ID == companies[id])
  j <- 0
  names_list <- c()
  
  for (a in ann){
    ret <- ret + 1
    j <- j + 1
    test_dates <- as.Date(estimation_start_end[[ret]][1]:(estimation_start_end[[ret]][length(estimation_start_end[[ret]])] + 41))
    index_ff <- which(Global_5_Factors$Date %in% test_dates)
    AR_all_5ff[,(ret+1)] <- returns_all[,(ret+1)] - predict(beta_assets_5_factor[[id]][[j]],
                                                            data.frame(MKT_RF = (Global_5_Factors$Mkt[index_ff] - Global_5_Factors$RF[index_ff]), 
                                                                       SMB = Global_5_Factors$SMB[index_ff], HML = Global_5_Factors$HML[index_ff],
                                                                       RMW = Global_5_Factors$RMW[index_ff], CMA = Global_5_Factors$CMA[index_ff]))
  }
}
write.table(AR_all_5ff, "results/AR_5ff.csv", sep = ",", row.names = F)



### To obtain more detailed information about the model
# http://www.learnbymarketing.com/tutorials/linear-regression-in-r/
summary(beta_assets_4_factor[[id]][[j]])

# The error term is the 'Residuals' which is assumed to be normally distributed
# A residual error is calculated for each data point of the dataset
# residuals(beta_assets_4_factor[[id]][[j]])

### Create a summary table with CARs
num_of_comb <- (length(models)*length(models) - length(models))/2
CARs_table <- as.data.frame(matrix(0,nrow = (nrow(window_pre) + nrow(window_an) + nrow(window_post)), 
                                   ncol = (1 + length(metrics_per_model)*length(models) + 
                                             length(metrics_tests)*num_of_comb)))
n <- c("Window")

for (i in models){
  n <- c(n,paste0(metrics_per_model,"_",i))
}

for (i in 1:(length(models) - 1)){
  for (j in (i+1):length(models)){
    n <- c(n,paste0(metrics_tests, "_", models[i], "_", models[j]))
  }
}

colnames(CARs_table) <- n

all_windows <- rbind(window_pre, window_an, window_post)
for (w in 1:nrow(all_windows)){
  CARs_table$Window[w] <- paste0("(",all_windows[w,1],"..",all_windows[w,2],")")
}

CAR_predicted <- list()

CAR_predicted[[models[1]]] <- calc_metrics(all_windows, AR_all_mkt_sp500us, marketsp500us)
CAR_predicted[[models[2]]] <- calc_metrics(all_windows, AR_all_mkt_banksus, marketbanksus)
CAR_predicted[[models[3]]] <- calc_metrics(all_windows, AR_all_4ff, Global_4_Factors)
CAR_predicted[[models[4]]] <- calc_metrics(all_windows, AR_all_5ff, Global_5_Factors)

j <- 2
for (m in 1:length(models)){
  CARs_table[,j:(j + length(metrics_per_model) - 1)] <- CAR_predicted[[models[m]]]$CAR_result
  j <- j + length(metrics_per_model)
}

j2 <- j
for (i in 1:(length(models) - 1)){
  for (k in (i+1):length(models)){
    for (w in 1:nrow(all_windows)){
      CARs_table[w,(j2+2)] <- t.test(CAR_predicted[[models[i]]]$CAR_list[[w]], CAR_predicted[[models[k]]]$CAR_list[[w]])$statistic
      CARs_table[w,(j2+3)] <- wilcox.test(CAR_predicted[[models[i]]]$CAR_list[[w]], CAR_predicted[[models[k]]]$CAR_list[[w]])$p.value
    }
    CARs_table[,j2] <- CAR_predicted[[models[i]]]$CAR_result$Mean - CAR_predicted[[models[k]]]$CAR_result$Mean
    CARs_table[,(j2+1)] <- CAR_predicted[[models[i]]]$CAR_result$Median - CAR_predicted[[models[k]]]$CAR_result$Median
    j2 <- j2 + length(metrics_tests)
  }
}

write.table(CARs_table, "results/CARs_summary_table.csv", sep = ",", row.names = F)
