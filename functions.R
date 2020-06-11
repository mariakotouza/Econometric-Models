# Std_Dev, Pos metrics: Standard deviation and percentage of firms with positive CARs
calc_metrics <- function(window, AR, market){
  CAR_result <- as.data.frame(matrix(0, nrow = nrow(window), ncol = length(metrics_per_model)))
  colnames(CAR_result) <- metrics_per_model
  CAR_list <- list()
  
  for (i in 1:nrow(window)){
    CAR <- c()
    SCAR <- c()
    
    index <- which(AR$Date %in% (window[i,1]:window[i,2]))
    CAR <- as.vector(colSums(AR[index,2:ncol(AR)]))
    
    CAR_list[[i]] <- CAR
    CAR_result$Mean[i] <- mean(CAR, na.rm = T)
    CAR_result$Median[i] <- median(CAR, na.rm = T)
    CAR_result$Std_Dev[i] <- sd(CAR[which(CAR > 0)])
    CAR_result$Pos[i] <- (length(which(CAR > 0))/length(CAR)) * 100
    CAR_result$BMP[i] <- boehmer(CAR, estimation_start_end, event_start_end, estimation_ids, AR, window[i,], market)
    CAR_result$Corrado[i] <- mean(corrado_sign_test_AR(estimation_start_end, event_start_end, AR, window[i,])$csign_stat)
  }
  
  result = list("CAR_result" = CAR_result,"CAR_list" = CAR_list)
  
  return(result)
  
}


#https://github.com/cran/estudy2/blob/master/R/parametric_tests.R

boehmer <- function(CAR, estimation_start_end, event_start_end, estimation_ids, AR_all_model, window, market) {
  SCAR <- vector(length = length(CAR))
  # delta <- numeric(length(list_of_returns))
  for(i in seq_along(estimation_start_end)) {
    t <- event_start_end[[i]] 
    names(t) <- min(window_pre):max(window_post)
    
    event_start <- t[names(t) %in% window[1]]
    event_end <- t[names(t) %in% window[2]]
    
    company_estimation_abnormal <- zoo::as.zoo(AR_all_model[AR_all_model$Date %in% estimation_ids,(i+1)])
    
    #company_event_abnormal <- zoo::as.zoo(AR_all_model[AR_all_model$Date %in% window[1]:window[2],(i+1)])
    
    # The market's returns
    market_estimation <- zoo::as.zoo(market$Mkt[which(market$Date %in% estimation_start_end[[i]])])
    market_event <- zoo::as.zoo(market$Mkt[which(market$Date %in% event_start_end[[i]])])
    
    mean_market_estimation <- mean(market_estimation, na.rm = TRUE)
    
    L1 <- length(which(!is.nan(market_estimation)))
    L2 <- length(which(!is.nan(market_event)))
    
    SCAR[i] <- CAR[i]/(stats::sd(company_estimation_abnormal, na.rm = TRUE) * 
                       sqrt(L2 + L2^2/L1 + 
                              sum((market_event - mean_market_estimation) ^ 2, na.rm = TRUE)/
                              sum((market_estimation - mean_market_estimation) ^ 2, na.rm = TRUE)))
  }
  
  N <- length(CAR)
  mean_SCAR <- mean(SCAR, na.rm = TRUE)
  sd_mean_SCAR <- sqrt(1/(N-1) * sum((SCAR - mean_SCAR) ^ 2, na.rm = TRUE))
  
  statistics <- sqrt(N) * mean_SCAR/sd_mean_SCAR
  
  return(statistics)
}

corrado_sign_test_AR <- function(estimation_start_end, event_start_end, AR_all_model, window) {
  const_q1 <- 0.1
  const_q2 <- 0.05
  const_q3 <- 0.01
  
  # zoo objects of signs
  event_sign <- NULL
  full_sign <- NULL
  delta_full <- numeric(length(estimation_start_end))
  for(i in seq_along(estimation_start_end)) {
    t <- event_start_end[[i]] 
    names(t) <- min(window_pre):max(window_post)
    
    event_start <- t[names(t) %in% window[1]]
    event_end <- t[names(t) %in% window[2]]
    
    if(estimation_start_end[[i]][length(estimation_start_end[[i]])] >= event_start) {
      stop(paste0("For ", as.character(i), "-th company estimation",
                  " period overlaps with event period."))
    }
    company_full_abnormal <- zoo::as.zoo(AR_all_model[,(i+1)])
    
    company_event_abnormal <- zoo::as.zoo(AR_all_model[(AR_all_model$Date %in% (window[1]:window[2])),(i+1)])
    
    company_median <- stats::median(zoo::coredata(company_full_abnormal), na.rm = TRUE)
    company_full_sign <- sign(company_full_abnormal - company_median)
    company_event_sign <- sign(company_event_abnormal - company_median)
    
    if(is.null(full_sign)) {
      full_sign <- company_full_sign
    } else {
      full_sign <- merge(full_sign, company_full_sign, all = TRUE)
    }
    
    if(is.null(event_sign)) {
      event_sign <- company_event_sign
    } else {
      event_sign <- merge(event_sign, company_event_sign, all = TRUE)
    }
    
    delta_full[i] <-
      length(company_full_abnormal[!is.na(company_full_abnormal)])
    
  }
  event_number_of_companies <- rowSums(!is.na(event_sign))
  result <- data.frame(date = zoo::index(event_sign),
                       percentage = event_number_of_companies /
                         ncol(event_sign) * 100)
  
  full_sign <- as.matrix(full_sign)
  event_sign <- as.matrix(event_sign)
  number_of_companies <- rowSums(!is.na(full_sign))
  number_of_companies[number_of_companies == 0] <- NA
  full_sign_sums <- rowMeans(full_sign, na.rm = TRUE) * ncol(full_sign)
  full_sign_sums[is.nan(full_sign_sums)] <- NA
  sd_full <- sqrt(1 / mean(delta_full, na.rm = TRUE) *
                    sum((1 / sqrt(number_of_companies) * full_sign_sums)^2, na.rm = TRUE))
  
  event_sign_sums <- rowMeans(event_sign, na.rm = TRUE) * ncol(event_sign)
  event_sign_sums[is.nan(event_sign_sums)] <- NA
  event_number_of_companies[event_number_of_companies == 0] <- NA
  statistics <- 1 / sqrt(event_number_of_companies) *
    event_sign_sums / sd_full
  statistics[is.nan(statistics)] <- NA
  significance <- rep("", length(statistics))
  significance[abs(statistics) >= const_q1] <- "*"
  significance[abs(statistics) >= const_q2] <- "**"
  significance[abs(statistics) >= const_q3] <- "***"
  result <- cbind(result, data.frame(csign_stat = statistics,
                                     csign_signif = significance))
  rownames(result) <- NULL
  return(result)
}

