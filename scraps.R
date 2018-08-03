pay <- function(principal, interest, duration, payfreq, firstpay, compoundfreq) {
  
  r <- interest / (100 * 12 / compoundfreq ) 
  
  if(firstpay > 1) {
    principal <- principal * (1 + r)^((firstpay - 1) / compoundfreq)
    duration <- duration - (firstpay - 1) / 12
  }	
  
  payment <- principal * r / ( 1 - ( 1 + r)^(-duration * 12 / compoundfreq) ) * payfreq / compoundfreq
  res <- list(r=r, payment=payment, principal=principal)
  return(res)
}

## Amortization table
amort <- function(principal, interest, duration, payfreq, firstpay, compoundfreq) {
  pay <- pay(principal, interest, duration, payfreq, firstpay, compoundfreq)
  data <- data.frame(month = seq(0, duration * 12))
  data$payment <- 0
  data$payment[ (data$month - firstpay) >= 0 & (data$month - firstpay) %% payfreq == 0 ] <- pay$payment
  i <- which(data$payment != 0)
  i <- i[length(i)]
  data$payment[ i ] <- 0
  data$payment[ i ] <- pay$payment * (duration - (firstpay - 1) / 12) * 12 / payfreq - sum(data$payment)
  data$totalPayed <- cumsum(data$payment)
  
  data$principal <- NA
  data$principal[1] <- principal
  idx <- (data$month - firstpay) >=0 & (data$month - firstpay) %% compoundfreq == 0
  idx.pr <- which(idx)[-length(idx)] + compoundfreq - 1
  if(any(idx.pr > max(data$month))) {
    idx.pr <- idx.pr[-which(idx.pr > max(data$month))]
  }	
  
  if(firstpay > 1) {
    data$principal[firstpay] <- pay$principal
  }
  data$principal[ idx.pr ] <- (1 + pay$r)^seq_len(length(idx.pr)) * pay$principal - ( (1 + pay$r)^seq_len(length(idx.pr)) - 1 ) / pay$r * pay$payment * compoundfreq / payfreq
  data$principal[ nrow(data)] <- 0
  
  return(data)
}


temp <- amort(250000, 3.75,30,1,0,1)
