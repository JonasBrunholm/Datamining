library(tidyverse)
library(snow)
library(neuralnet)
library(Metrics)
library(sigmoid)

#cl <- makeCluster(10, type = "SOCK")

#################### Black - Scholes ####################################
d1 <- function(x,k,r,sivma,T_,t){
  (log(x/k)+(r-sivma^2/2)*(T_-t))/(sivma*sqrt(T_-t))
}

d2 <- function(x,k,r,sivma,T_,t){
  d1(x,k,r,sivma,T_,t) + sivma*sqrt(T_-t)
}

# price_bs <- function(x,k,r,sivma,T_,t,S){
#   S*pnorm(d1(x,k,r,sivma,T_,t))-exp(-r*(T_-t))*k*pnorm(d2(x,k,r,sivma,T_,t))
# }
# 
# price_bs <- function(data){
#   data[,7]*pnorm(d1(data[,1],data[,2],data[,3],data[,4],data[,5],data[,6]))-
#   exp(-data[,3]*(data[,5]-data[,6]))*data[,2]*pnorm(d2(data[,1],data[,2],data[,3],data[,4],data[,5],data[,6]))
# }
# 
# test2 <- data.frame(x=c(1,1),k=c(0.5,0.5),r=c(1,1),sivma=c(1,1),T_=c(0.2,0.),t=c(0.1,0.1),S=c(1,1))
# price_bs(test2)
price_bs_fixed <- function(data){
  1*pnorm(d2(1,data[,1],0.2,data[,2],data[,3],0))-
    exp(-0.2*(data[,3]-0))*data[,1]*
    pnorm(d1(1,data[,1],0.2,data[,2],data[,3],0))
}

# test3 <- data.frame(K=c(0.5,2),sivma = c(1,1),T_ = c(0.1,0.1))
# 
# price_bs_fixed(test3)
# 
# Strike_Prices <- seq(from = 0.5, to = 2, by = 0.1)
# Final_Times <- seq(from = 0.1, to = 1, by = 0.1)
# sivma <- seq(from = 0.1, to = 1, by = 0.1)

grid <- expand.grid(K = seq(from = 0.5, to = 2, by = 0.1),
                    sivma = seq(from = 0.1, to = 1, by = 0.1),
                    T_ = seq(from = 0.02, to = 1, by = 0.02))

black_scoles_actual <- tibble(price = price_bs_fixed(grid), grid)
JJB_black_scholes <- round(black_scoles_actual,3)

write.csv(JJB_black_scholes , 
          file = "C:\\8 Semester\\Projekt\\P8\\JJB_black_scholes.csv",
          row.names = FALSE)

#################### Black - scholes - NN ####################################

index <- sample(1:nrow(JJB_black_scholes),round(0.75*nrow(JJB_black_scholes)))

sample_75 <- JJB_black_scholes[index,]
sample_25 <- JJB_black_scholes[-index,]

nn <- neuralnet(price ~ K + sivma + T_, data = sample_75, 
                hidden=c(33,33,33),act.fct = "logistic", linear.output = TRUE)
#plot(nn)
rmse(nn$response,nn$net.result[[1]])

compute_25 <- neuralnet::compute(nn,sample_25[,-1])

rmse(sample_25$price,compute_25$net.result)

resid <- function(sivma){
  sum_sq <- 0
  sample_25$sivma <- sivma
  for (i in 1:nrow(sample_25)) {
    sum_sq <- sum_sq + 
      (as.numeric(compute(nn, sample_25[i,])$net.result) - sample_25$price[i])^2
  }
  return(sum_sq)
}

optim(par = c(1), fn = resid)



















