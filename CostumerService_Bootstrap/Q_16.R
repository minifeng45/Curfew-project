#### Single run function for customer service ####
single_run = function(customer_number){
  ### define r.v:
  # time_spend = how much time a customer spend on telephone
  # total_spend = how much time the service operation
  time_spend = c() # a vector for 1st, 2nd, 3rd... customer
  total_spend = 0 # start from 0 hour
  # every customer enter a Poisson dist. and is given the server situation 
  serve_situation = rpois(customer_number, 4) 
  for (i in 1:customer_number) { # for each customer in order
    if (serve_situation[i] <= 3){ # if there are equal or fewer than 3, a costumer can enter the service
      # a customer spends time on telephone following a exponential dist. with rate = 4.2
      time_spend[i] = rexp(1,rate = 4.2) 
      total_spend = total_spend+ time_spend[i] # each customer time would be added into the total time that service spends
      if (total_spend > 8){ # if service time exceeds 8, the service ends
        break
      }
    }
  }
  avr = mean(time_spend,na.rm = T) # calculate the mean of time spend of all customers
  
  return(avr)
}


#### Run bootstrap to get the distribution ####
## Test the service for 1000 times
avr_timespend = c()
for (i in 1:1000) {
  avr_timespend[i] = single_run(1000) #customer_number=1000, which means 1000 people want to use the service today
}

## bootstrapping: hand-written

y_actual = mean(avr_timespend)
## bootstrap: sampling with replacement
# resample 100 times with replacement
residual= c()
for (i in 1:100) { # resample 100 times 
  y_pred  = sample(avr_timespend, 1, replace = TRUE)
  residual[i] = (y_actual - y_pred) # calculate MSE
}


MSE = mean(residual^2)
std_error = sd(residual^2)
bias = mean(residual^2) - median(residual^2)

Bootstrap_Statistics = list(MSE =MSE, Bias = bias, std_error = std_error)
Bootstrap_Statistics


## bootstrapping: package 
# Load the library
library(boot)

# Creating a function to pass into boot() function
bootFunc <- function(data, i){
  df <- data[i]
  sum((mean(data) - df)^2)/length(df) #MSE
}

boot(avr_timespend , bootFunc, R = 100) #average time spend is put into bootstrap and resample for 100 times, calculating MSE in each
