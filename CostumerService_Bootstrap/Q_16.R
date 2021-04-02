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

y = mean(avr_timespend)
## bootstrap: sampling with replacement
# sample 100 individuals from 1000
sample_number = 10
boot_result  = sample(avr_timespend, sample_number, replace = TRUE)
MSE = sum((y - boot_result)^2)/sample_number


