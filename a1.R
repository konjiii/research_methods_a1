#first plot code
#Type one error rate sequential testing without optional stopping and without rounding down p_values (base case)

# set seed
set.seed(1)

# init basic vars
samplesize <-10000
type_one_error_rate <- c()
amount_of_sequence_tests <- c()

#for amount of sequence tests
for (g in c(seq(1,10,1), 20, 50, 100, 200, 1000)){
  #init counters
  type_one_error_counter <- 0
  good_test_counter <- 0
  amount_of_sequence_tests <- append(amount_of_sequence_tests,g)
  
  # loop for amount of tests
  for (i in 1:g){
    
    print(i)
    
    # vector with the heights
    height_nl <- rnorm(1, 177, 10) #the std for nl/mne is 9.7cm rounded up to 10cm
    height_mne <- rnorm(1, 177, 10) #the mean for nl is 177.1cm and for mne is 176.6 so rounded for both is 177cm
    
    # loop for sample size
    for (i in 1:samplesize) {
      # add a new test result every loop
      height_nl <- append(height_nl, rnorm(1, 177, 10))
      height_mne <- append(height_mne, rnorm(1, 177, 10))
      
      # perform the t-test
      out <- t.test(height_nl, height_mne)
      if (i == samplesize && out$p.value >= 0.05){
        good_test_counter <- good_test_counter + 1
      }
      else if (i == samplesize && out$p.value < 0.05) {
        type_one_error_counter <- type_one_error_counter + 1
      }
    }
    
  }
  #calculate error_rate
  error_rate <- type_one_error_counter/(type_one_error_counter+good_test_counter)
  # append type one error rate to the list
  type_one_error_rate <- append(type_one_error_rate,error_rate)
}
print("error_rate:")
print(error_rate)

# plot the type one error rates
plot(amount_of_sequence_tests, type_one_error_rate, type = "l")
title(main="type one error rate without optional stopping")

#second plot code
#Type one error rate of sequential testing without rounding down p_values

# set seed
set.seed(1)

# to toggle the rounding down QRP on(TRUE) or off(FALSE)
roundingdown <- FALSE

# init basic vars
samplesize <-10000
type_one_error_rate <- c()
amount_of_sequence_tests <- c()

#for amount of sequence tests
for (g in c(seq(1,10,1), 20, 50, 100, 200, 1000)){
  #init counters
  type_one_error_counter <- 0
  good_test_counter <- 0
  amount_of_sequence_tests <- append(amount_of_sequence_tests,g)
  
  # loop for amount of tests
  for (i in 1:g){
    print(i)
    
    # vector with the heights
    height_nl <- rnorm(1, 177, 10) #the std for nl/mne is 9.7cm rounded up to 10cm
    height_mne <- rnorm(1, 177, 10) #the mean for nl is 177.1cm and for mne is 176.6 so rounded for both is 177cm
    
    # loop for sample size
    for (i in 1:samplesize) {
      # add a new test result every loop
      height_nl <- append(height_nl, rnorm(1, 177, 10))
      height_mne <- append(height_mne, rnorm(1, 177, 10))
      
      # perform the t-test
      out <- t.test(height_nl, height_mne)
      # if roundingdown is TRUE round down the p_values with floor(out$p.value *10)/ 10
      round_out <- if (roundingdown) floor(out$p.value * 10)/ 10 else out$p.value
      
      # if the p value is under 0.05 stop the test
      if (round_out < 0.05){
        type_one_error_counter <- type_one_error_counter + 1
        break
      }
      else if (i == samplesize){
        good_test_counter <- good_test_counter + 1
      }
    }
  }
  #calculate error_rate
  error_rate <- type_one_error_counter/(type_one_error_counter+good_test_counter)
  # append type one error rate to the list
  type_one_error_rate <- append(type_one_error_rate,error_rate)
}
print("error_rate:")
print(error_rate)

# plot the type one error rates
plot(amount_of_sequence_tests, type_one_error_rate, type = "l")
title(main="type one error rate with optional stopping")


#third plot code
#Type one error rate of sequential testing with rounding down p_values

# set seed
set.seed(1)

# to toggle the rounding down QRP on(TRUE) or off(FALSE)
roundingdown <- TRUE

# init basic vars
samplesize <-10000
type_one_error_rate <- c()
amount_of_sequence_tests <- c()

#for amount of sequence tests

for (g in c(seq(1,10,1), 20, 50, 100, 200, 1000)){
  #init counters
  type_one_error_counter <- 0
  good_test_counter <- 0
  amount_of_sequence_tests <- append(amount_of_sequence_tests,g)
  
  # loop for amount of tests
  for (i in 1:g){
    print(i)
    
    # vector with the heights
    height_nl <- rnorm(1, 177, 10) #the std for nl/mne is 9.7cm rounded up to 10cm
    height_mne <- rnorm(1, 177, 10) #the mean for nl is 177.1cm and for mne is 176.6 so rounded for both is 177cm
    
    # loop for sample size
    for (i in 1:samplesize) {
      # add a new test result every loop
      height_nl <- append(height_nl, rnorm(1, 177, 10))
      height_mne <- append(height_mne, rnorm(1, 177, 10))
      
      # perform the t-test
      out <- t.test(height_nl, height_mne)
      # if roundingdown is TRUE round down the p_values with floor(out$p.value *10)/ 10
      round_out <- if (roundingdown) floor(out$p.value * 10)/ 10 else out$p.value
      
      # if the p value is under 0.05 stop the test
      if (round_out < 0.05){
        type_one_error_counter <- type_one_error_counter + 1
        break
      }
      else if (i == samplesize){
        good_test_counter <- good_test_counter + 1
      }
    }
  }
  #calculate error_rate
  error_rate <- type_one_error_counter/(type_one_error_counter+good_test_counter)
  # append type one error rate to the list
  type_one_error_rate <- append(type_one_error_rate,error_rate)
}
print("error_rate:")
print(error_rate)

# plot the type one error rates
plot(amount_of_sequence_tests, type_one_error_rate, type = "l")
title(main="type one error rate with optional stopping and rounding down")

# fourth plot code
# sequential testing without optional stopping and without rounding down p_values (base case)

# vector to store the p values
p_values <- c()

# set seed
set.seed(1)

# vector with the heights
height_nl <- rnorm(1, 177, 10)
height_mne <- rnorm(1, 177, 10)

# loop infinitely
for (i in 1:6000) {
  # add a new test result every loop
  height_nl <- append(height_nl, rnorm(1, 177, 10))
  height_mne <- append(height_mne, rnorm(1, 177, 10))
  
  # perform the t-test
  out <- t.test(height_nl, height_mne)
  # append current p value to p_values
  p_values <- append(p_values, out$p.value)
}

# plot the p values
sample_size <- 2:(length(p_values)+1)
plot(sample_size, p_values, type = "l")
abline(h=0.05, col = "red", lty=2) #setting the p-value boundary
title(main="p_values per sample size without optional stopping")

# fifth plot code
# sequential testing with optional stopping and without rounding down p_values

# vector to store the p values
p_values <- c()

# set seed
set.seed(1)

# vector with the heights
height_nl <- rnorm(1, 177, 10) #the std for nl/mne is 9.7cm rounded up to 10cm
height_mne <- rnorm(1, 177, 10) #the mean for nl is 177.1cm and for mne is 176.6 so rounded for both is 177cm

# loop infinitely
repeat {
  # add a new test result every loop
  height_nl <- append(height_nl, rnorm(1, 177, 10))
  height_mne <- append(height_mne, rnorm(1, 177, 10))
  
  # perform the t-test
  out <- t.test(height_nl, height_mne)
  # append current p value to p_values
  p_values <- append(p_values, out$p.value)
  # if the p value is under 0.05 stop the test
  if (out$p.value < 0.05){
    break
  }
}

# plot the p values
sample_size <- 2:(length(p_values)+1)
plot(sample_size, p_values, type = "l")
abline(h=0.05, col = "red", lty=2) #setting the p-value boundary
title(main="p_values per sample size with optional stopping")

#sixth plot code
#sequential testing with optional stopping and rounding down p_values

# set seed
set.seed(1)

p_values <- c()
# vector with the heights
height_nl <- rnorm(1, 177, 10) #the std for nl/mne is 9.7cm rounded up to 10cm
height_mne <- rnorm(1, 177, 10) #the mean for nl is 177.1cm and for mne is 176.6 so rounded for both is 177cm

# loop infinitely
repeat {
  # add a new test result every loop
  height_nl <- append(height_nl, rnorm(1, 177, 10))
  height_mne <- append(height_mne, rnorm(1, 177, 10))
  
  # perform the t-test
  out <- t.test(height_nl, height_mne)
  round_out <- floor(out$p.value * 10)/ 10 #rounds down the p_values of out
  # append current p value to p_values
  p_values <- append(p_values,round_out)
  # if the p value is under 0.05 stop the test
  if (round_out < 0.05){
    break
  }
}
# plot the p values
sample_size <- 2:(length(p_values)+1)
plot(sample_size, p_values, type = "l")
abline(h=0.05, col = "red", lty=2) #setting the p-value boundary
title(main="p_values per sample size with optional stopping and rounding down")