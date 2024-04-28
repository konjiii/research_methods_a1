# first plot code
# questionable practice
# vector to store the p values
p-values <- c()

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
  # append current p value to p-values
  p-values <- append(p-values, out$p.value)
  # if the p value is under 0.05 stop the test
  if (out$p.value < 0.05){
    break
  }
}

# plot the p values
sample_size <- 2:(length(p-values)+1)
plot(sample_size, p-values, type = "l")
abline(h=0.05, col = "red", lty=2) #setting the p-value boundary
title(main="p-values per sample size ")

# second plot code
# showing the first plot is wrong
# vector to store the p values
p-values <- c()

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
  # append current p value to p-values
  p-values <- append(p-values, out$p.value)
}

# plot the p values
sample_size <- 2:(length(p-values)+1)
plot(sample_size, p-values, type = "l")
abline(h=0.05, col = "red", lty=2) #setting the p-value boundary
title(main="")

#third plot code
#sequential testing and rounding down p-values

# set seed
set.seed(1)

p-values <- c()
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
  round_out <- floor(out$p.value * 10)/ 10 #rounds down the p-values of out
  # append current p value to p-values
  p-values <- append(p-values,round_out)
  # if the p value is under 0.05 stop the test
  if (round_out < 0.05){
    break
  }
}
# plot the p values
sample_size <- 2:(length(p-values)+1)
plot(sample_size, p-values, type = "l")
abline(h=0.05, col = "red", lty=2) #setting the p-value boundary


#fourth plot code
#Type one error rate of sequential testing

# set seed
set.seed(1)

# to toggle the rounding down QRP on(TRUE) or off(FALSE)
roundingdown <- TRUE

# init basic vars
samplesize <-1000
type_one_error_rate <- c()
amount_of_sequence_tests <- c()

#for amount of sequence tests
for (g in seq(1, 10000, 100) ){
  #init counters
  type_one_error_counter <- 0
  good_test_counter <- 0
  amount_of_sequence_tests <- append(amount_of_sequence_tests,g)
  
  # loop for amount of tests
  for (i in 1:g){
    
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
      if (roundingdown) {
        round_out <- floor(out$p.value * 10)/ 10 #rounds down the p-values of out
        # if the p value is under 0.05 stop the test
        if (round_out < 0.05){
          type_one_error_counter <- type_one_error_counter + 1
          break
        }
        else if (i == samplesize){
          good_test_counter <- good_test_counter + 1
        }
      }
      # if the p value is under 0.05 stop the test
      else if (out$p.value<0.5){
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

# plot the type one error rates
plot(amount_of_sequence_tests, type_one_error_rate, type = "l")


#fifth plot code
#Type one error rate sequential testing (base case)

# set seed
set.seed(1)

# init basic vars
samplesize <-1000
type_one_error_rate <- c()
amount_of_sequence_tests <- c()

#for amount of sequence tests
for (g in seq(1, 10000, 100) ){
  #init counters
  type_one_error_counter <- 0
  good_test_counter <- 0
  amount_of_sequence_tests <- append(amount_of_sequence_tests,g)
  
  # loop for amount of tests
  for (i in 1:g){
    
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
      if (i == samplesize && out$p.value<0.5){
        good_test_counter <- good_test_counter + 1
      }
    }
    
  }
  #calculate error_rate
  error_rate <- type_one_error_counter/(type_one_error_counter+good_test_counter)
  # append type one error rate to the list
  type_one_error_rate <- append(type_one_error_rate,error_rate)
}

# plot the type one error rates
plot(amount_of_sequence_tests, type_one_error_rate, type = "l")

#fifth plot code
#Type one error rate sequential testing (base case)

# set seed
set.seed(1)

# init basic vars
samplesize <-1000
type_one_error_rate <- c()
amount_of_sequence_tests <- c()

#for amount of sequence tests
for (g in seq(1, 10000, 100) ){
  #init counters
  type_one_error_counter <- 0
  good_test_counter <- 0
  amount_of_sequence_tests <- append(amount_of_sequence_tests,g)
  
  # loop for amount of tests
  for (i in 1:g){
    
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
      if (i == samplesize && out$p.value>=0.5){
        good_test_counter <- good_test_counter + 1
      }
      else {
        type_one_error_counter <- type_one_error_counter + 1
      }
    }
    
  }
  #calculate error_rate
  error_rate <- type_one_error_counter/(type_one_error_counter+good_test_counter)
  # append type one error rate to the list
  type_one_error_rate <- append(type_one_error_rate,error_rate)
}

# plot the type one error rates
plot(amount_of_sequence_tests, type_one_error_rate, type = "l")

