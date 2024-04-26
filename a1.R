# first plot code
# questionable practice
# vector to store the p values
pvalue_list <- c()

# set seed
set.seed(1)

# vector with the heights
iq_nl <- rnorm(1, 177, 10) #the std for nl/mne is 9.7cm rounded up to 10cm
iq_mne <- rnorm(1, 177, 10) #the mean for nl is 177.1cm and for mne is 176.6 so rounded for both is 177cm

# loop infinitely
repeat {
  # add a new test result every loop
  iq_nl <- append(iq_nl, rnorm(1, 177, 10))
  iq_mne <- append(iq_mne, rnorm(1, 177, 10))
  
  # perform the t-test
  out <- t.test(iq_nl, iq_mne)
  # append current p value to pvalue_list
  pvalue_list <- append(pvalue_list, out$p.value)
  # if the p value is under 0.05 stop the test
  if (out$p.value < 0.05){
    break
  }
}

# plot the p values
plot(2:(length(pvalue_list)+1), pvalue_list, type = "l")
abline(h=0.05, col = "red", lty=2) #setting the p-value boundary

# second plot code
# showing the first plot is wrong
# vector to store the p values
pvalue_list <- c()

# set seed
set.seed(1)

# vector with the heights
iq_nl <- rnorm(1, 177, 10)
iq_fi <- rnorm(1, 177, 10)

# loop infinitely
for (i in 1:6000) {
  # add a new test result every loop
  iq_nl <- append(iq_nl, rnorm(1, 177, 10))
  iq_fi <- append(iq_mne, rnorm(1, 177, 10))
  
  # perform the t-test
  out <- t.test(iq_nl, iq_mne)
  # append current p value to pvalue_list
  pvalue_list <- append(pvalue_list, out$p.value)
}

# plot the p values
plot(2:(length(pvalue_list)+1), pvalue_list, type = "l")

