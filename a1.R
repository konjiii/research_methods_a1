# first plot code
# questionable practice
# vector to store the p values
pvalue_list <- c()

# set seed
set.seed(1)

# vector with the iq's
iq_nl <- rnorm(1, 101, 15)
iq_fi <- rnorm(1, 101, 15)

# loop infinitely
repeat {
  # add a new test result every loop
  iq_nl <- append(iq_nl, rnorm(1, 101, 15))
  iq_fi <- append(iq_fi, rnorm(1, 101, 15))
  
  # perform the t-test
  out <- t.test(iq_nl, iq_fi)
  # append current p value to pvalue_list
  pvalue_list <- append(pvalue_list, out$p.value)
  # if the p value is under 0.05 stop the test
  if (out$p.value < 0.05){
    break
  }
}

# plot the p values
plot(2:(length(pvalue_list)+1), pvalue_list, type = "l")

# second plot code
# showing the first plot is wrong
# vector to store the p values
pvalue_list <- c()

# set seed
set.seed(1)

# vector with the iq's
iq_nl <- rnorm(1, 101, 15)
iq_fi <- rnorm(1, 101, 15)

# loop infinitely
for (i in 1:6000) {
  # add a new test result every loop
  iq_nl <- append(iq_nl, rnorm(1, 101, 15))
  iq_fi <- append(iq_fi, rnorm(1, 101, 15))
  
  # perform the t-test
  out <- t.test(iq_nl, iq_fi)
  # append current p value to pvalue_list
  pvalue_list <- append(pvalue_list, out$p.value)
}

# plot the p values
plot(2:(length(pvalue_list)+1), pvalue_list, type = "l")

