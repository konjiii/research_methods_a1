
timestep <- 0
while (timestep != 10000){ 
  # first plot code
  # questionable practice
  # vector to store the p values
  pvalue_list <- c()
  
  # set seed
  set.seed(1)
  max_sample <- 300 #maximum size of sample per population
  min_sample <- 80  #starting sample must be practical so lets say we pull 80 people per population
  # vector with the heights
  height_nl <- rnorm(min_sample, 177, 10) #the std for nl/mne is 9.7cm rounded up to 10cm
  height_mne <- rnorm(min_sample, 177, 10) #the mean for nl is 177.1cm and for mne is 176.6 so rounded for both is 177cm
    
    # loop infinitely
    repeat {
      # add a new test result every loop
      height_nl <- append(height_nl, rnorm(1, 177, 10))
      height_mne <- append(height_mne, rnorm(1, 177, 10))
      
      
      # perform the t-test
      out <- t.test(height_nl, height_mne)
      # append current p value to pvalue_list
      pvalue_list <- append(pvalue_list, out$p.value)
      # if the p value is under 0.05 stop the test
      if ((out$p.value < 0.05)||(length(height_mne) == max_sample)) {
        break
      }
    }
  
  # plot the p values
  plot(2:(length(pvalue_list)+1), pvalue_list, type = "l")
  timestep <- timestep + 1
}
  
  
  
  
title(main = "Sequential tests", sub = "Sub-title",
      xlab = "X axis", ylab = "Y axis",
      cex.main = 2,   font.main= 4, col.main= "red",
      cex.sub = 0.75, font.sub = 3, col.sub = "green",
      col.lab ="darkblue"
)
abline(h=0.05, col = "red", lty=2) #setting the p-value boundary








# second plot code
# showing the first plot is wrong
# vector to store the p values
pvalue_list <- c()

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
  # append current p value to pvalue_list
  pvalue_list <- append(pvalue_list, out$p.value)
}

# plot the p values
plot(2:(length(pvalue_list)+1), pvalue_list, type = "l")
abline(h=0.05, col = "red", lty=2) #setting the p-value boundary

#third plot code
#sequential testing and rounding down p-values

pvalue_list <- c()
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
  # append current p value to pvalue_list
  pvalue_list <- append(pvalue_list,round_out)
  # if the p value is under 0.05 stop the test
  if (round_out < 0.05){
    break
  }
}
# plot the p values
plot(2:(length(pvalue_list)+1), pvalue_list, type = "l")
abline(h=0.05, col = "red", lty=2) #setting the p-value boundary



