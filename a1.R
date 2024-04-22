max_sample <- 10000
pvalue_list <- c()

set.seed(1)
for (i in 10:max_sample) {
  iq_nl <- rnorm(i, 101, 1)
  iq_fi <- rnorm(i, 101, 1)
  
  out <- t.test(iq_nl, iq_fi)
  pvalue_list <- append(pvalue_list, out$p.value)
  if (out$p.value < 0.05){
    break
  }
}

print(out)
print(pvalue_list)

plot(1:length(pvalue_list), pvalue_list)
