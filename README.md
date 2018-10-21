# Assignment-2.4   https://github.com/NeuroPsych/Assignment-2.4.git

# The order of the variables from left to right is: 
# treatment indicator (1 if treated, 0 if not treated), 
# age, education, 
# Black (1 if black, 0 otherwise), 
# Hispanic (1 if Hispanic, 0 otherwise), 
# married (1 if married, 0 otherwise), 
# nodegree (1 if no degree, 0 otherwise), 
# RE75 (earnings in 1975), and RE78 (earnings in 1978). 
# The last variable is the outcome 
# other variables are pre-treatment.
# Import dataset "From STATA" to read properly

nsw 
res3<-glm(re78~treat,data=nsw)
res3confint<-confint(res3) 

# res3$coefficients
set.seed(236)

# create matrix for results
n=10000
bootconfint<-matrix(nrow=n, ncol=2, data=0) 
# for loop
for (k in (1:n)){  
  sampnsw<-sample(1:722,722,replace=TRUE)
  sampnsw<-nsw[sampnsw,]
  sampres<-glm(re78~treat,data=sampnsw) 
  # summary (sampres)
  # confint(sampres)
  bootconfint[k, ]<-sampres$coefficients # populate coefs
}


# histogram for bootconfint results
hist(bootconfint[, 1],
     main="Bootstrap Intercept",
     xlab="Intercept Values ",
     xlim=c(4000,6500),
     col="darkmagenta",
     freq=TRUE
)

hist(bootconfint[, 2],
     main="Bootstrap Regression Coefficient",
     xlab="Treatment Coefficient",
     xlim=c(-1000,2500),
     col="darkmagenta",
     freq=TRUE
)





