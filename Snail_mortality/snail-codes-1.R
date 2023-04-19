

#### Two ways to do binomial/ binary  regressions


### Read data


data = 
  data.frame(temperature = c(38,40,40,40,42,42,42,44,44,44,47,47,47), 
                  alive = c(20,20,20,20,19,14,15,5,3,2,0,0,0), 
                  dead = c(0,0,0,0,1,6,5,15,17,18,20,20,20))



### two column response

y = cbind(data$alive,data$dead)
y




### Logistic regression

logit.y = glm(y~data$temperature,family=binomial)

logit.y = glm(y~data$temperature,family=binomial())
logit.y = glm(y~data$temperature,family=binomial(link="logit"))

logit.y$fit

## Plot


plot(data$temperature, data$alive/20, 
     xlab="temperature", ylab="Fraction survived")

lines(data$temperature, logit.y$fit)


##  One column binary response 

n=260
X=matrix(1,n,2)
X[,2]=kronecker(data$temperature, rep(1,20))
y0=rep(0,n)

w=matrix(0,13,20)
for(i in 1:13) { if(data$alive[i] >0) { w[i,1:data$alive[i]]=1}}
y0=as.numeric(matrix(t(w),n,1))


#### Fit logistic regressions 


logit.y.2 = glm(y0~X[,2], family=binomial)

summary(logit.y)
summary(logit.y.2)

logit.y$coef
V = vcov(logit.y)

#95% CI for beta
c(logit.y$coef[2]-1.96*sqrt(V[2,2]), 
  logit.y$coef[2]+1.96*sqrt(V[2,2]))


# exp(beta)
exp(logit.y$coef)
# 95% CI for exp(beta)
exp(c(logit.y$coef[2]-1.96*sqrt(V[2,2]), logit.y$coef[2]+1.96*sqrt(V[2,2])))

# At temp = 43, parameter theta = alpha + beta*43
theta_hat = logit.y$coef[1]+logit.y$coef[2]*43
exp(theta_hat) # Predicted survival odds 
exp(theta_hat)/(1+exp(theta_hat)) # Predicted survival probability 
se = sqrt(t(c(1,43))%*%V%*%c(1,43))
c(theta_hat-1.96*se, theta_hat+1.96*se)
exp(c(theta_hat-1.96*se, theta_hat+1.96*se)) / (1+exp(c(theta_hat-1.96*se, theta_hat+1.96*se)))





