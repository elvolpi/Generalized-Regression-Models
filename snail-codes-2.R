

#### Two ways to do binomial/ binary  regressions


### Read data


data = 
  data.frame(temperature = c(38,40,40,40,42,42,42,44,44,44,47,47,47), 
             alive = c(20,20,20,20,19,14,15,5,3,2,0,0,0), 
             dead = c(0,0,0,0,1,6,5,15,17,18,20,20,20))



### two column response

y = cbind(data$alive,data$dead)
y


logit.y = glm(y~data$temperature,family=binomial)


##  One column binary response 

n=260
X=matrix(1,n,2)
X[,2]=kronecker(data$temperature, rep(1,20))
y0=rep(0,n)

w=matrix(0,13,20)
for(i in 1:13) { if(data$alive[i] >0) { w[i,1:data$alive[i]]=1}}
y0=as.numeric(matrix(t(w),n,1))


logit.y.2 = glm(y0~X[,2], family=binomial)

summary(logit.y)
summary(logit.y.2)

### fitted probabilities

phat = logit.y$fitted.values
phat.2 = logit.y.2$fitted.values

### AIC 
logit.y$aic
l = sum(log(choose(y[,1]+y[,2],y[,1])) + y[,1]*log(phat)+y[,2]*log(1-phat))
aic.y = 2*2 - 2*l
aic.y

logit.y.2$aic
l2 = sum(y0*log(phat.2)+(1-y0)*log(1-phat.2))
#l2 = sum(y[,1]*log(phat)+y[,2]*log(1-phat))
aic.y.2 = 2*2 - 2*l2

l3 = sum(y[,1]*log(phat)+y[,2]*log(1-phat))

