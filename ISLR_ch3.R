## Lab: Simple Linear Regression
library(MASS)
library(ISLR)
?fix
?names
fix(Boston)
names(Boston)
?lm
?lm.fit
lm.fit=lm(medv~lstat,data=Boston)
attach(Boston)
lm.fit=lm(medv~lstat)
lm.fit
summary(lm.fit) ## Why does it compute an F stat for SLR? I thought that was only useful for MR. Also, go over interpretation of these results.
names(lm.fit)
coef(lm.fit)
confint(lm.fit)
predict(lm.fit,data.frame(lstat=c(5,10,15)),
        interval="confidence") 
predict(lm.fit,data.frame(lstat=c(5,10,15)),
        interval="prediction")  ## Let's go over the differences between these.
?abline
plot(lstat,medv)
abline(lm.fit)
abline(lm.fit,lwd=3)
abline(lm.fit,lwd=3,col="red")
plot(lstat,medv,col="red")
plot(lstat,medv,pch="+")
?pch
plot(1:20,1:20,pch=1:20)
par(mfrow=c(2,2))
plot(lm.fit)  
plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit),rstudent(lm.fit)) ## Can we go over interpretting these?
plot(hatvalues(lm.fit)) ## I don't understand what happened here.
which.max(hatvalues(lm.fit)) 

## Lab: Multiple Linear Regression
lm.fit=lm(medv~lstat+age, data=Boston)
summary(lm.fit)
lm.fit=lm(medv~., data=Boston)
summary(lm.fit)
summary(lm.fit)$r.sq
summary(lm.fit)$sigma
library(car)
vif(lm.fit)
lm.fit1=lm(medv~.-age, data=Boston)
summary(lm.fit1)

# Lab: Interaction Terms 
summary(lm(medv~lstat*age, data=Boston)) # Why don't I have to run this new version first, before getting the summary?
# Also, why would we think there was synergy with lstat and age, since age showed up as non-sig?
# Also, examine this F-stat.

# Lab: Non-linear Transformations of the Predictors
lm.fit2=lm(medv~lstat+I(lstat^2))
summary(lm.fit2)
lm.fit=lm(medv~lstat)
anova(lm.fit,lm.fit2) # Cool. I get what this is doing but how do we interpret a squared lstat in real life, to talk about this non-linear prediction?
par(mfrow=c(2,2))
plot(lm.fit2) # Let's interpret these plots again...
lm.fit5=lm(medv~poly(lstat,5))
summary(lm.fit5) #Cool. Let's also interpret what different orders of lstat mean.
summary(lm(medv~log(rm), data=Boston)) 

# Lab: Qualitative Predictors
fix(Carseats)
names(Carseats)
?Carseats
lm.fit=lm(Sales~.+Income:Advertising+Price:Age, data=Carseats)
summary(lm.fit)
contrasts(ShelveLoc, data=Carseats) #I tried this as an alternative to attaching. Why didn't it work?
attach(Carseats) #Why don't we see ShelveLocBad in the summary?
contrasts(ShelveLoc) 

# Exercise 8
library(ISLR)
Auto=read.table("Auto.data", header=T, na.strings="?")
Auto=na.omit(Auto)
lm.fit=lm(mpg~horsepower, data=Auto)
summary(lm.fit) # There IS a relationship (low p-value), and it's strong. The R2 is about 61%. The relationship is negative. 
predict(lm.fit, data.frame(horsepower=98), interval="confidence") # 24 mpg. Lower=24, Upper=25. This is weird though, since there's a negative relationship. Shouldn't mpg be much higher?
predict(lm.fit, data.frame(horsepower=98), interval="prediction") # Lower=15, Upper=34
plot(mpg ~ horsepower, data=Auto) # plot(horesepower,mpg) did not work so I looked it up and tried this. It worked but I got a message that said "HOW_BACKTRACE environmental variable."
abline(lm.fit)
par(mfrow=c(2,2))
plot(lm.fit) # Let's talk about these. For the first one, the relationship is probably non-linear, since there is a clear pattern with the residuals vs. fitted.

# Exercise 9
plot(Auto)
pairs(Auto)
names(Auto)
cor(Auto[c(1:8)]) # Can we go through and interpret these together?
lim.fit9=lm(mpg~.-name, data=Auto)
summary(lim.fit9) # Weight, year, and origin (and displacement to a lesser extent) are significant. Each year, the mpg increases by .75. (This isn't 75%, right? That seems high...)
plot(lim.fit9) # There is a clear pattern in the residuals v fitted, so it suggests the shape is actually non-linear. Let's go over the rest...
lm.fit9e=lm(mpg~.-name +year*weight, data=Auto)
summary(lm.fit9e) # Hmmm. The Y is so much higher here. Did I do something wrong?
lm.fit10e=lm(mpg~.-name +origin*year, data=Auto)
summary(lm.fit10e) # Okay, I must have done something wrong. Y is so different again. Anyways, year and weight are significant. Year and origin are a little less so but still significant. But origin becomes a little less significant when adding this interaction. 
lm.fit9f=lm(mpg~.-name +weight*year +I(origin^2), data=Auto)
summary(lm.fit9f) # Is it appropriate to do the interaction AND the transformation like this? Or should I be just regressing mpg on the one variable and its square? Anyways, when squaring origin, origin is still significant, suggesting the form is non-linear, it's also making horsepower be a little more significant - that's another indicator, right?
# So in my practice file, I squared horsepower (not origin, that didn't make sense when I looked at what it meant) and weirdly horsepower suddenly became significant (in addition to horsepower^2). That's not right, is it?
anova(lm.fit9e, lm.fit9f) #It's showing that Model 2 (the transformed "origin") is a better fit. IRL what does this mean we're doing...
plot(lm.fit9f) # Let's go over this. Also, is there a convenient way to compare residual charts without having to hit the back arrow in the plots window and guess which one it is? 
lm.fit9f5=lm(mpg~.-name +origin*year +poly(origin,3), data=Auto) # This didn't work...
lm.fit9flog=lm(mpg~.-name +weight*year +log(origin), data=Auto)
summary(lm.fit9flog) # Since the log of origin is significant, we're improving the model. 

# Exercise 10
library(ISLR)
summary(Carseats)
lm.fit10a=lm(Sales~ Price + Urban + US, data=Carseats)
summary(lm.fit10a)
?Carseats # The significant variables here are: the price the company charges for each carseat at each site and whether the store is in the US. For each one unit increase in price at a location, the sales will drop by $5,400 at that location. Sales are also positively correlated with whether a store is in the US. Can I say more about that? For every store in the U.S. sales are $1200 more.
#10C: sales = 13.04 - 0.05p - 0.02u + 1.20US
lm.fit10d=lm(Sales~., data=Carseats)
summary(lm.fit10d) # We can reject the null hypothesis for: CompPrice, Income, Advertising, Price, Good and Medium Shelf Locations, and Age. (What about bad shelf locations?)
lm.fit10e=lm(Sales~. -Population -Education -Urban -US, data=Carseats)
summary(lm.fit10e)
#10f: Model A has a .62% RSE (low, which is good)(or maybe I should be just saying "2.472 on 396 dof?) while Model E has a .26% RSE (which is also low, good)(or 1.019 on 392 dof). The R2 is low (.23) for Model A and high (.87) for Model E, so in Model E a larger proportion of the variability is explained by the regression.
anova(lm.fit10a,lm.fit10e) # See "practice" too. I think the anova shows that Model E is a better fit?
confint(lm.fit10e) # Okay, but interpret? The ranges seem okay, right?
plot(lm.fit10e)
par(mfrow=c(2,2)) #How do I run these together? And yes, there seem to be some outliers.

# Exercise #11
set.seed(1)
x=rnorm(100)
y=2*x+rnorm(100) # What is an example of this, what does it look like?
lm.fit11=lm(y~x+0)
summary(lm.fit11) # Coefficient estimate: 1.9939, T-stat: 18.73, p-value: <2e-16 ***
# I don't actually know what the coef means here! And the t-stat is the slope/SE, but what does it tell us, on its own? I DO know it's significant.
# OLD ANSWER (I went over all this again): For each 1 unit increase in X, Y will increase by 1.99 (the slope). The mean of X differs from the actual mean of X by .11. There are 18.73 standard devs away from 0. X is significant. 
lm.fit11b=lm(x~y+0)
summary(lm.fit11b) # Coefficient estimate: 0.39111, SE: 0.02089, T-stat: 18.73, p-value: <2e-16 ***
# For each 1 unit increase in Y, X will increase .39. The mean of Y differs from the actual mean of Y by .02. The number of SDs the estimate of Y is away from 0 is 18.73. And the relationship is significant. 
# It makes sense that the # of standard deviations away from 0 of both x and y are the same because it's a normal distribution. I don't know what else to say about them.
# d:
.39111/.02089 # Equals the t-stat...
# e: I can't :(
#f
lm.fit11f1=lm(y~x)
lm.fit11f2=lm(x~y)
summary(lm.fit11f1)
summary(lm.fit11f2)

# Exercise 12
#12A: I don't know. Come back to this. 

#Exercise 13
set.seed(1) 
x = rnorm(100)
eps = rnorm(100, sd = sqrt(0.25))
y = -1 + 0.5 * x + eps 
length(y) # 100 (and -1, 0.5)
plot(x,y) # Seems linear. RPub says "with some noise intro'd by eps variable." How do I see this?
lm.fit13e = lm(y~x)
summary(lm.fit13e) # The estimates are very close to the true values of the coefficients. The relationship is significant.
plot(x,y)
abline(lm.fit13e, col="red")
abline(-1,0.5, col="green")
legend("topleft", c("Least Squares Line","Pop Regression Line"), col=c("red","green"), lty = c(1,1))
lm.fit13g = lm(y~x + I(x^2))
summary(lm.fit13g) 
anova(lm.fit13e,lm.fit13g) # The quadratic model does not seem to improve the fit. The x2 is not significant and the anova isn't either.

#h
set.seed(1)
x = rnorm(100)
eps = rnorm(100, sd = 0.125)
y = -1 + 0.5 * x + eps
length(y)
plot(x,y)
lm.fit13e2 = lm(y~x)
summary(lm.fit13e2) # Still similar coefs, smaller SEs, much different t-stats, and is still significant.
plot(x,y)
abline(lm.fit13e2, col = "red")
abline(-1, 0.5, col = "blue") # For some reason, this looks terrible...
legend("topleft", c("Least Squares", "Regression"), col=c("red", "blue"), lty=c(1,1))

#i
set.seed(1)
x=rnorm(100)
eps = rnorm(100, sd = .725)
y = -1 + 0.5 * x + eps
plot(x,y)
lm.fit13i = lm(y~x)
summary(lm.fit13i) # Still similar coefs, still significant. Higher RSE and lower R2 (so, less of a good fit)
plot(x,y) # Same as above, looks really weird (maybe just a glitch?)
abline(lm.fit13i, col="red")
abline(x,y, col="orange")
legend("topleft", c("Least Squares", "Regression"), col=c("red", "orange"), lty=c(1,1))

#j
confint(lm.fit13e) # -1.115 to -0.923 ; 0.393 to 0.606
confint(lm.fit13e2) # -1.029 to -0.981 ; 0.023 to 0.077
confint(lm.fit13i) # -1.167 to -0.888 ; 0.344 to 0.654
# Smallest interval for y in Model 2, largest in  Model 3 (so 2 is best?). Model 2 has the smallest interval for x too. 

#14
set.seed(1)
x1 = runif(100)
x2 = 0.5 * x1 + rnorm (100)/10
y = 2 + 2 * x1 + 0.3 * x2 + rnorm(100)
plot(x1,x2)
cor(x1,x2) # 0.8351212
lm.fit14 = lm(y~x1 + x2)
summary(lm.fit14) # y=2.469 x1=1.329 x2=0.216. x1 is closer to sig. R2 is really low (bad). Can't reject null, especially for B2.
lm.fit14d = lm(y~x1)
summary(lm.fit14d) # Now we can safely reject null. 
lm.fit14e = lm(y~x2)
summary(lm.fit14e) # Now B2 is more significant (0.00102). (Still low R2...)
#f: Contradict isn't the right word. x1 and x2 are clearly collinear though, which reduces the accuracy of the coefs. 
x1 = c(x1, 0.1) #E: what's this doing? I'm confused...
x2 = c(x2, 0.8)
y = c(y, 6)
lm.fit14g1 = lm(y~x1 + x2)
lm.fit14g2 = lm(y~x1)
lm.fit14g3 = lm(y~x2)
summary(lm.fit14g1)
summary(lm.fit14g2)
summary(lm.fit14g3)
#g x1 coef grew (as did its SE) when by itself (and became much more sig.). x2 coef shrunk (but SE grew), and x2 is much more sig when by itself.
par(mfrow=c(2,2))
plot(lm.fit14g1)
plot(lm.fit14g2)
plot(lm.fit14g3)
# Not sure how to interpret. See ch3 solutions and go over?

#15
library(MASS)
summary(Boston)
lm.fit15a = lm(crim ~ zn, data = Boston)
lm.fit15b = lm(crim ~ indus, data = Boston)
lm.fit15c = lm(crim ~ chas, data = Boston)
lm.fit15d = lm(crim ~ nox, data = Boston)
lm.fit15e = lm(crim ~ rm, data = Boston)
lm.fit15f = lm(crim ~ age, data = Boston)
lm.fit15g = lm(crim ~ dis, data = Boston)
lm.fit15h = lm(crim ~ rad, data = Boston)
lm.fit15i = lm(crim ~ tax, data = Boston)
lm.fit15j = lm(crim ~ ptratio, data = Boston)
lm.fit15k = lm(crim ~ black, data = Boston)
lm.fit15l = lm(crim ~ lstat, data = Boston)
lm.fit15m = lm(crim ~ medv, data = Boston)
summary(lm.fit15a) # zn yes
summary(lm.fit15b) # indus yes
summary(lm.fit15c) # chas no
summary(lm.fit15d) # nox yes
summary(lm.fit15e) # rm yes
summary(lm.fit15f) # age yes
summary(lm.fit15g) # dis yes
summary(lm.fit15h) # rad yes
summary(lm.fit15i) # tax yes
summary(lm.fit15j) # ptratio yes
summary(lm.fit15k) # black yes
summary(lm.fit15l) # lstat yes
summary(lm.fit15m) # medv yes
lm.fit15n = lm(crim ~ ., data=Boston)
summary(lm.fit15n)

# Do coef(lm.fit15n) 
# Then coef(lm.fit15a)...?
# Then 
# Then plot(coef(lm.fit15a, ))
