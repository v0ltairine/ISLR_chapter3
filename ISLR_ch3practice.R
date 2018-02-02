# Exercise 8
Auto=read.table("Auto.data", header=T, na.strings="?")
Auto=na.omit(Auto)
lm.fit8=lm(mpg~horsepower, data=Auto)
summary(lm.fit8)
predict(lm.fit8, data.frame(horsepower=98), interval="confidence")
predict(lm.fit8, data.frame(horsepower=98), interval="prediction")
plot(mpg ~ horsepower, data=Auto)
abline(lm.fit8)
par(mfrow=c(2,2))
plot(lm.fit8)

# Exercise 9
pairs(~mpg+., data=Auto)
names(Auto)
cor(Auto[1:8])
lm.fit9=lm(mpg~. -name, data=Auto)
summary(lm.fit9)
plot(lm.fit9)
lm.fit9e=lm(mpg~. -name +year*origin +weight*displacement, data=Auto)
summary(lm.fit9e)
lm.fit9f=lm(mpg ~ . -name +I(horsepower^2), data=Auto)
summary(lm.fit9f)

# Exercise 10
library(ISLR)
lm.fit10a=lm(Sales~Price +Urban +US, data=Carseats)
summary(lm.fit10a)
?Carseats
lm.fit10e=lm(Sales ~ Price +US, data=Carseats)
summary(lm.fit10e)
anova(lm.fit10a, lm.fit10e) # Hmmm. Model E isn't really signigicant. I think this shows that neither fit well? 
confint(lm.fit10e, level = 0.95)
confint(lm.fit10e)
plot(lm.fit10e)

# Exercise 11
set.seed(1)
x=rnorm(100)
y=2*x+rnorm(100)
lm.fit11a=lm(y~x+0)
summary(lm.fit11a)
lm.fit11b=lm(x~y+0)
summary(lm.fit11b)
