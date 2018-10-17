setwd("Documents/College/S18/Pstat126/proj")
projdata=read.table("projdata.txt",header=T)
attach(projdata)
summary(projdata)

#1. fit Scatterplots
pairs(projdata)

#2. fit 1st order linear model
fit1 = lm(happy~gender+workhrs+relationship, data = projdata)
summary(fit1)

#3. Evaluate 2-way interaction
fit2 = lm(happy~.^2,data=projdata)
summary(fit2)
anova(fit2,fit1)

#4. Model Selection
plot(fitted.values(fit1),residuals(fit1),pch=19)
null=lm(happy~1,data=projdata)
full=lm(happy~.^2,data=projdata)
step(null,scope=list(lower=null,upper=full),direction='forward')
step(full,direction='backward')
step(null,scope=list(upper=full),direction='both') 

#5. Residuals
fit3 = lm(formula = happy ~ relationship + gender + workhrs + relationship:gender, data = projdata)
plot(fitted(fit3),residuals(fit3),pch=19)
abline(h=0)
qqnorm(residuals(fit3))
qqline(residuals(fit3))
hist(residuals(fit3))
summary(fit3)

#6. desribe final model
summary(fit3)
## interaction plot for relationship 
plot(relationship,happy,xlim=c(0,10),ylim=c(0,10),col="blue",xlab="Relationship",ylab="Happy",pch=19)
points(relationship[gender==1],happy[gender==1],col="red",pch=19)
abline(lm(happy[gender==0]~relationship[gender==0]),col="blue")
abline(lm(happy[gender==1]~relationship[gender==1]),col="red")
legend("topleft", inset=.05,cex=.75,pch=19,lty=c(1,1),col=c("red","blue"),legend=c( "female","male"))
