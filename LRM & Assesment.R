#Linear Regression Model Building and Assesment#

# reading the data
df <- read.delim("bonds.txt", row.names = 1)

# view the data
View(df)

# top and bottom 6 rows
head(df)
tail(df)

# Summary of the data
summary(df)

# Plotting the data
plot(df$CouponRate, df$BidPrice,
     main="Bid Price Vs Coupon Rate",
     xlab = "CouponRate",
     ylab = "BidPrice")

#Building linear regression model
bondsmod <- lm(df$BidPrice~df$CouponRate)

#Fitting the regression line over the plot
plot(df$CouponRate,df$BidPrice,
     main = "CouponRate Vs BidPrice",
     xlab = "CouponRate",
     ylab = "BidPrice")
abline(bondsmod)

# Model Summary
summary(bondsmod)

#Model Assesment
#hypotyhesis test on coefficient
#Lvel1
alpha<- 0.05
n<- 35
p<-1
qt(p=1-(alpha/2), df=n-p-1)

Confident_int_Low <- 3.0661 - (2.034515*0.3068)
Confident_int_upper <- 3.0661 + (2.034515*0.3068)
confident_interval<- c(Confident_int_Low,Confident_int_upper)
confident_interval

# Computing F-statistics
SSE<- sum((df$BidPrice-bondsmod$fitted.values)^2)
SSR <- sum((bondsmod$fitted.values-mean(df$BidPrice))^2)
n<-35
F_stat <- (SSR/SSE)*(n-2)
SSE
SSR
F_stat

#level2
#Residual Analysis
plot(bondsmod$fitted.values, rstandard(bondsmod),
     main = "Residual Plot",
     xlab = "Predicted Value for BidPrice",
     ylab = "Standardized Residuals")
abline(h=2, lty=2)
abline(h=-2, lty=2)

#locate the outlines
plot(bondsmod$fitted.values, rstandard(bondsmod),
     main = "Residual Plot",
     xlab = "Predicted Value for BidPrice",
     ylab = "Standardized Residuals")
abline(h=2, lty=2)
abline(h=-2, lty=2)
identify(bondsmod$fitted.values, rstandard(bondsmod))

#Remove farthest outlier point-13
bonds_new <- df[-13,]
bondsmod1 <- lm(bonds_new$BidPrice~bonds_new$CouponRate)

plot(bondsmod1$fitted.values, rstandard(bondsmod1),
     main = "Residual Plot",
     xlab = "Predicted Value for BidPrice",
     ylab = "Standardized Residuals")
abline(h=2, lty=2)
abline(h=-2, lty=2)
identify(bondsmod1$fitted.values, rstandard(bondsmod1))

#Remove farthest outlier- point 34
bonds_new1 <- bonds_new[-34,]
bondsmod2 <- lm(bonds_new1$BidPrice~bonds_new1$CouponRate)

plot(bondsmod2$fitted.values, rstandard(bondsmod2),
     main = "Residual Plot",
     xlab = "Predicted Value for BidPrice",
     ylab = "Standardized Residuals")
abline(h=2, lty=2)
abline(h=-2, lty=2)
identify(bondsmod2$fitted.values, rstandard(bondsmod2))

#Remove farthest outlier- point 4
bonds_new2 <- bonds_new1[-4,]
bondsmod3 <- lm(bonds_new2$BidPrice~bonds_new2$CouponRate)

plot(bondsmod3$fitted.values, rstandard(bondsmod3),
     main = "Residual Plot",
     xlab = "Predicted Value for BidPrice",
     ylab = "Standardized Residuals")
abline(h=2, lty=2)
abline(h=-2, lty=2)
identify(bondsmod3$fitted.values, rstandard(bondsmod3))

#Remove farthest outlier- point 32
bonds_new3 <- bonds_new2[-4,]
bondsmod4 <- lm(bonds_new3$BidPrice~bonds_new3$CouponRate)

plot(bondsmod4$fitted.values, rstandard(bondsmod4),
     main = "Residual Plot",
     xlab = "Predicted Value for BidPrice",
     ylab = "Standardized Residuals")
abline(h=2, lty=2)
abline(h=-2, lty=2)
identify(bondsmod4$fitted.values, rstandard(bondsmod4))

summary(bondsmod4)

# After removing outlier plot new regression over plot
plot(df$CouponRate[-c(4,13,34,35)], df$BidPrice[-c(4,13,34,35)],
     main = "BidPrice Vs CouponRate without Outliers",
     xlab = "CouponRate",
     ylab = "BidPrice")
abline(bondsmod1)
