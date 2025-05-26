### MASM15 Proj 2
library(extRemes)

# Loading data
data("PORTw", package = "extRemes")
names(PORTw)

# Fitting models
fit1 <- fevd(TMX1, PORTw, units = "deg C")
fit2 <- fevd(TMX1, PORTw, location.fun = ~ AOindex, units = "deg C")
plot(fit2)

# Q1
fit2
# m0 + m1t

plot(fit2, "trace")

# Q2
# Gradient 0 when negative likelihood minimized

# Q3
lr.test(fit1, fit2)
# Reject reduced model = include covariate
#p-value = 0.0005653


# Q4 - explain values
AO.cov <- make.qcov(fit2, vals = list(mu1 = c(-0.5, 0.5)))
return.level(fit2, return.period = c(2, 20, 100), qcov = AO.cov)


fit3 <- fevd(TMX1, PORTw, location.fun= ~ AOindex, scale.fun = ~ AOindex,
             units = "deg C")
fit3
plot(fit3)

# LR-test

lr.test(fit1, fit3)
# Significant
lr.test(fit2, fit3)
# Reduced model sufficient

# Q5
# Probably lr.test(fit2, fit3)
# Should not include scale parameter!

## 3
data("damage", package = "extRemes")
par(mfrow = c(2, 2))
plot(damage$Year, damage$Dam, xlab = "Year",
     ylab = "Damage (billion USD)", cex = 1.25,
     cex.lab = 1.25, col = "darkblue", bg = "lightblue", pch = 21)
plot(damage[, "Year"], log(damage[, "Dam"]), xlab = "Year",
     ylab = "log(Damage)", ylim = c(-10, 5), cex.lab = 1.25,
     col = "darkblue", bg = "lightblue", pch = 21)
qqnorm(log(damage[, "Dam"]), ylim = c(-10, 5), cex.lab = 1.25)

# Finding threshold
threshrange.plot(damage$Dam, r = c(1, 15), nint = 20)

# Q6
# Choose value that is constant but low so we have many data points

# Q7
mrlplot(damage$Dam, xlim = c(2, 12))
# Maybe 6

# Fitting model
fitD <- fevd(Dam, damage, threshold = 6, type = "GP", time.units = "2.06/year")
fitD
plot(fitD)

# Q8
# Why Q-Q not good?
# CI for shape

fitD2 <- fevd(Dam, damage, threshold = 6, type = "GP", time.units = "10/year")
fitD2
plot(fitD2)

# Q9
# Size of exceedence is not affected by time scale, only frequency