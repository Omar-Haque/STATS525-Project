data <- read.csv("soccer_clean.csv")
project_data <- data[, c("value","age","goals_assists_per90","position",
                         "passes_pct","minutes")]
model <- lm(value ~ age + goals_assists_per90 + position + passes_pct + minutes,
            data = project_data)
plot(fitted(model), resid(model), pch = 19,
     xlab = "Fitted values",ylab = "Residuals")
abline(h = 0, lty = 2)
qqnorm(resid(model), pch = 19)
qqline(resid(model), lwd = 2)
model0 <- lm(log(value) ~ age + goals_assists_per90 + position + passes_pct
             + minutes,data = project_data)
plot(fitted(model0), resid(model0), pch = 19,
     xlab = "Fitted values",ylab = "Residuals")
abline(h = 0, lty = 2)
qqnorm(resid(model0), pch = 19)
qqline(resid(model0), lwd = 2)
bc <- MASS::boxcox(model, lambda = seq(-2, 2, by = 0.01))
lambda.hat <- bc$x[which.max(bc$y)]
model1 <- lm(I(value^lambda.hat) ~ age + goals_assists_per90 +
               position + passes_pct + minutes,data = project_data)
plot(fitted(model1), resid(model1), pch = 19,
     xlab = "Fitted values", ylab = "Residuals")
abline(h = 0, lty = 2)
qqnorm(resid(model1), pch = 19)
qqline(resid(model1), lwd = 2)
summary(model1)
summary(model)
summary(model0)
lambda.hat