data <- read.csv("soccer_clean.csv")
project_data <- data[, c("value","age","goals_assists_per90","position",
                         "passes_pct","minutes")]
model_age <- lm(value ~ age, data = project_data)
model_goals_assists_per90 <- lm(value ~ goals_assists_per90,data = project_data)
model_passes_pct <- lm(value ~ passes_pct, data = project_data)
model_minutes <- lm(value ~ minutes, data = project_data)
par(mfrow = c(2, 2))
plot(fitted(model_age), resid(model_age), pch = 19,
     xlab = "Fitted values", ylab = "Residuals")
abline(h = 0, lty = 2)
qqnorm(resid(model_age), pch = 19)
qqline(resid(model_age), lwd = 2)

model_goals_assists_per90 <- lm(value ~ goals_assists_per90, data = project_data)
plot(fitted(model_goals_assists_per90), resid(model_goals_assists_per90), pch = 19,
     xlab = "Fitted values", ylab = "Residuals")
abline(h = 0, lty = 2)
qqnorm(resid(model_goals_assists_per90), pch = 19)
qqline(resid(model_goals_assists_per90), lwd = 2)

model_passes_pct <- lm(value ~ passes_pct, data = project_data)
plot(fitted(model_passes_pct), resid(model_passes_pct), pch = 19,
     xlab = "Fitted values", ylab = "Residuals")
abline(h = 0, lty = 2)
qqnorm(resid(model_passes_pct), pch = 19)
qqline(resid(model_passes_pct), lwd = 2)

model_minutes <- lm(value ~ minutes, data = project_data)
plot(fitted(model_minutes), resid(model_minutes), pch = 19,
     xlab = "Fitted values", ylab = "Residuals")
abline(h = 0, lty = 2)
qqnorm(resid(model_minutes), pch = 19)
qqline(resid(model_minutes), lwd = 2)
summary(model_age)
summary(model_goals_assists_per90)
summary(model_passes_pct)
summary(model_minutes)
