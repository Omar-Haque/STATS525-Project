data <- read.csv("soccer_clean.csv")
project_data <- data[, c("value","age","goals_assists_per90","position",
  "passes_pct","minutes")]
plot(project_data$age, project_data$value, pch = 19,
     xlab = "Age",ylab = "Market Value")
plot(project_data$goals_assists_per90, project_data$value,pch = 19,
     xlab = "Goals and Assists per 90 Minutes",ylab = "Market Value")
plot(project_data$passes_pct, project_data$value, pch = 19,
     xlab = "Pass Completion", ylab = "Market Value")
plot(project_data$minutes, project_data$value, pch = 19,
     xlab = "Minutes Played",ylab = "Market Value")
model_age <- lm(value ~ age, data = project_data)
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





