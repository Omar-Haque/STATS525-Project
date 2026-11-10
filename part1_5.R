data <- read.csv("soccer_clean.csv")
project_data <- data[, c(
  "value",
  "age",
  "goals_assists_per90",
  "position",
  "passes_pct",
  "minutes"
)]
model <- lm(value ~ age + goals_assists_per90 + position + passes_pct + minutes,
            data = project_data)
plot(fitted(model), resid(model), pch = 19,
     xlab = "Fitted values",ylab = "Residuals")
abline(h = 0, lty = 2)
qqnorm(resid(model), pch = 19)
qqline(resid(model), lwd = 2)
model0 <- lm(log(value) ~ age + goals_assists_per90 + position + passes_pct + minutes,
             data = project_data)
plot(fitted(model0), resid(model0), pch = 19,
     xlab = "Fitted values",ylab = "Residuals")
abline(h = 0, lty = 2)
qqnorm(resid(model0), pch = 19)
qqline(resid(model0), lwd = 2)
bc <- MASS::boxcox(model, lambda = seq(-2, 2, by = 0.01))
lambda.hat <- bc$x[which.max(bc$y)]
lambda.hat
plot(project_data$age, project_data$value, pch = 19,
     xlab = "Age",ylab = "Market Value")
plot(project_data$goals_assists_per90, project_data$value,pch = 19,
     xlab = "Goals and Assists per 90 Minutes",ylab = "Market Value")
plot(project_data$passes_pct, project_data$value, pch = 19,
     xlab = "Pass Completion", ylab = "Market Value")
plot(project_data$minutes, project_data$value, pch = 19,
     xlab = "Minutes Played",ylab = "Market Value")


