data <- read.csv("soccer_clean.csv")
project_data <- data[, c("value","age","goals_assists_per90","position",
                         "passes_pct","minutes")]
par(mfrow = c(2, 3))
plot(project_data$age, project_data$value, pch = 19,
     xlab = "Age",ylab = "Market Value")
plot(project_data$goals_assists_per90, project_data$value,pch = 19,
     xlab = "Goals and Assists per 90 Minutes",ylab = "Market Value")
plot(project_data$passes_pct, project_data$value, pch = 19,
     xlab = "Pass Completion", ylab = "Market Value")
plot(project_data$minutes, project_data$value, pch = 19,
     xlab = "Minutes Played",ylab = "Market Value")
boxplot(value ~ position, data = project_data,
     xlab = "Position",ylab = "Market Value")

