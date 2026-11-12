data <- read.csv("soccer_clean.csv")
project_data <- data[, c("value","age","goals_assists_per90","position",
                         "passes_pct","minutes")]
hist(project_data$value,
     main = "Market Value",xlab = "Market Value")
hist(project_data$age,
     main = "Age",xlab = "Age")
hist(project_data$goals_assists_per90,
     main = "Goals and Assists per 90",xlab = "Goals and Assists per 90")
hist(project_data$passes_pct,
     main = "Pass Completion Percentage",xlab = "Pass Completion")
hist(project_data$minutes,
     main = "Minutes Played",xlab = "Minutes Played")
table(project_data$position)

