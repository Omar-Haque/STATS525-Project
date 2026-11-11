data <- read.csv("soccer_clean.csv")
project_data <- data[, c("value","age","goals_assists_per90","position",
                         "passes_pct","minutes")]
numeric_preds <- project_data[, c("age","goals_assists_per90","passes_pct","minutes")]
cor(numeric_preds)