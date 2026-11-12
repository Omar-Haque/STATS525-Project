options(scipen = 999)
data <- read.csv("soccer_clean.csv")
data$position <- factor(data$position, levels = c("DF", "MF", "FW", "GK"))
data$value_m <- data$value / 1e6
dir.create("images", showWarnings = FALSE)
dir.create("tables", showWarnings = FALSE)
sink("tables/part1_output.txt", split = TRUE)
pt <- rgb(0, 0, 0, 0.25)
fill <- "grey85"

png("images/fig1_distributions.png", width = 13, height = 6.5, units = "in", res = 150)
par(mfrow = c(2, 4), mar = c(4.5, 4.5, 2.5, 1))
hist(data$value_m, breaks = 40, col = fill, main = "Market value",
     xlab = "Market value (millions)")
hist(log(data$value), breaks = 40, col = fill, main = "log(Market value)",
     xlab = "log(market value)")
hist(log(ifelse(data$value < 1e6, data$value / 10, data$value)), breaks = 40, col = fill,
     main = "log(Market value), before x10 fix", xlab = "log(market value)")
hist(data$age, breaks = seq(16.5, 40.5, by = 1), col = fill, main = "Age",
     xlab = "Age (years)")
hist(data$goals_assists_per90, breaks = 40, col = fill, main = "Goals + assists per 90",
     xlab = "Goals + assists per 90 minutes")
hist(data$passes_pct, breaks = 40, col = fill, main = "Pass completion",
     xlab = "Pass completion (%)")
hist(data$minutes, breaks = 30, col = fill, main = "Minutes played",
     xlab = "Minutes played")
barplot(table(data$position), col = fill, main = "Position",
        xlab = "Position", ylab = "Number of players")
dev.off()

xvars <- c("age", "goals_assists_per90", "passes_pct", "minutes")
titles <- c("Age", "Goals + assists per 90", "Pass completion", "Minutes played")
xlabs <- c("Age (years)", "Goals + assists per 90 minutes", "Pass completion (%)",
           "Minutes played")

png("images/fig2_scatterplots.png", width = 13, height = 8, units = "in", res = 150)
par(mfrow = c(2, 3), mar = c(4.5, 4.5, 2.5, 1))
for (i in seq_along(xvars)) {
  plot(data[[xvars[i]]], data$value_m, pch = 19, col = pt, main = titles[i],
       xlab = xlabs[i], ylab = "Market value (millions)")
  lines(lowess(data[[xvars[i]]], data$value_m), col = "red", lwd = 2)
}
boxplot(value_m ~ position, data = data, col = fill, main = "Market value by position",
        xlab = "Position", ylab = "Market value (millions)")
boxplot(log(value) ~ position, data = data, col = fill,
        main = "log(Market value) by position", xlab = "Position",
        ylab = "log(market value)")
dev.off()

slr_models <- lapply(xvars, function(v) lm(reformulate(v, response = "value_m"), data = data))

png("images/fig3_slr_diagnostics.png", width = 15, height = 7.5, units = "in", res = 150)
par(mfrow = c(2, 4), mar = c(4.5, 4.5, 2.5, 1))
for (i in seq_along(slr_models)) {
  m <- slr_models[[i]]
  plot(fitted(m), resid(m), pch = 19, col = pt, main = paste0(titles[i], ": residuals vs fitted"),
       xlab = "Fitted value (millions)", ylab = "Residual (millions)")
  abline(h = 0, lty = 2, col = "red")
}
for (i in seq_along(slr_models)) {
  m <- slr_models[[i]]
  qqnorm(resid(m), pch = 19, col = pt, main = paste0(titles[i], ": normal Q-Q"))
  qqline(resid(m), col = "red", lwd = 2)
}
dev.off()

fmt_p <- function(p) ifelse(p < 0.001, "< 0.001", sprintf("%.3f", p))

slr_table <- do.call(rbind, lapply(seq_along(slr_models), function(i) {
  s <- summary(slr_models[[i]])
  co <- coef(s)
  data.frame(predictor = titles[i],
             slope = signif(co[2, 1], 4),
             std_error = signif(co[2, 2], 4),
             t_value = round(co[2, 3], 2),
             p_value = fmt_p(co[2, 4]),
             r_squared = round(s$r.squared, 3))
}))
position_model <- lm(value_m ~ position, data = data)
position_f <- anova(position_model)

cat("\nSimple linear regressions of market value (millions) on each predictor\n")
print(slr_table, row.names = FALSE)
cat("\nMarket value (millions) on position\n")
print(round(coef(summary(position_model)), 3))
cat("F =", round(position_f$`F value`[1], 2), "on", position_f$Df[1], "and", position_f$Df[2],
    "df, p", fmt_p(position_f$`Pr(>F)`[1]), ", R-squared =",
    round(summary(position_model)$r.squared, 3), "\n")
write.csv(slr_table, "tables/table_slr.csv", row.names = FALSE)

skew <- function(x) {
  x <- x - mean(x)
  mean(x^3) / mean(x^2)^1.5
}
num_vars <- c("value_m", xvars)
desc <- data.frame(variable = c("Market value (millions)", titles),
                   mean = sapply(num_vars, function(v) mean(data[[v]])),
                   median = sapply(num_vars, function(v) median(data[[v]])),
                   sd = sapply(num_vars, function(v) sd(data[[v]])),
                   min = sapply(num_vars, function(v) min(data[[v]])),
                   max = sapply(num_vars, function(v) max(data[[v]])),
                   skewness = sapply(num_vars, function(v) skew(data[[v]])))
desc[, -1] <- round(desc[, -1], 2)
cat("\nDescriptive statistics\n")
print(desc, row.names = FALSE)
cat("Skewness of log(value):", round(skew(log(data$value)), 2), "\n")
cat("Share of players with zero goals + assists:",
    round(mean(data$goals_assists_per90 == 0), 3), "\n")
cat("\nMedian market value (millions) by position\n")
print(tapply(data$value_m, data$position, median))
cat("\nMedian market value (millions) by age group\n")
print(tapply(data$value_m, cut(data$age, c(16, 19, 24, 28, 30, 32, 35, 40)), median))
q <- quantile(data$value_m, c(0.25, 0.75))
cat("\nPlayers above Q3 + 1.5 IQR:", sum(data$value_m > q[2] + 1.5 * diff(q)),
    "; worth 100 million or more:", sum(data$value_m >= 100), "\n")
print(head(data[order(-data$value), c("player", "position", "age", "goals_assists_per90",
                                      "value_m")], 6), row.names = FALSE)
lz <- (log(data$value) - mean(log(data$value))) / sd(log(data$value))
cat("Range of standardized log(value):", round(range(lz), 2), "\n")
cat("Lowest pass completion:", head(sort(data$passes_pct), 4), "\n")
cat("Players below 60% pass completion:", sum(data$passes_pct < 60), "; goalkeepers:",
    sum(data$passes_pct < 60 & data$position == "GK"), "\n")
cor_table <- round(cor(data[, xvars]), 2)
cat("\nCorrelations among quantitative predictors\n")
print(cor_table)
cat("\nCorrelation with log(value)\n")
print(round(sapply(xvars, function(v) cor(data[[v]], log(data$value))), 2))
cat("\nR-squared of simple regressions on log(value)\n")
print(round(sapply(xvars, function(v) summary(lm(log(data$value) ~ data[[v]]))$r.squared), 3))
write.csv(cor_table, "tables/table_correlations.csv")
sink()
