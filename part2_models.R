options(scipen = 999)
data <- read.csv("soccer_clean.csv")
data$position <- factor(data$position, levels = c("DF", "MF", "FW", "GK"))
data$age_c <- data$age - mean(data$age)
data$pass_c <- data$passes_pct - mean(data$passes_pct)
dir.create("images", showWarnings = FALSE)
dir.create("tables", showWarnings = FALSE)
sink("tables/part2_output.txt", split = TRUE)
n <- nrow(data)
pt <- rgb(0, 0, 0, 0.25)
fmt_p <- function(p) ifelse(p < 0.001, "< 0.001", sprintf("%.3f", p))

m1 <- lm(value ~ age_c + goals_assists_per90 + pass_c + minutes + position, data = data)
m2 <- lm(log(value) ~ age_c + goals_assists_per90 + pass_c + minutes + position, data = data)
m3 <- lm(log(value) ~ age_c + I(age_c^2) + goals_assists_per90 + pass_c + minutes + position,
         data = data)
m4 <- lm(log(value) ~ age_c + I(age_c^2) + goals_assists_per90 + pass_c + I(pass_c^2) +
           minutes + position, data = data)
m5 <- lm(log(value) ~ age_c + I(age_c^2) + I(age_c^3) + goals_assists_per90 + pass_c +
           I(pass_c^2) + minutes + position, data = data)
m6 <- lm(log(value) ~ age_c + I(age_c^2) + goals_assists_per90 + pass_c + I(pass_c^2) +
           minutes + position + position:goals_assists_per90, data = data)
m7 <- lm(log(value) ~ age_c + I(age_c^2) + goals_assists_per90 + pass_c + I(pass_c^2) +
           minutes + position + position:pass_c, data = data)
models <- list(m1, m2, m3, m4, m5, m6, m7)
m8 <- update(m4, . ~ . + I(goals_assists_per90^2))
m9 <- update(m4, . ~ . + I(minutes^2))

bc_model <- lm(value ~ age_c + I(age_c^2) + goals_assists_per90 + pass_c + minutes + position,
               data = data, y = TRUE, qr = TRUE)
png("images/fig4_boxcox.png", width = 6.5, height = 4.5, units = "in", res = 150)
par(mar = c(4.5, 4.5, 2.5, 1))
bc <- MASS::boxcox(bc_model, lambda = seq(-1, 1, by = 0.005))
title("Box-Cox profile log-likelihood")
dev.off()
lambda_hat <- bc$x[which.max(bc$y)]
lambda_ci <- range(bc$x[bc$y > max(bc$y) - qchisq(0.95, 1) / 2])

bc_final <- MASS::boxcox(lm(value ~ age_c + I(age_c^2) + goals_assists_per90 + pass_c +
                              I(pass_c^2) + minutes + position, data = data, y = TRUE,
                            qr = TRUE),
                         lambda = seq(-1, 1, by = 0.005), plotit = FALSE)
lambda_hat_final <- bc_final$x[which.max(bc_final$y)]
data$value_before <- ifelse(data$value < 1e6, data$value / 10, data$value)
bc_before <- MASS::boxcox(lm(value_before ~ age_c + I(age_c^2) + goals_assists_per90 + pass_c +
                               minutes + position, data = data, y = TRUE, qr = TRUE),
                          lambda = seq(-1, 1, by = 0.005), plotit = FALSE)
lambda_hat_before <- bc_before$x[which.max(bc_before$y)]
skew <- function(x) {
  x <- x - mean(x)
  mean(x^3) / mean(x^2)^1.5
}
exkurt <- function(x) {
  x <- x - mean(x)
  mean(x^4) / mean(x^2)^2 - 3
}

log_jacobian <- c(0, rep(-sum(log(data$value)), 6))
comparison <- data.frame(
  model = paste0("M", 1:7),
  response = c("value", rep("log(value)", 6)),
  terms = c("main effects", "main effects", "+ age^2", "+ age^2 + pass^2",
            "M4 + age^3", "M4 + position x G+A/90", "M4 + position x pass %"),
  parameters = sapply(models, function(m) length(coef(m))),
  r_squared = round(sapply(models, function(m) summary(m)$r.squared), 3),
  adj_r_squared = round(sapply(models, function(m) summary(m)$adj.r.squared), 3),
  aic_value_scale = round(sapply(models, AIC) - 2 * log_jacobian, 1),
  bic_value_scale = round(sapply(models, BIC) - 2 * log_jacobian, 1)
)

partial_f <- function(small, big, label) {
  a <- anova(small, big)
  data.frame(test = label, df = a$Df[2], F = round(a$F[2], 2), p_value = fmt_p(a[["Pr(>F)"]][2]))
}
f_tests <- rbind(
  partial_f(m2, m3, "age^2 (M2 vs M3)"),
  partial_f(m3, m4, "pass^2 (M3 vs M4)"),
  partial_f(m4, m5, "age^3 (M4 vs M5)"),
  partial_f(m4, m6, "position x G+A/90 (M4 vs M6)"),
  partial_f(m4, m7, "position x pass % (M4 vs M7)"),
  partial_f(m4, m8, "G+A^2 (M4 vs M4 + G+A^2)"),
  partial_f(m4, m9, "minutes^2 (M4 vs M4 + minutes^2)")
)

term_tests <- drop1(m4, test = "F")

coef_table <- as.data.frame(coef(summary(m4)))
names(coef_table) <- c("estimate", "std_error", "t_value", "p_value")
ci <- confint(m4)
coef_table <- data.frame(term = rownames(coef_table),
                         estimate = signif(coef_table$estimate, 4),
                         std_error = signif(coef_table$std_error, 4),
                         ci_lower = signif(ci[, 1], 4),
                         ci_upper = signif(ci[, 2], 4),
                         t_value = round(coef_table$t_value, 2),
                         p_value = fmt_p(coef_table$p_value))

pct <- function(x) round(100 * (exp(x) - 1), 1)
effect_rows <- list(
  c("+0.1 goals + assists per 90", "goals_assists_per90", 0.1),
  c("+90 minutes played", "minutes", 90),
  c("MF vs DF", "positionMF", 1),
  c("FW vs DF", "positionFW", 1),
  c("GK vs DF", "positionGK", 1)
)
pct_effects <- do.call(rbind, lapply(effect_rows, function(r) {
  k <- as.numeric(r[3])
  data.frame(effect = r[1],
             pct_change = pct(k * coef(m4)[r[2]]),
             ci_lower = pct(k * ci[r[2], 1]),
             ci_upper = pct(k * ci[r[2], 2]))
}))
peak_age <- mean(data$age) - coef(m4)["age_c"] / (2 * coef(m4)["I(age_c^2)"])
pass_min <- mean(data$passes_pct) - coef(m4)["pass_c"] / (2 * coef(m4)["I(pass_c^2)"])

X <- model.matrix(m4)[, -1]
vif <- sapply(colnames(X), function(v) {
  1 / (1 - summary(lm(X[, v] ~ X[, colnames(X) != v]))$r.squared)
})

cd <- cooks.distance(m4)
influential <- which(cd > 4 / n)
m4_without <- update(m4, data = data[-influential, ])
sensitivity <- data.frame(term = names(coef(m4)),
                          all_players = signif(coef(m4), 4),
                          without_influential = signif(coef(m4_without), 4))
top_influence <- data.frame(data[order(cd, decreasing = TRUE)[1:5],
                                 c("player", "position", "age", "minutes",
                                   "goals_assists_per90", "passes_pct", "value")],
                            cooks_d = round(sort(cd, decreasing = TRUE)[1:5], 3))

base_row <- function(position) {
  s <- data[data$position == position, ]
  data.frame(age_c = median(s$age) - mean(data$age),
             goals_assists_per90 = median(s$goals_assists_per90),
             pass_c = median(s$passes_pct) - mean(data$passes_pct),
             minutes = median(s$minutes),
             position = factor(position, levels = levels(data$position)))
}
typical <- aggregate(cbind(age, goals_assists_per90, passes_pct, minutes) ~ position,
                     data = data, FUN = median)
pred_value <- function(nd) exp(predict(m4, newdata = nd)) / 1e6
repeat_row <- function(position, k) base_row(position)[rep(1, k), ]

ga_grid <- c(0.1, 0.3, 0.5, 0.8)
age_grid <- c(19, 22, 25, 28, 31, 34)
pass_grid <- c(65, 75, 85)
min_grid <- c(900, 1800, 2700)
s_pos <- do.call(rbind, lapply(levels(data$position), base_row))
s_ga <- repeat_row("FW", length(ga_grid))
s_ga$goals_assists_per90 <- ga_grid
s_age <- repeat_row("MF", length(age_grid))
s_age$age_c <- age_grid - mean(data$age)
s_pass <- repeat_row("MF", length(pass_grid))
s_pass$pass_c <- pass_grid - mean(data$passes_pct)
s_min <- repeat_row("MF", length(min_grid))
s_min$minutes <- min_grid
predictions <- data.frame(
  scenario = c(paste("Typical", levels(data$position)),
               paste("FW, G+A per 90 =", ga_grid),
               paste("MF, age", age_grid),
               paste("MF, pass completion", paste0(pass_grid, "%")),
               paste("MF,", min_grid, "minutes")),
  predicted_value_millions = round(pred_value(rbind(s_pos, s_ga, s_age, s_pass, s_min)), 2)
)

png("images/fig5_model_diagnostics.png", width = 15, height = 7.5, units = "in", res = 150)
par(mfrow = c(2, 4), mar = c(4.5, 4.5, 2.5, 1))
plot(fitted(m1) / 1e6, resid(m1) / 1e6, pch = 19, col = pt,
     main = "M1 (value): residuals vs fitted", xlab = "Fitted value (millions)",
     ylab = "Residual (millions)")
abline(h = 0, lty = 2, col = "red")
plot(data$age, resid(m2), pch = 19, col = pt, main = "M2 (log, linear): residuals vs age",
     xlab = "Age (years)", ylab = "Residual (log scale)")
abline(h = 0, lty = 2, col = "red")
lines(lowess(data$age, resid(m2)), col = "blue", lwd = 2)
plot(fitted(m4), resid(m4), pch = 19, col = pt, main = "M4 (final): residuals vs fitted",
     xlab = "Fitted log(value)", ylab = "Residual (log scale)")
abline(h = 0, lty = 2, col = "red")
plot(cd, type = "h", main = "M4 (final): Cook's distance", xlab = "Player (row)",
     ylab = "Cook's distance", ylim = c(0, max(cd) * 1.15))
abline(h = 4 / n, lty = 2, col = "red")
top3 <- order(cd, decreasing = TRUE)[1:3]
text(top3, cd[top3], labels = data$player[top3], pos = 3, cex = 0.7)
qqnorm(resid(m1) / 1e6, pch = 19, col = pt, main = "M1 (value): normal Q-Q")
qqline(resid(m1) / 1e6, col = "red", lwd = 2)
plot(data$passes_pct, resid(m2), pch = 19, col = pt,
     main = "M2 (log, linear): residuals vs pass %", xlab = "Pass completion (%)",
     ylab = "Residual (log scale)")
abline(h = 0, lty = 2, col = "red")
lines(lowess(data$passes_pct, resid(m2)), col = "blue", lwd = 2)
qqnorm(resid(m4), pch = 19, col = pt, main = "M4 (final): normal Q-Q")
qqline(resid(m4), col = "red", lwd = 2)
plot(m4, which = 5, pch = 19, col = pt, sub.caption = "", caption = rep("", 6),
     main = "M4 (final): residuals vs leverage")
dev.off()

cols <- c(DF = "#1b9e77", MF = "#d95f02", FW = "#7570b3", GK = "#e7298a")
effect_panel <- function(var, raw, shift, xlab, title, positions, legend_pos) {
  curves <- lapply(positions, function(p) {
    r <- range(data[data$position == p, raw])
    grid <- seq(r[1], r[2], length.out = 200)
    nd <- repeat_row(p, length(grid))
    nd[[var]] <- grid - shift
    list(x = grid, y = pred_value(nd))
  })
  plot(NA, xlim = range(unlist(lapply(curves, `[[`, "x"))),
       ylim = c(0, max(unlist(lapply(curves, `[[`, "y")))),
       main = title, xlab = xlab, ylab = "Predicted market value (millions)")
  for (i in seq_along(positions)) {
    lines(curves[[i]]$x, curves[[i]]$y, col = cols[positions[i]], lwd = 2)
  }
  legend(legend_pos, legend = positions, col = cols[positions], lty = 1, lwd = 2, bty = "n")
}
png("images/fig6_effects.png", width = 11, height = 8, units = "in", res = 150)
par(mfrow = c(2, 2), mar = c(4.5, 4.5, 2.5, 1))
effect_panel("age_c", "age", mean(data$age), "Age (years)", "Age",
             levels(data$position), "topright")
effect_panel("goals_assists_per90", "goals_assists_per90", 0, "Goals + assists per 90 minutes",
             "Goals + assists per 90", c("DF", "MF", "FW"), "topleft")
effect_panel("pass_c", "passes_pct", mean(data$passes_pct), "Pass completion (%)",
             "Pass completion", levels(data$position), "topleft")
effect_panel("minutes", "minutes", 0, "Minutes played", "Minutes played",
             levels(data$position), "topleft")
dev.off()

cat("\nBox-Cox (M3 predictors): lambda-hat =", lambda_hat, ", 95% CI", lambda_ci[1], "to",
    lambda_ci[2], "\n")
cat("Box-Cox (M4 predictors): lambda-hat =", lambda_hat_final, "\n")
cat("Box-Cox before the x10 value fix (M3 predictors): lambda-hat =", lambda_hat_before, "\n")
cat("\nCandidate models (AIC and BIC on the original value scale, comparable across responses)\n")
print(comparison, row.names = FALSE)
cat("\nPartial F-tests\n")
print(f_tests, row.names = FALSE)
cat("\nFinal model M4: F-test for dropping each term\n")
print(term_tests)
cat("\nFinal model M4 coefficients (response = log market value)\n")
print(coef_table, row.names = FALSE)
cat("\nResidual standard error:", round(summary(m4)$sigma, 3), "on", m4$df.residual,
    "df; R-squared =", round(summary(m4)$r.squared, 3), "; adjusted R-squared =",
    round(summary(m4)$adj.r.squared, 3), "\n")
cat("\nPercent change in market value, with 95% CI\n")
print(pct_effects, row.names = FALSE)
cat("Peak age:", round(peak_age, 1), "; pass completion curve minimum:", round(pass_min, 1), "%\n")
cat("\nVariance inflation factors\n")
print(round(vif, 2))
cat("\nInfluence: Cook's distance > 4/n for", length(influential), "players; max =",
    round(max(cd), 3), "; |standardized residual| > 3 for", sum(abs(rstandard(m4)) > 3),
    "players\n")
print(top_influence, row.names = FALSE)
cat("Residual skewness and excess kurtosis: M1", round(skew(resid(m1)), 2),
    round(exkurt(resid(m1)), 2), "; M4", round(skew(resid(m4)), 2),
    round(exkurt(resid(m4)), 2), "\n")
cat("Correlation of |residual| with fitted value: M1", round(cor(abs(resid(m1)), fitted(m1)), 2),
    "; M4", round(cor(abs(resid(m4)), fitted(m4)), 2), "\n")
cat("Median absolute difference between M4 and M5 predictions:",
    round(100 * median(abs(exp(fitted(m5) - fitted(m4)) - 1)), 1), "%\n")
cat("MF vs DF without influential players: p =",
    fmt_p(coef(summary(m4_without))["positionMF", 4]), "\n")
cat("\nM4 coefficients with and without influential players\n")
print(sensitivity, row.names = FALSE)
cat("\nTypical player for each position (medians), used as the baseline for predictions\n")
print(typical, row.names = FALSE)
cat("\nPredicted typical (median) market value, millions; other variables at position medians\n")
print(predictions, row.names = FALSE)

write.csv(comparison, "tables/table_model_comparison.csv", row.names = FALSE)
write.csv(f_tests, "tables/table_partial_f.csv", row.names = FALSE)
write.csv(coef_table, "tables/table_final_coefficients.csv", row.names = FALSE)
write.csv(pct_effects, "tables/table_percent_effects.csv", row.names = FALSE)
write.csv(sensitivity, "tables/table_influence_sensitivity.csv", row.names = FALSE)
write.csv(predictions, "tables/table_predictions.csv", row.names = FALSE)
sink()
