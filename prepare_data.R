MIN_MINUTES <- 450
SEASON_MAX  <- 3420

data <- read.csv("soccer_old.csv", stringsAsFactors = FALSE)
n_start <- nrow(data)

n_rescaled <- sum(data$value < 1e6)
data$value[data$value < 1e6] <- data$value[data$value < 1e6] * 10

data$passes_pct[data$passes_pct == 0] <- NA

dup_names <- unique(data$player[duplicated(data$player)])
is_clash <- sapply(dup_names, function(p) {
  rows <- data[data$player == p, ]
  length(unique(rows$age)) > 1 ||
    length(unique(rows$value)) > 1 ||
    sum(rows$minutes) > SEASON_MAX
})
clash_names <- dup_names[is_clash]
data <- data[!(data$player %in% clash_names), ]
n_after_clash <- nrow(data)

combine_rows <- function(rows) {
  w  <- rows$minutes
  ok <- !is.na(rows$passes_pct)
  main <- rows[which.max(w), ]
  data.frame(
    value               = rows$value[1],
    player              = rows$player[1],
    position            = main$position,
    age                 = rows$age[1],
    minutes             = sum(w),
    goals_assists_per90 = round(sum(rows$goals_assists_per90 * w) / sum(w), 2),
    passes_pct          = if (any(ok)) {
                            round(sum(rows$passes_pct[ok] * w[ok]) / sum(w[ok]), 1)
                          } else NA,
    stringsAsFactors    = FALSE
  )
}
players <- data$player[!duplicated(data$player)]
data <- do.call(rbind, lapply(players, function(p) {
  combine_rows(data[data$player == p, ])
}))
data$passes_proportion <- round(data$passes_pct / 100, 3)
n_after_combine <- nrow(data)

data$position <- sub(",.*$", "", data$position)

n_short   <- sum(data$minutes < MIN_MINUTES)
n_no_pass <- sum(data$minutes >= MIN_MINUTES & is.na(data$passes_pct))
data <- data[data$minutes >= MIN_MINUTES & !is.na(data$passes_pct), ]
rownames(data) <- NULL

write.csv(data, "soccer_clean.csv", row.names = FALSE)

cat("Rows in soccer_old.csv:          ", n_start, "\n")
cat("Values under 1 million, x10:     ", n_rescaled, "\n")
cat("Dropped, name clashes:           ", n_start - n_after_clash,
    "rows (", paste(clash_names, collapse = ", "), ")\n")
cat("Merged, repeated players:        ", n_after_combine, "players from",
    n_after_clash, "rows\n")
cat("Dropped, under", MIN_MINUTES, "minutes:       ", n_short, "\n")
cat("Dropped, missing pass %:         ", n_no_pass, "\n")
cat("Rows in soccer_clean.csv:        ", nrow(data), "\n")
print(table(data$position))
