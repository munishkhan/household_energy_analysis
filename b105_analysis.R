# B105 Applied Statistical Modelling - Individual Project


# The Business Problem :  What is the main reason for a household's high annual energy bill?

# Dataset : US Energy Information Administration, Residential Energy Consumption
#       Survey (RECS) 2020, public use microdata.
#       https://www.eia.gov/consumption/residential/data/2020/index.php?view=microdata


# Packages

library(psych)   
library(car)    

# SECTION 1  LOAD THE DATA



data_file <- file.path("data", "recs2020_public_v7.csv.gz")
if (!file.exists(data_file)) data_file <- file.path("data", "recs2020_public_v7.csv")

if (!file.exists(data_file)) {
  stop("Dataset file not found.")
}


recs_full <- read.csv(data_file, stringsAsFactors = FALSE)

cat("Dimensions:", nrow(recs_full), "households x",
    ncol(recs_full), "variables\n")
cat("Duplicated household IDs:", sum(duplicated(recs_full$DOEID)), "\n")


# Exploring the data
# View(recs_full)



inventory <- data.frame(
  variable   = names(recs_full),
  type       = vapply(recs_full, function(x) class(x)[1], character(1)),
  n_distinct = vapply(recs_full, function(x) length(unique(x)), integer(1)),
  n_missing  = vapply(recs_full, function(x) sum(is.na(x)), integer(1)),
  row.names  = NULL
)

# View(inventory)

cat("\nVariable types\n")
print(table(inventory$type))


# Choosing the variables needed for the analysis.
vars_needed <- c(
  "DOEID",          # unique household id
  "TOTALDOL",       # annual energy cost for all fuels
  "TOTALBTU",       # annual site energy
  "TOTSQFT_EN",     # energy-used floor area
  "HDD65",          # heating degree days
  "CDD65",          # cooling degree days
  "REGIONC",        # census region
  "TYPEHUQ",        # housing type
  "YEARMADERANGE"   # year built, banded
)

recs_raw <- recs_full[, vars_needed]

cat("\nStructure of the data:\n")
str(recs_raw)


# SECTION 2 - CHECK AND CLEAN

# the documentation of the dataset explains that RECS codes saves "not applicable" as -2, which read.csv imports as a normal number, checking if our respective features have this value, also checking for nulls.

checks <- data.frame(
  n_missing = vapply(recs_raw, function(x) sum(is.na(x)), integer(1)),
  n_code_na = vapply(recs_raw, function(x) {
    if (is.numeric(x)) sum(x == -2, na.rm = TRUE) else 0L
  }, integer(1))
)

cat("\nCompleteness check:\n")
print(checks)


# Cost must be positive

keep <- recs_raw$TOTALDOL > 0 & recs_raw$TOTSQFT_EN > 0
recs <- recs_raw[keep, ]

cat("\nExclusions:\n")
cat("cost <= 0:", sum(recs_raw$TOTALDOL <= 0), "\n")
cat("floor area <= 0:", sum(recs_raw$TOTSQFT_EN <= 0), "\n")
cat("rows before:", nrow(recs_raw), "\n")
cat("rows analysed:", nrow(recs), "\n")


#fixing Variable types.

# Changing to categorical type
recs$REGIONC <- factor(
  recs$REGIONC,
  levels = c("NORTHEAST", "MIDWEST", "SOUTH", "WEST"),
  labels = c("Northeast", "Midwest", "South", "West")
)


# removing ordinality from categorical data
recs$TYPEHUQ <- factor(
  recs$TYPEHUQ,
  levels = 1:5,
  labels = c("Mobile home",
             "Single-family detached",
             "Single-family attached",
             "Apartment, 2-4 units",
             "Apartment, 5+ units")
)


# Codebook bands from the documentation: 1 before 1950 ... 6 1990-99, 7 2000-09, 8 2010-15, 9 2016-20. -> numbers corresponding to a certain time period
# Codes 1-6 are pre-2000, codes 7-9 are 2000 or later. Grouping variable accordingly
recs$vintage <- factor(
  ifelse(recs$YEARMADERANGE <= 6, "Before 2000", "2000 or later"),
  levels = c("Before 2000", "2000 or later")
)

# keeping the variables the tests will use.
recs <- recs[, c("TOTALDOL", "TOTALBTU", "TOTSQFT_EN", "HDD65", "CDD65",
                 "REGIONC", "TYPEHUQ", "vintage")]

cat("\nStructure of the analysis data:\n")
str(recs)

cat("\nSummary of the analysis data:\n")
print(summary(recs))


# Sampling.

# RECS 2020 dataset is a stratified multistage sample with a final weight (NWEIGHT) and
# 60 replicate weights. This analysis is unweighted.

# SECTION 3 - EXPLORE

# Descriptive statistics for the numeric variables.

num_vars <- c("TOTALDOL", "TOTALBTU", "TOTSQFT_EN", "HDD65", "CDD65")
descriptives <- describe(recs[num_vars])[, c("n", "mean", "sd", "median",
                                             "min", "max", "se")]
descriptives$iqr <- sapply(recs[num_vars], IQR)

cat("\nDescriptive statistics:\n")
print(round(descriptives, 2))



out_vals <- boxplot.stats(recs$TOTALDOL)$out

cat("\n Number of Outliers in total cost variable (TOTALDOL), 1.5 times the Inter Quartile Range):",
    length(out_vals),
    sprintf("(%.1f%% of households)\n", 100 * length(out_vals) / nrow(recs)))


# Group means. These are the numbers H1 and H2 test formally.

cat("\nMean annual cost by region:\n")
print(aggregate(TOTALDOL ~ REGIONC, data = recs, FUN = mean))

cat("\nMean annual cost by housing type:\n")
print(aggregate(TOTALDOL ~ TYPEHUQ, data = recs, FUN = mean))

cat("\nMean annual cost by vintage:\n")
print(aggregate(TOTALDOL ~ vintage, data = recs, FUN = mean))

# Visualization
# Plots. 



old_par <- par(mfrow = c(1, 2))
hist(recs$TOTALDOL, breaks = 60, col = "grey67", border = "white",
     main = "Annual energy cost", xlab = "USD per year")

# Using log scale due to heavily skewed data 
hist(log(recs$TOTALDOL), breaks = 60, col = "grey67", border = "white",
     main = "Annual energy cost, log scaled", xlab = "log(USD per year)")
par(old_par)

# Cost by region.
boxplot(TOTALDOL ~ REGIONC, data = recs, col = "grey65",
        main = "Annual energy cost by census region",
        xlab = "Census region", ylab = "USD per year")

# Cost by housing type.
boxplot(TOTALDOL ~ TYPEHUQ, data = recs, col = "grey65",
        names = c("Mobile", "SF detached", "SF attached",
                  "Apt 2-4", "Apt 5+"),
        main = "Annual energy cost by housing type",
        xlab = "Housing type", ylab = "USD per year")

# Cost by vintage(year built).
boxplot(TOTALDOL ~ vintage, data = recs, col = "grey65",
        main = "Annual energy cost by year built",
        xlab = "Year built", ylab = "USD per year")


# SECTION 4 - TWO-SAMPLE T-TEST ON Year built

# H0: homes built before 2000 and from 2000 onward have the same mean annual energy cost.
# H1: the two means differ.

cat("\nGroup descriptives:\n")
print(describeBy(recs$TOTALDOL, recs$vintage, mat = TRUE)[
  , c("group1", "n", "mean", "sd", "se")])

# Assumption 1: equal variances. Bartlett's test.
cat("\nBartlett test:\n")
print(bartlett.test(TOTALDOL ~ vintage, data = recs))

# Assumption 2: normality. QQ plot per group.
old_par <- par(mfrow = c(1, 2), mar = c(6, 4.5, 4, 2))
for (g in levels(recs$vintage)) {
  qqnorm(recs$TOTALDOL[recs$vintage == g],
         main = paste("Homes built", g),
         xlab = "Theoretical quantiles (normal)",
         ylab = "Observed annual cost (USD)")
  qqline(recs$TOTALDOL[recs$vintage == g])
  mtext("Points on the line = normally distributed",
        side = 1, line = 4.2, cex = 0.75)
}
par(old_par)



#Historgram function for more plots
hist_normal <- function(v, title, xlab = "Variance (USD per year)") {
  hist(v, breaks = 100, freq = FALSE, col = "grey65", border = "white",
       main = title, xlab = xlab)
  m <- mean(v)
  s <- sd(v)
  curve(dnorm(x, mean = m, sd = s), add = TRUE, col = "red", lwd = 2)
  legend("topright", legend = "Normal distribution", col = "red",
         lwd = 2, bty = "n")
}

old_par <- par(mfrow = c(1, 2))
for (g in levels(recs$vintage)) {
  hist_normal(recs$TOTALDOL[recs$vintage == g],
              paste("Homes built", g),
              xlab = "Annual energy cost (USD per year)")
}
par(old_par)



# Two-sample t-test
cat("\nTwo-sample t-test:\n")
t_vintage <- t.test(TOTALDOL ~ vintage, data = recs)
print(t_vintage)

# Practical significance: the difference against the size of a typical bill.
diff_means <- abs(diff(t_vintage$estimate))
cat("\nDifference in means:", round(diff_means, 2), "USD per year",
    sprintf("(%.1f%% of the mean bill)\n", 100 * diff_means / mean(recs$TOTALDOL)))

cat("\n Reject the null but the difference is only $66 a year, 3.3% of a typical bill.\n")


# SECTION 5  ONE-WAY ANOVA ACROSS REGIONS

# H0: mean annual energy cost is equal in all four census regions.
# H1: It differs

cat("\nGroup descriptives:\n")
print(describeBy(recs$TOTALDOL, recs$REGIONC, mat = TRUE)[
  , c("group1", "n", "mean", "sd", "se")])

aov_region <- aov(TOTALDOL ~ REGIONC, data = recs)

cat("\nOne-way ANOVA:\n")
print(summary(aov_region))

# Assumption 1: equal variances across groups.
# Assumption 2: normally distributed Residual values (Variances)
cat("\nBartlett test:\n")
print(bartlett.test(TOTALDOL ~ REGIONC, data = recs))




faint <- rgb(0, 0, 0, 0.07)

old_par <- par(mfrow = c(1, 2), mar = c(8, 4.2, 3.5, 1))


res_by_region <- split(residuals(aov_region), recs$REGIONC)
sds <- round(sapply(res_by_region, sd))

boxplot(res_by_region, col = "grey65", outline = FALSE,
        names = c("NE", "MW", "S", "W"),
        ylab = "Distance from the region's mean (USD)")
abline(h = 0, lty = 3)
title(main = "Spread even in each region?", cex.main = 0.95, line = 1.6)
mtext(paste("SD", sds), side = 3, at = 1:4, line = 0.1, cex = 0.58)

plot(aov_region, which = 2, caption = "", sub.caption = "", id.n = 0,
     pch = 16, cex = 0.3, col = faint)
title(main = "Are the errors normal?", cex.main = 0.95)
mtext("x = where a normal value would sit", side = 1, line = 3.9, cex = 0.62)
mtext("y = where it actually sits", side = 1, line = 4.8, cex = 0.62)
mtext("On the dashed line = normal", side = 1, line = 6.0, cex = 0.62)
mtext("Curving up at the right = expensive outliers", side = 1, line = 6.9, cex = 0.62)

par(old_par)

hist_normal(residuals(aov_region),
            "ANOVA Variances against a normal distribution")

# whar part does Variance play in cost when it comes to region,
# R squared
cat("\nR squared:", round(summary.lm(aov_region)$r.squared, 4), "\n")



# SECTION 6  SIMPLE LINEAR REGRESSION

# H0: floor area has no effect on annual energy cost (slope = 0).
# H1: It does has an effect.

# Correlation first: strength and direction before fitting a line.
cat("\nCorrelation, cost vs floor area:\n")
print(cor(recs$TOTSQFT_EN, recs$TOTALDOL))

lm_simple <- lm(TOTALDOL ~ TOTSQFT_EN, data = recs)

cat("\nSimple linear regression:\n")
print(summary(lm_simple))

# Slope in money terms.
slope <- coef(lm_simple)[2]
cat("\nEach extra square foot costs", round(slope, 3), "USD per year\n")
cat("A home 500 sq ft larger costs about", round(500 * slope), "USD more per year\n")

# The line of best fit.
off <- sum(recs$TOTSQFT_EN > 6000 | recs$TOTALDOL > 8000)

old_par <- par(mar = c(6.5, 4.5, 4, 2))

plot(recs$TOTSQFT_EN, recs$TOTALDOL,
     pch = 16, cex = 0.35, col = rgb(0, 0, 0, 0.09),
     xlim = c(0, 6000), ylim = c(0, 8000),
     main = "Larger homes cost more",
     xlab = "Energy-used floor area (square feet)",
     ylab = "Annual energy cost (USD per year)")

bands  <- cut(recs$TOTSQFT_EN, breaks = seq(0, 6000, by = 500))
mid_x  <- tapply(recs$TOTSQFT_EN, bands, mean)
mean_y <- tapply(recs$TOTALDOL,   bands, mean)

abline(lm_simple, col = "red", lwd = 2.5)
points(mid_x, mean_y, pch = 21, bg = "deepskyblue", col = "black", cex = 1.3)

legend("topleft", bty = "n", cex = 0.85,
       legend = c(
         sprintf("Line of best fit: cost = %.0f + %.2f x sq ft",
                 coef(lm_simple)[1], coef(lm_simple)[2]),
         sprintf("R squared = %.3f  (size explains %.0f%% of the variation)",
                 summary(lm_simple)$r.squared,
                 100 * summary(lm_simple)$r.squared),
         "Average cost per 500 sq ft band"),
       pch = c(NA, NA, 21), pt.bg = c(NA, NA, "deepskyblue"),
       lty = c(1, NA, NA), lwd = c(2.5, NA, NA),
       col = c("red", NA, "black"))

mtext(sprintf("Each dot is one household.",
              format(off, big.mark = ",")),
      side = 1, line = 4.3, cex = 0.72)
mtext("The vertical spread widens with size",
      side = 1, line = 5.3, cex = 0.72)

par(old_par)

# Assumptions.
old_par <- par(mfrow = c(1, 2), mar = c(8, 4.2, 3.5, 1))

plot(lm_simple, which = 1, caption = "", sub.caption = "", id.n = 0,
     pch = 16, cex = 0.3, col = faint, lwd = 2)
title(main = "Is the spread even?", cex.main = 0.95)
mtext("x = cost the model predicts for that home", side = 1, line = 3.9, cex = 0.62)
mtext("y = how far the household sits from it", side = 1, line = 4.8, cex = 0.62)
mtext("An even band = good", side = 1, line = 6.0, cex = 0.62)
mtext("A widening cone = spread grows with size", side = 1, line = 6.9, cex = 0.62)

plot(lm_simple, which = 2, caption = "", sub.caption = "", id.n = 0,
     pch = 16, cex = 0.3, col = faint)
title(main = "Are the errors normal?", cex.main = 0.95)
mtext("x = where a normal value would sit", side = 1, line = 3.9, cex = 0.62)
mtext("y = where it actually sits", side = 1, line = 4.8, cex = 0.62)
mtext("On the dashed line = normal", side = 1, line = 6.0, cex = 0.62)
mtext("Curving up at the right = costs underpredicted", side = 1, line = 6.9, cex = 0.62)

par(old_par)

hist_normal(residuals(lm_simple),
            "Regression Variances against a normal distribution")

# Outcome: floor area does predict cost, $0.47 per square foot per year, explaining 24.4% of it.


# SECTION 7 - MULTIPLE LINEAR REGRESSION

# H0: floor area and the two climate measures have no effect on annual cost.
# H1: at least one of them does.

cat("\nCorrelation matrix of the predictors:\n")
print(round(cor(recs[c("TOTSQFT_EN", "HDD65", "CDD65")]), 3))

lm_multi <- lm(TOTALDOL ~ TOTSQFT_EN + HDD65 + CDD65, data = recs)

cat("\nMultiple linear regression, raw dollars:\n")
print(summary(lm_multi))

cat("\nVariance inflation factors:\n")
print(vif(lm_multi))

# Assumption : Climate has little change to the cost
cat("\nRaw multiple model vs the simple model, same errors?\n")
cat("  correlation between the two error sets:",
    round(cor(residuals(lm_simple), residuals(lm_multi)), 4), "\n")
cat("  spread of the errors, simple :",
    round(sd(residuals(lm_simple)), 1), "USD\n")
cat("  spread of the errors, multi  :",
    round(sd(residuals(lm_multi)), 1), "USD\n")

hist_normal(residuals(lm_multi), "Raw model Variances against a normal curve")

# The assumptions failed, so transform the skewed outcome.
lm_multi_log <- lm(log(TOTALDOL) ~ TOTSQFT_EN + HDD65 + CDD65, data = recs)

cat("\nMultiple linear regression, log of cost:\n")
print(summary(lm_multi_log))

old_par <- par(mfrow = c(1, 2), mar = c(8, 4.2, 3.5, 1))

plot(lm_multi_log, which = 1, caption = "", sub.caption = "", id.n = 0,
     pch = 16, cex = 0.3, col = faint, lwd = 2)
title(main = "Log cost: spread even?", cex.main = 0.95)
mtext("x = log cost the model predicts", side = 1, line = 3.9, cex = 0.62)
mtext("y = how far the household sits from it", side = 1, line = 4.8, cex = 0.62)


plot(lm_multi_log, which = 2, caption = "", sub.caption = "", id.n = 0,
     pch = 16, cex = 0.3, col = faint)
title(main = "Log cost: errors normal?", cex.main = 0.95)
mtext("x = where a normal value would sit", side = 1, line = 3.9, cex = 0.62)
mtext("y = where it actually sits", side = 1, line = 4.8, cex = 0.62)

par(old_par)

hist_normal(residuals(lm_multi_log),
            "Log model Variances against a normal curve",
            xlab = "Variance (log USD)")

# The log model is better but still misses at the bottom.
cat("\nHouseholds with a bill under 100 USD:", sum(recs$TOTALDOL < 100), "\n")


# Coefficients of the log model read as percentages.
coef_log <- coef(lm_multi_log)
cat("\nEffect of each predictor on annual cost, log model:\n")
cat("  +100 sq ft :", round(100 * (exp(100 * coef_log["TOTSQFT_EN"]) - 1), 2), "%\n")
cat("  +100 HDD65 :", round(100 * (exp(100 * coef_log["HDD65"]) - 1), 2), "%\n")
cat("  +100 CDD65 :", round(100 * (exp(100 * coef_log["CDD65"]) - 1), 2), "%\n")


# Does climate add anything beyond floor area?
# Adjusted R squared
cat("\nAdjusted R squared, raw-dollar models:\n")
cat("  floor area only      :", round(summary(lm_simple)$adj.r.squared, 4), "\n")
cat("  plus heating/cooling :", round(summary(lm_multi)$adj.r.squared, 4), "\n")


# Prediction error on data the models have never seen
set.seed(123)
train_rows <- sample(nrow(recs), 0.8 * nrow(recs))
train <- recs[train_rows, ]
test  <- recs[-train_rows, ]

cat("\nTrain/test split:", nrow(train), "train,", nrow(test), "test\n")

fit_simple <- lm(TOTALDOL ~ TOTSQFT_EN, data = train)
fit_multi  <- lm(TOTALDOL ~ TOTSQFT_EN + HDD65 + CDD65, data = train)
fit_log    <- lm(log(TOTALDOL) ~ TOTSQFT_EN + HDD65 + CDD65, data = train)

# The log model predicts log dollars so exp() brings it back to dollars.
pred <- list(
  "floor area only"      = predict(fit_simple, newdata = test),
  "plus heating/cooling" = predict(fit_multi,  newdata = test),
  "log model"            = exp(predict(fit_log, newdata = test))
)

errors <- t(sapply(pred, function(p) {
  c(RMSE = sqrt(mean((test$TOTALDOL - p)^2)),
    MAE  = mean(abs(test$TOTALDOL - p)))
}))

cat("\nPrediction error on the test set, USD per year:\n")
print(round(errors, 2))

# Outcome: climate adds little very little to the cost



