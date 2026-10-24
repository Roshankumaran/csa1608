# DWDM DAY 2 - ALL 10 EXERCISES
# Run entire file in RStudio using Source (Ctrl+Shift+Enter).
# Outputs appear in Console; plots saved as DWDM_Day2_Plots.pdf in getwd().
# Question 4 contains a truncated value in the provided document: assumed $80,000.
# Question 3 contains duplicated/concatenated marks: nine distinct listed values used per class.
# Question 10 needs diabetes.csv with columns Age and BloodPressure.

options(width = 100)
plot_file <- file.path(getwd(), "DWDM_Day2_Plots.pdf")
pdf(plot_file, width = 9, height = 6)

run_question <- function(number, title, code) {
  cat("\n", strrep("=", 60), "\n", sep = "")
  cat("QUESTION ", number, ": ", title, "\n", sep = "")
  cat(strrep("=", 60), "\n")
  tryCatch(code(), error = function(e) cat("ERROR: ", conditionMessage(e), "\n", sep = ""))
}

run_question(1, "Covariance and correlation of photograph preferences", function() {
  preferences <- data.frame(A = c(18, 2, 20), B = c(22, 28, 10), C = c(20, 40, 40))
  print(preferences)
  cat("Covariance B,C:\n"); print(cov(preferences$B, preferences$C))
  cat("Covariance matrix:\n"); print(cov(preferences))
  cat("Correlation B,C:\n"); print(cor(preferences$B, preferences$C))
  cat("Correlation matrix:\n"); print(cor(preferences))
  cat("Interpretation: Only three aggregated age-group rows; preferences are categorical counts,\n")
  cat("so correlations of counts do NOT establish a significant relationship at child level.\n")
  cat("A chi-squared test on the underlying contingency table would be more appropriate.\n")
  counts <- as.matrix(preferences)
  print(suppressWarnings(chisq.test(counts)))
})

run_question(2, "Equal-frequency binning and smoothing of prices", function() {
  prices <- c(1,1,5,5,5,5,5,8,8,10,10,10,10,12,14,14,14,
              15,15,15,15,15,15,18,18,18,18,18,18,18,
              20,20,20,20,20,20,20,21,21,21,21,25,25,25,25,25,28,28,30,30,30)
  prices <- sort(prices)
  indices <- split(seq_along(prices), rep(1:3, length.out = length(prices)))
  # Use contiguous equal-frequency groups of approximately equal sizes.
  indices <- split(seq_along(prices), rep(1:3, times = c(17,17,length(prices)-34)))
  bins <- lapply(indices, function(i) prices[i])
  cat("Original sorted values (", length(prices), "):\n", sep = ""); print(prices)
  cat("Equal-frequency bins:\n"); print(bins)
  means <- vapply(bins, mean, numeric(1))
  cat("Bin means:\n"); print(means)
  smooth_mean <- unlist(Map(function(v,m) rep(m,length(v)), bins, means), use.names = FALSE)
  cat("Smoothing by bin means:\n"); print(smooth_mean)
  smooth_boundary <- unlist(lapply(bins, function(v) {
    low <- min(v); high <- max(v)
    ifelse(abs(v-low) <= abs(v-high), low, high)
  }), use.names = FALSE)
  cat("Smoothing by bin boundaries:\n"); print(smooth_boundary)
  hist(prices, breaks = 10, main = "Q2: Item Price Histogram", xlab = "Price ($)", col = "lightblue")
})

run_question(3, "Compare marks of Classes A and B", function() {
  # The source repeats concatenated numbers (8476 and 5051); using the nine intended marks.
  class_a <- c(76,35,47,64,95,66,89,36,84)
  class_b <- c(51,56,84,60,59,70,63,66,50)
  result <- rbind(Class_A = c(mean = mean(class_a), median = median(class_a), range = diff(range(class_a))),
                  Class_B = c(mean = mean(class_b), median = median(class_b), range = diff(range(class_b))))
  print(result)
  cat("Higher mean: ", rownames(result)[which.max(result[,"mean"])], "\n", sep = "")
  cat("Higher median: ", rownames(result)[which.max(result[,"median"])], "\n", sep = "")
  cat("Higher range: ", rownames(result)[which.max(result[,"range"])], "\n", sep = "")
  boxplot(list(Class_A = class_a, Class_B = class_b), main = "Q3: Class Marks Boxplot",
          ylab = "Marks", col = c("lightblue", "lightgreen"))
  cat("Interpretation: Class A has higher scores on average, with greater spread.\n")
})

run_question(4, "Min-max and z-score normalization", function() {
  # Source says '$80,'; assume it means $80,000 in a $50,000-$100,000 feature.
  value <- 80000; minimum <- 50000; maximum <- 100000
  cat("Normalized $80,000: ", (value-minimum)/(maximum-minimum), "\n", sep = "")
  x <- c(200,300,400,600,1000)
  cat("Min-max normalized:\n"); print((x-min(x))/(max(x)-min(x)))
  cat("Z-score normalized (R sample standard deviation):\n"); print((x-mean(x))/sd(x))
})

run_question(5, "AirPassengers histogram", function() {
  data("AirPassengers")
  x <- as.numeric(AirPassengers)
  # Requested bin width 150, covering roughly 100 through 700.
  breaks <- seq(100, 700, by = 150)
  hist(x, breaks = breaks, xlim = c(100,700), col = "lightblue",
       main = "Q5: AirPassengers Histogram", xlab = "Monthly passengers (thousands)")
  cat("Histogram generated: x-axis starts at 100 and bins are 150 wide.\n")
})

run_question(6, "Multiple lines using mtcars mpg and qsec", function() {
  data("mtcars")
  plot(mtcars$mpg, type = "l", col = "blue", lwd = 2,
       xlab = "Car Index", ylab = "Value", main = "Q6: MPG and QSEC")
  lines(mtcars$qsec, col = "red", lwd = 2)
  legend("topright", legend = c("mpg", "qsec"), col = c("blue", "red"), lty = 1, bty = "n")
  cat("Line graph generated for mpg and qsec (32 cars).\n")
})

run_question(7, "Water hardness vs mortality regression", function() {
  if (!requireNamespace("MASS", quietly = TRUE)) {
    cat("MASS package not found. In Console run: install.packages('MASS')\n")
    return(invisible(NULL))
  }
  data("water", package = "MASS", envir = environment())
  model <- lm(mortality ~ hardness, data = water)
  print(summary(model))
  prediction <- predict(model, newdata = data.frame(hardness = 88))
  cat("Predicted mortality when hardness = 88:\n"); print(prediction)
  plot(water$hardness, water$mortality, pch = 19, col = "blue",
       xlab = "Hardness", ylab = "Mortality", main = "Q7: Hardness vs Mortality")
  abline(model, col = "red", lwd = 2)
})

run_question(8, "Boxplot mtcars mpg by number of cylinders", function() {
  data("mtcars")
  boxplot(mpg ~ factor(cyl), data = mtcars, col = "lightgreen",
          xlab = "Number of cylinders", ylab = "Miles per gallon",
          main = "Q8: MPG by Cylinders")
  print(tapply(mtcars$mpg, mtcars$cyl, summary))
})

run_question(9, "Boxplot showing tennis score outliers", function() {
  # Illustrative scores, as the source specifies no actual score dataset.
  scores <- c(8,10,11,12,13,14,14,15,15,16,17,18,19,20,42)
  cat("Example scores:\n"); print(scores)
  cat("Potential outliers (1.5 x IQR method):\n"); print(boxplot.stats(scores)$out)
  boxplot(scores, horizontal = TRUE, col = "lightyellow", xlab = "Points scored",
          main = "Q9: Tennis Player Scores and Outliers")
})

run_question(10, "Diabetes.csv: BloodPressure versus Age", function() {
  csv_path <- file.path(getwd(), "diabetes.csv")
  if (!file.exists(csv_path)) {
    cat("DATA FILE MISSING: Place diabetes.csv in the R working directory:\n", getwd(), "\n")
    cat("It must contain columns Age and BloodPressure. Q10 is skipped until provided.\n")
    return(invisible(NULL))
  }
  diabetes <- read.csv(csv_path)
  if (!all(c("Age", "BloodPressure") %in% names(diabetes))) {
    cat("CSV must contain columns named Age and BloodPressure. Available columns:\n")
    print(names(diabetes)); return(invisible(NULL))
  }
  diabetes <- diabetes[complete.cases(diabetes[, c("Age", "BloodPressure")]), ]
  plot(diabetes$Age, diabetes$BloodPressure, pch = 19, col = "blue",
       main = "Q10: BloodPressure vs Age", xlab = "Age", ylab = "BloodPressure")
  groups <- cut(diabetes$Age, breaks = c(-Inf,20,30,40,50,60,Inf),
                labels = c("<=20","21-30","31-40","41-50","51-60",">60"))
  avg <- tapply(diabetes$BloodPressure, groups, mean, na.rm = TRUE)
  print(avg)
  barplot(avg, col = "lightblue", main = "Q10: Mean BloodPressure by Age Group",
          xlab = "Age Group", ylab = "Mean BloodPressure")
})

dev.off()
cat("\nALL 10 EXERCISES PROCESSED.\n")
cat("Saved graphs to: ", plot_file, "\n", sep = "")
cat("To check the working directory, run: getwd()\n")
