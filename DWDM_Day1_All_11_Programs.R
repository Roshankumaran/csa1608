# SAVEETHA SCHOOL OF ENGINEERING
# DWDM - DAY 1 LAB EXERCISES (Q1-Q11)
# Run entire file using: Rscript DWDM_Day1_All_11_Programs.R
# Or open the file in RStudio and select Source.
# Graphs will be saved in DWDM_Day1_Plots.pdf in the working directory.

section <- function(n, question) {
  cat("\n", strrep("=", 65), "\n", sep = "")
  cat("QUESTION ", n, ": ", question, "\n", sep = "")
  cat(strrep("=", 65), "\n", sep = "")
}

# Q1: Approximate median from grouped age-frequency data
section(1, "Approximate median of grouped ages")
lower <- c(1, 5, 15, 20, 50, 80)
upper <- c(5, 15, 20, 50, 80, 110)
frequency <- c(200, 450, 300, 1500, 700, 44)
total <- sum(frequency)
cumulative <- cumsum(frequency)
median_bin <- which(cumulative >= total/2)[1]
prior <- if (median_bin == 1) 0 else cumulative[median_bin-1]
median_value <- lower[median_bin] +
  ((total/2 - prior)/frequency[median_bin]) *
  (upper[median_bin] - lower[median_bin])
cat("Approximate grouped median:", median_value, "\n")

# Q2: Descriptive statistics of ages
section(2, "Mean, median, mode, midrange and quartiles")
age <- c(13,15,16,16,19,20,20,21,22,22,25,25,25,25,
         30,33,33,35,35,35,35,36,40,45,46,52,70)
counts <- table(age)
modes <- as.numeric(names(counts)[counts == max(counts)])
cat("Mean:", mean(age), "\n")
cat("Median:", median(age), "\n")
cat("Mode(s):", paste(modes, collapse = ", "), "\n")
cat("Modality:", if (length(modes) == 2) "Bimodal" else
    if (length(modes) == 1) "Unimodal" else
    paste(length(modes), "modes"), "\n")
cat("Midrange:", (min(age) + max(age))/2, "\n")
cat("Q1:", as.numeric(quantile(age, 0.25)), "\n")
cat("Q3:", as.numeric(quantile(age, 0.75)), "\n")

# Q3: Data normalization
section(3, "Min-max and z-score normalization")
x <- c(200, 300, 400, 600, 1000)
cat("Min-max normalized values:\n")
print((x - min(x)) / (max(x) - min(x)))
cat("Z-score normalized values:\n")
print((x - mean(x))/sd(x))

# Q4: Equal-frequency smoothing by bin mean, median and boundaries
section(4, "Smoothing by bin means, medians and boundaries")
data <- c(11,13,13,15,15,16,19,20,20,20,21,21,
          22,23,24,30,40,45,45,45,71,72,73,75)
bins <- split(sort(data), rep(1:4, each = 6))
cat("Bin means:\n")
print(sapply(bins, mean))
cat("Bin medians:\n")
print(sapply(bins, median))
cat("Boundary-smoothed bins:\n")
print(lapply(bins, function(v) {
  low <- min(v); high <- max(v)
  ifelse(v - low <= high - v, low, high)
}))

# Q5: Hospital age and body fat statistics and charts
section(5, "Age and body-fat statistics, boxplot, scatter and Q-Q plots")
hospital_age <- c(23,23,27,27,39,41,47,49,50,
                  52,54,54,56,57,58,58,60,61)
fat <- c(9.5,26.5,7.8,17.8,31.4,25.9,27.4,27.2,31.2,
         34.6,42.5,28.8,33.4,30.2,34.1,32.9,41.2,35.7)
cat("Age mean:", mean(hospital_age), "\n")
cat("Age median:", median(hospital_age), "\n")
cat("Age standard deviation:", sd(hospital_age), "\n")
cat("Body-fat mean:", mean(fat), "\n")
cat("Body-fat median:", median(fat), "\n")
cat("Body-fat standard deviation:", sd(fat), "\n")

# Q6: Normalize 35 based on hospital age data
section(6, "Normalize age 35 by min-max, z-score and decimal scaling")
value <- 35
cat("Min-max:", (value-min(hospital_age)) /
    (max(hospital_age)-min(hospital_age)), "\n")
cat("Z-score:", (value-mean(hospital_age))/12.94, "\n")
j <- floor(log10(max(abs(hospital_age)))) + 1
cat("Decimal scaling:", value/10^j, "\n")

# Q7: Pencil statistics
section(7, "Mean, median and mode of pencil counts")
pencils <- c(9,25,23,12,11,6,7,8,9,10)
pencil_counts <- table(pencils)
cat("Mean:", mean(pencils), "\n")
cat("Median:", median(pencils), "\n")
cat("Mode:", paste(names(pencil_counts)[pencil_counts == max(pencil_counts)],
                   collapse = ", "), "\n")

# Q8: Mobile sales scatter plot
section(8, "Scatter plot of mobile phones sold versus money")
phones <- c(4,1,5,7,10,2,50,25,90,36)
money <- c(12,5,13,19,31,7,153,72,275,110)
cat("Scatter plot will be saved to the PDF.\n")

# Q9: Equal-frequency and equal-width bins of student marks
section(9, "Equal-frequency and equal-width partitioning and histogram")
marks <- c(55,60,71,63,55,65,50,55,58,
           59,61,63,65,67,71,72,75)
ordered_marks <- sort(marks)
equal_frequency <- split(ordered_marks, rep(1:3, times = c(6,6,5)))
cat("Equal-frequency bins:\n")
print(equal_frequency)
equal_width <- split(ordered_marks,
    cut(ordered_marks, breaks = seq(min(marks), max(marks), length.out = 4),
        include.lowest = TRUE))
cat("Equal-width bins:\n")
print(equal_width)

# Q10: Interquartile range and sample standard deviation
section(10, "IQR and standard deviation of car speeds")
speed <- c(78.3,81.8,82,74.2,83.4,84.5,82.9,77.5,80.9,70.6)
cat("IQR:", IQR(speed), "\n")
cat("Standard deviation:", sd(speed), "\n")

# Q11: Quartiles of ages
section(11, "First and third quartiles of age data")
cat("Q1:", as.numeric(quantile(age, 0.25)), "\n")
cat("Q3:", as.numeric(quantile(age, 0.75)), "\n")

# Save every requested graphical output, including Q5, Q8 and Q9.
pdf("DWDM_Day1_Plots.pdf", width = 8, height = 6)
boxplot(hospital_age, fat, names = c("Age", "Body Fat (%)"),
        main = "Q5 - Age and Body Fat Boxplots", col = c("skyblue", "pink"))
scatter.smooth(hospital_age, fat, main = "Q5 - Age vs Body Fat",
               xlab = "Age", ylab = "Body Fat (%)")
qqplot(hospital_age, fat, main = "Q5 - Age vs Body Fat Q-Q Plot",
       xlab = "Age Quantiles", ylab = "Body Fat Quantiles")
scatter.smooth(phones, money, main = "Q8 - Mobile Sales",
               xlab = "Mobile Phones Sold", ylab = "Money")
hist(marks, breaks = 3, main = "Q9 - Student Marks Histogram",
     xlab = "Marks", col = "lightblue")
dev.off()
cat("\nCompleted all 11 questions. Graphs saved to DWDM_Day1_Plots.pdf\n")
