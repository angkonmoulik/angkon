 x1bar=1258
 x2bar=1243
 s1=34
 s2=28
 n1=40
 n2=60
 zcal=(x1bar - x2bar) / sqrt((s1^2)/n1 + (s2^2)/n2)
 zcal
 z_tab=qnorm(0.975)
 z_tab
 p_two <- 2 * (1 - pnorm(abs(zcal)))
 p_two
 
 
 x=c(6.2,5.7,6.5,6.0,6.3,5.8,5.7,6.0,6.0,5.8)
 y=c(5.6,5.9,5.6,5.7,5.8,5.7,6.0,5.5,5.7,5.5)
 mean_x <- mean(x)
 mean_y <- mean(y)
 var_x  <- var(x)
 var_y  <- var(y)
 n1 <- length(x)
 n2 <- length(y)
 t_stat <- (mean_x - mean_y) / sqrt(var_x/n1 + var_y/n2)
 df <- ( (var_x/n1 + var_y/n2)^2 ) / ( ((var_x/n1)^2)/(n1-1) + ((var_y/n2)^2)/(n2-1) )
 
 p_two <- 2 * (1 - pt(abs(t_stat), df))
 p_less <- pt(t_stat, df)
 p_greater <- 1 - pt(t_stat, df)
 list(t = t_stat, df = df, p_two = p_two, p_less = p_less, p_greater = p_greater)
 tcrit=qt(0.95,df=n1+n2-2)
 tcrit
lower=(mean_x-mean_y)-tcrit*sqrt(((n-1)*(sd(x))^2+(n2-1)*(sd(y))^2)/(n1+n2-2))*sqrt(1/n1+1/n2)
upper=(mean_x-mean_y)+tcrit*sqrt(((n-1)*(sd(x))^2+(n2-1)*(sd(y))^2)/(n1+n2-2))*sqrt(1/n1+1/n2)
c(lower,upper)

library(stats)
x <- c(790,645,887,866,758,670,571,978,339,852,663,771,803,585)
y <- c(471,382,455,459,460,567,554,458,549,345,258,767)
ks.test(x,y,alternative = "two.sided")
ts=ks.test(x,y,alternative = "two.sided")$statistic
ts
p.ts=ks.test(x,y,alternative = "two.sided")$p.value
p.ts

v1 <- c(790,645,887,866,758,670,571,978,339,852,663,771,803,585)
v2 <- c(471,382,455,459,460,567,554,458,549,345,258,767,987,876)
v3 <- c(786,687,976,968,879,699,870,476,275,397,420,501,785,854)
v4 <- c(349,354,359,366,369,439,361,450,546,478,450,556,538,895)
v5=   c(454,754,688,554,787,846,458,525,245,245,344,854,687,568)
data <- data.frame(value = c(v1, v2, v3, v4,v5),
                   group = factor(rep(c("v1", "v2", "v3", "v4","v5"),
                                      times = c(length(v1), length(v2), length(v3), length(v4),length(v5)))))
boxplot(value~group,data=data)
kruskal.test(value~group,data=data)

# ---------------------------------------------
# Homogeneity test for correlation coefficients
# ---------------------------------------------

# Given data
n  <- c(21, 26, 19, 28, 25)
r  <- c(0.39, 0.61, 0.43, 0.54, 0.48)
k  <- length(r)

# Fisher's z transformation
z  <- 0.5 * log((1 + r) / (1 - r))

# Variance of each z_i
var_z <- 1 / (n - 3)

# Weighted mean (estimate of common z)
z_bar <- sum(z / var_z) / sum(1 / var_z)

# Chi-square statistic for homogeneity
chi_sq <- sum((z - z_bar)^2 / var_z)
df <- k - 1
p_value <- 1 - pchisq(chi_sq, df)

# Estimate of pooled correlation (r_bar)
r_bar <- (exp(2 * z_bar) - 1) / (exp(2 * z_bar) + 1)

# Output
cat("Test for homogeneity of correlations\n")
cat("-----------------------------------\n")
cat("Chi-square =", chi_sq, "\n")
cat("Degrees of freedom =", df, "\n")
cat("p-value =", p_value, "\n\n")
cat("Estimated common correlation (r_bar) =", r_bar, "\n")

# Test if first sample could have rho = 0
# H0: rho = 0 for first sample
r1 <- r[1]; n1 <- n[1]
t_stat <- r1 * sqrt((n1 - 2) / (1 - r1^2))
p_val_r1 <- 2 * (1 - pt(abs(t_stat), df = n1 - 2))

cat("\nTest for first sample (H0: rho = 0)\n")
cat("-----------------------------------\n")
cat("t =", t_stat, "\n")
cat("p-value =", p_val_r1, "\n")

x <- c(21, 26, 19, 28, 25)
y <- c(0.39, 0.61, 0.43, 0.54, 0.48)

cor.test(x, y, alternative = "two.sided", method = "pearson")
23-2
# Given parameters
sigma <- sqrt(0.5)     # Standard deviation
mu0 <- 5               # Under H0
mu1 <- 7               # Under H1
x_crit <- 7            # Critical region: reject if X > 7

# (i) Level of significance (alpha)
alpha <- 1 - pnorm(x_crit, mean = mu0, sd = sigma)
alpha

# (ii) Power function (general form)
power_function <- function(mu) {
  1 - pnorm(x_crit, mean = mu, sd = sigma)
}

# Evaluate power for a range of mu values
mu_vals <- seq(4, 8, by = 0.1)
power_vals <- power_function(mu_vals)

# Plot power curve
plot(mu_vals, power_vals, type = "l", col = "blue", lwd = 2,
     main = "Power Curve of the Test",
     xlab = expression(mu), ylab = "Power")

# (iii) Compute power of the test when mu = 7
power_mu7 <- power_function(mu1)
power_mu7
# Create the contingency table
votes <- matrix(c(520, 280, 450, 350),
                nrow = 2,
                byrow = TRUE)

# Add row and column names
rownames(votes) <- c("Rural", "Urban")
colnames(votes) <- c("A", "B")

# Display the table
votes

# Perform Chi-square test of independence
test_result <- chisq.test(votes, correct = FALSE)

# Show results
test_result

# Expected frequencies
test_result$expected
