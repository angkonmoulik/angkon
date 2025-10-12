x <- c(790,645,887,866,758,670,571,978,339,852,663,771,803,585)

n <- length(x)
mu0 <- 700
sigma <- sqrt(2500)
xbar <- mean(x)
xbar
# Test statistic
z <- (xbar - mu0) / (sigma / sqrt(n))
z
# p-values
p_left  <- pnorm(z)
p_right <- 1 - pnorm(z)
p_two   <- 2 * (1 - pnorm(abs(z)))

# 95% Confidence Interval (2-tail test এর ক্ষেত্রে)
z_tab <- qnorm(0.975)
ci_lower <- xbar - z_tab * sigma / sqrt(n)
ci_upper <- xbar + z_tab * sigma / sqrt(n)

z_tab
ci_lower
ci_upper
ii)
x <- c(790,645,887,866,758,670,571,978,339,852,663,771,803,585)
n <- length(x)
mu0 <- 700

# summary
xbar <- mean(x)
s <- sd(x)            # sample standard deviation
df <- n - 1

# t-statistic
t_stat <- (xbar - mu0) / (s / sqrt(n))

# p-values using pt (Student t CDF)
p_left  <- pt(t_stat, df)            # P(T <= t)
p_right <- 1 - pt(t_stat, df)        # P(T >= t)
p_two   <- 2 * (1 - pt(abs(t_stat), df))  # two-sided

# 95% CI
t_tab <- qt(0.975, df)
ci_lower <- xbar - t_tab * s / sqrt(n)
ci_upper <- xbar + t_tab * s / sqrt(n)

list(n = n, mean = xbar, s = s, df = df,
     t = t_stat, p_left = p_left, p_right = p_right, p_two = p_two,
     CI95 = c(ci_lower, ci_upper))

x <- c(790,645,887,866,758,670,571,978,339,852,663,771,803,585)
y <- c(471,382,455,459,460,567,554,458,549,345,258,767)

mean_x <- mean(x)
mean_y <- mean(y)
n1 <- length(x)
n2 <- length(y)

sigma <- 100
z <- (mean_x - mean_y) / (sigma * sqrt(1/n1 + 1/n2))
p_less <- pnorm(z)
p_greater <- 1 - pnorm(z)
p_two <- 2 * (1 - pnorm(abs(z)))

list(z = z, p_two = p_two, p_less = p_less, p_greater = p_greater)
known,eq
x <- c(790,645,887,866,758,670,571,978,339,852,663,771,803,585)
y <- c(471,382,455,459,460,567,554,458,549,345,258,767)

mean_x <- mean(x)
mean_y <- mean(y)
n1 <- length(x)
n2 <- length(y)

sigma <- 100
z <- (mean_x - mean_y) / (sigma * sqrt(1/n1 + 1/n2))
p_less <- pnorm(z)
p_greater <- 1 - pnorm(z)
p_two <- 2 * (1 - pnorm(abs(z)))

list(z = z, p_two = p_two, p_less = p_less, p_greater = p_greater)
known,uneq
x <- c(790,645,887,866,758,670,571,978,339,852,663,771,803,585)
y <- c(471,382,455,459,460,567,554,458,549,345,258,767)

mean_x <- mean(x)
mean_y <- mean(y)
n1 <- length(x)
n2 <- length(y)

sigma1 = 100
sigma2 = 120

z <- (mean_x - mean_y) / sqrt((sigma1^2)/n1 + (sigma2^2)/n2)
p_less <- pnorm(z)
p_greater <- 1 - pnorm(z)
p_two <- 2 * (1 - pnorm(abs(z)))
list(z = z, p_two = p_two, p_less = p_less, p_greater = p_greater)
un,eq
x <- c(790,645,887,866,758,670,571,978,339,852,663,771,803,585)
y <- c(471,382,455,459,460,567,554,458,549,345,258,767)

mean_x <- mean(x)
mean_y <- mean(y)
var_x  <- var(x)
var_y  <- var(y)
n1 <- length(x)
n2 <- length(y)

sp <- sqrt(((n1 - 1)*var_x + (n2 - 1)*var_y) / (n1 + n2 - 2))
t_stat <- (mean_x - mean_y) / (sp * sqrt(1/n1 + 1/n2))
df <- n1 + n2 - 2

p_less <- pt(t_stat, df)
p_greater <- 1 - pt(t_stat, df)
p_two <- 2 * (1 - pt(abs(t_stat), df))
list(t = t_stat, df = df, p_two = p_two, p_less = p_less, p_greater = p_greater)

un,uneq
x <- c(790,645,887,866,758,670,571,978,339,852,663,771,803,585)
y <- c(471,382,455,459,460,567,554,458,549,345,258,767)

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
several
v1 <- c(790,645,887,866,758,670,571,978,339,852,663,771,803,585)
v2 <- c(471,382,455,459,460,567,554,458,549,345,258,767)
v3 <- c(786,687,976,968,879,699,870,476,275,397,420,501,785)
v4 <- c(349,354,359,366,369,439,361,450,546,478,450,556,538)

# Combine the data into a data frame
data <- data.frame(value = c(v1, v2, v3, v4),
                   group = factor(rep(c("v1", "v2", "v3", "v4"),
                                      times = c(length(v1), length(v2), length(v3), length(v4)))))
#data
# Perform ANOVA
anova_result <- aov(value ~ group, data = data)

# Display the ANOVA summary
summary(anova_result)
single var
x = c(790,645,887,866,758,670,571,978,339,852,663,771,803,585)
sigma_0 = 100
n = length(x)
s2 = var(x)

df <- n - 1
chi2_stat = (n - 1)*s2/(sigma_0^2)

# one-sided / two-sided p-values
p_less    <- pchisq(chi2_stat, df)           # P(Chi2 <= chi2_stat)
p_greater <- 1 - pchisq(chi2_stat, df)       # P(Chi2 >= chi2_stat)
p_two     <- 2 * min(p_less, p_greater)      # two-sided (conservative)


data.frame(chisq = chi2_stat, df, p_less, p_greater, p_two)
two
1 <- c(790,645,887,866,758,670,571,978,339,852,663,771,803,585)
v2 <- c(471,382,455,459,460,567,554,458,549,345,258,767)

s1_sq <- var(v1)
s2_sq <- var(v2)
n1 <- length(v1)
n2 <- length(v2)

F_stat <- s1_sq / s2_sq
df1 <- n1 - 1
df2 <- n2 - 1

# One-tailed p-values (if needed)
p_greater <- 1 - pf(F_stat, df1, df2)  # H1: sigma1^2 > sigma2^2
p_less <- pf(F_stat, df1, df2)         # H1: sigma1^2 < sigma2^2

# Two-tailed p-value
p_two <- 2 * min(p_less, p_greater)

data.frame(F = F_stat, df1, df2, p_two, p_less, p_greater)
v1 <- c(790,645,887,866,758,670,571,978,339,852,663,771,803,585)
v2 <- c(471,382,455,459,460,567,554,458,549,345,258,767)
v3 <- c(786,687,976,968,879,699,870,476,275,397,420,501,785)
v4 <- c(349,354,359,366,369,439,361,450,546,478,450,556,538)

# Combine into one data frame
data <- data.frame(
  value = c(v1, v2, v3, v4),
  group = factor(rep(c("v1", "v2", "v3", "v4"),
                     times = c(length(v1), length(v2), length(v3), length(v4)))
  )
)


# 1️⃣ Bartlett's test
bartlett.test(value ~ group, data = data)
zero corr
# Data
hours_studied <- c(10, 15, 12, 20, 8, 18, 25, 5, 16, 22, 14, 9, 28, 11, 19)
gpa <- c(3.0, 3.5, 3.3, 3.8, 2.5, 3.7, 4.0, 2.0, 3.6, 3.9, 3.4, 2.8, 4.0, 3.1, 3.8)

r <- cor(hours_studied, gpa)

# Step 2: Compute t-statistic
n <- length(hours_studied)
t_stat <- r * sqrt(n - 2) / sqrt(1 - r^2)
df <- n - 2

# Right-tailed test (H1: rho > 0)
p_right <- 1 - pt(t_stat, df = df)

# Left-tailed test (H1: rho < 0)
p_left <- pt(t_stat, df = df)

# Two-tailed test (H1: rho ≠ 0)
p_two <- 2 * (1 - pt(abs(t_stat), df = df))

data.frame(r, t_stat, df, p_left, p_right, p_two)
value
hours_studied <- c(10, 15, 12, 20, 8, 18, 25, 5, 16, 22, 14, 9, 28, 11, 19)
gpa <- c(3.0, 3.5, 3.3, 3.8, 2.5, 3.7, 4.0, 2.0, 3.6, 3.9, 3.4, 2.8, 4.0, 3.1, 3.8)

n <- length(hours_studied)

# Sample correlation
r <- cor(hours_studied, gpa)

# Hypothesized correlation
rho0 <- 0.8

# Step 1: Fisher's z-transform
z_r <- 0.5 * log((1 + r) / (1 - r))
z_rho0 <- 0.5 * log((1 + rho0) / (1 - rho0))

# Step 2: Test statistic
z_stat <- (z_r - z_rho0) * sqrt(n - 3)

# Step 3: Two-tailed p-value
p_two <- 2 * (1 - pnorm(abs(z_stat)))

# Step 4: One-tailed p-values
p_right <- 1 - pnorm(z_stat)   # H1: r > rho0
p_left  <- pnorm(z_stat)       # H1: r < rho0


data.frame(r, z_stat, p_left, p_right, p_two)
two
# Sample data
hours1 <- c(10, 12, 14, 16, 18, 20, 22, 24)
gpa1   <- c(2.8, 3.0, 3.2, 3.4, 3.5, 3.7, 3.9, 4.0)

hours2 <- c(8, 10, 12, 14, 16, 18, 20, 22)
gpa2   <- c(2.5, 2.9, 3.0, 3.2, 3.4, 3.6, 3.6, 3.7)

# Step 1: Compute correlations
r1 <- cor(hours1, gpa1)
r2 <- cor(hours2, gpa2)

# Step 2: Sample sizes
n1 <- length(hours1)
n2 <- length(hours2)

# Step 3: Fisher z-transform
z1 <- 0.5 * log((1 + r1) / (1 - r1))
z2 <- 0.5 * log((1 + r2) / (1 - r2))

# Step 4: Standard error
SE <- sqrt(1/(n1 - 3) + 1/(n2 - 3))

# Step 5: Test statistic
Z <- (z1 - z2) / SE

# Step 6: p-values
p_right <- 1 - pnorm(Z)
p_left <- pnorm(Z)
p_two <- 2 * (1 - pnorm(abs(Z)))

data.frame(r1, r2, Z, p_left, p_right, p_two)
several
1 <- c(10,12,14,16,18,20,22,24); y1 <- c(2.8,3.0,3.2,3.4,3.5,3.7,3.9,4.0)
x2 <- c(8,10,12,14,16,18,20,22);  y2 <- c(2.5,2.9,3.0,3.2,3.4,3.6,3.6,3.7)
x3 <- c(9,11,13,15,17,19,21,23);  y3 <- c(2.7,2.8,3.1,3.3,3.4,3.5,3.6,3.8)

# compute r and n per group
rs <- c(cor(x1,y1), cor(x2,y2), cor(x3,y3))
ns <- c(length(x1), length(x2), length(x3))

# then reuse the same test
z  <- 0.5 * log((1 + rs)/(1 - rs))
w  <- ns - 3
zbar <- sum(w * z) / sum(w)
chi_sq <- sum(w * (z - zbar)^2)
p_value <- 1 - pchisq(chi_sq, df = length(rs)-1)

data.frame(r = round(rs,3), n = ns, z = round(z,4), w = w)
cat("chi_sq =", round(chi_sq,4), " df =", length(rs)-1, " p-value =", round(p_value,4), "\n")
