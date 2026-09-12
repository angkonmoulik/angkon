# =====================================================================
# B.Sc. (Hons) Part-IV Practical Examination, 2024
# Department of Statistics, University of Rajshahi
# Course: B.Stat-411, Session-I: Multivariate Analysis  --  SET - 1
# =====================================================================
# NOTE ON DATA:
# The question refers to a supplied file "performance.csv" containing
# 500 students with:
#   Lifestyle set X : sleep.hrs, social.evt, exercise.hrs
#   Academic set  Y : gpa.scr, study.hrs, project.scr, library.vst
#
# If you already HAVE performance.csv, delete the "SIMULATE DATA" block
# below and simply do:  data1 <- read.csv("performance.csv")
# The simulation block only exists so this script runs end-to-end for
# practice/checking; replace it with your real file before submission.
# =====================================================================

## ---- 0. Packages ----
required_pkgs <- c("CCA", "CCP", "psych", "fastICA", "MASS")
new_pkgs <- required_pkgs[!(required_pkgs %in% installed.packages()[, "Package"])]
if (length(new_pkgs)) install.packages(new_pkgs)
invisible(lapply(required_pkgs, library, character.only = TRUE))

## ---- SIMULATE DATA (remove this block if real performance.csv exists) ----
set.seed(123)
n <- 500
sleep.hrs    <- rnorm(n, 7, 1)
social.evt   <- rpois(n, 3)
exercise.hrs <- rnorm(n, 5, 1.5)
gpa.scr     <- pmin(4, pmax(0, 2 + 0.15*sleep.hrs - 0.05*social.evt + 0.10*exercise.hrs + rnorm(n, 0, 0.3)))
study.hrs   <- pmax(0, 2 + 0.3*gpa.scr + rnorm(n, 0, 0.5))
project.scr <- pmin(100, pmax(0, 60 + 8*gpa.scr + rnorm(n, 0, 5)))
library.vst <- pmax(0, round(2 + 1.5*study.hrs + rnorm(n, 0, 1)))
performance <- data.frame(sleep.hrs, social.evt, exercise.hrs,
                           gpa.scr, study.hrs, project.scr, library.vst)
write.csv(performance, "performance.csv", row.names = FALSE)
## ---------------------------------------------------------------------

# =====================================================================
# QUESTION 1: Canonical Correlation Analysis (CCA)
# =====================================================================

## a) Read CSV; summary statistics & internal structure
data1 <- read.csv("performance.csv")
summary(data1)
str(data1)

## b) Split into X (Lifestyle) and Y (Academic performance)
X <- data1[, c("sleep.hrs", "social.evt", "exercise.hrs")]
Y <- data1[, c("gpa.scr", "study.hrs", "project.scr", "library.vst")]

## c) Correlations within each set
cor(X)
cor(Y)
# Comment: Low/moderate correlations within X (and within Y) indicate the
# variables inside each set are not badly redundant with one another,
# which is a healthy pre-condition for CCA (severe multicollinearity
# within a set would make canonical coefficients unstable).

## d) Canonical Correlation Analysis
cc_res <- cc(X, Y)
cc_res$cor        # the canonical correlations (largest first)
cc_res            # full output

## e) Canonical coefficients and canonical loadings
cc_res$xcoef                       # raw canonical coefficients for X
cc_res$ycoef                       # raw canonical coefficients for Y
load_res <- comput(X, Y, cc_res)
load_res$corr.X.xscores            # canonical loadings: X vars vs X-variates
load_res$corr.Y.yscores            # canonical loadings: Y vars vs Y-variates
# Comment: Coefficients give the (standardized) weight of each raw variable
# in forming a canonical variate; loadings (structure correlations) show
# how strongly each ORIGINAL variable correlates with its own canonical
# variate and are usually more reliable for substantive interpretation,
# since coefficients can be unstable under multicollinearity.

## f) Plot the pairs of canonical variates
U <- as.matrix(scale(X)) %*% cc_res$xcoef      # X-side canonical variates
V <- as.matrix(scale(Y)) %*% cc_res$ycoef      # Y-side canonical variates

plot(U[, 1], V[, 1],
     xlab = "1st Canonical Variate (X)", ylab = "1st Canonical Variate (Y)",
     main = "First Pair of Canonical Variates", pch = 19, col = "steelblue")

plot(U[, 2], V[, 2],
     xlab = "2nd Canonical Variate (X)", ylab = "2nd Canonical Variate (Y)",
     main = "Second Pair of Canonical Variates", pch = 19, col = "darkorange")

## g) Significance test of canonical correlations
rho   <- cc_res$cor
n_obs <- nrow(X); p <- ncol(X); q <- ncol(Y)
p.asym(rho, n_obs, p, q, tstat = "Wilks")
# You may also try tstat = "Hotelling", "Pillai", or "Roy"
# Comment: Wilks' Lambda tests sequentially whether the k-th canonical
# correlation and all remaining ones are jointly zero. Dimensions with
# p < 0.05 represent statistically real association between the Lifestyle
# and Academic Performance sets; once p > 0.05 the remaining canonical
# variates add no further meaningful shared information.


# =====================================================================
# QUESTION 2: PCA on data frame X (created in Question 1)
# =====================================================================

## a) Variance-covariance matrix, eigenvectors & eigenvalues
S <- cov(X)
S
eig <- eigen(S)
eig$values       # eigenvalues
eig$vectors      # eigenvectors

## b) PCA using prcomp()
pca_X <- prcomp(X, center = TRUE, scale. = TRUE)
summary(pca_X)
pca_X$rotation          # loadings (eigenvectors of correlation matrix)
pca_X$sdev^2            # eigenvalues of correlation matrix

## c) Plot first two principal components
plot(pca_X$x[, 1], pca_X$x[, 2],
     xlab = "PC1", ylab = "PC2",
     main = "First Two Principal Components (Lifestyle Variables)",
     pch = 19, col = "forestgreen")
abline(h = 0, v = 0, lty = 2, col = "grey")

## d) Interpretation of 2(a), 2(b), 2(c)
# The covariance-matrix eigenvalues in (a) are on the RAW scale, so they
# are dominated by whichever variable has the largest variance (units
# differ: hours vs event counts). Running prcomp() with scale.=TRUE in
# (b) standardizes all three lifestyle variables first, so eigenvalues
# there are comparable and represent the correlation-matrix structure.
# summary(pca_X) reports the proportion of variance each PC explains;
# typically the first two PCs already capture most of the variability
# among sleep, social activity and exercise, meaning a student's
# lifestyle pattern can reasonably be summarised in 1-2 composite scores
# instead of 3 separate variables. The PC1-vs-PC2 scatter in (c) shows
# whether students form distinct lifestyle clusters or vary continuously;
# a diffuse, unclustered cloud suggests no sharply distinct lifestyle
# "types" in this sample.


# =====================================================================
# QUESTION 3: Exploratory Factor Analysis on performance.csv
# =====================================================================

## a) Factorability: KMO & Bartlett's Test
KMO(data1)
cortest.bartlett(cor(data1), n = nrow(data1))
# Comment: An overall KMO statistic > 0.6 ("mediocre" or better) means the
# data are broadly suitable for factor analysis; individual MSA values
# above 0.5 indicate every variable shares enough common variance with the
# rest. Bartlett's test with p < 0.001 confirms the correlation matrix is
# significantly different from an identity matrix, i.e., the variables are
# correlated enough that factor analysis is meaningful (not just noise).

## b) Determine number of factors
fa.parallel(data1, fa = "fa")     # parallel analysis / scree plot
eigen(cor(data1))$values          # Kaiser's rule: eigenvalue > 1
# Comment: Count how many eigenvalues exceed 1 (Kaiser's rule) and/or
# where the scree plot bends ("elbow"); both together suggest the
# appropriate number of factors, Nfacs (commonly 2 for a 7-variable set
# split into a lifestyle block and an academic block).

## c) Exploratory Factor Analysis using factanal()
Nfacs <- 2      # <-- set this based on part (b)
fa_res <- factanal(data1, factors = Nfacs, rotation = "varimax")
print(fa_res, digits = 2, cutoff = 0.3)
# Comment: Variables loading highly on Factor 1 vs Factor 2 reveal the
# latent structure -- e.g., one factor may represent "academic engagement"
# (study.hrs, project.scr, library.vst, gpa.scr) while another represents
# "lifestyle habits" (sleep.hrs, social.evt, exercise.hrs). The cumulative
# variance line in the output shows what fraction of total variability the
# retained factors jointly explain.


# =====================================================================
# QUESTION 4: Independent Component Analysis (fastICA)
# =====================================================================

set.seed(4155)     # <-- REPLACE 4101 with the LAST 4 DIGITS of YOUR student ID

## a) Generate two independent source tones with additive Gaussian noise
t  <- 1:1000
s1 <- 0.7*sin((t)/19 + 0.57*pi) + mvrnorm(n = 1000, mu = 0, Sigma = 0.004)
s2 <- sin((t)/33) + mvrnorm(n = 1000, mu = 0.03, Sigma = 0.005)

matplot(t, cbind(s1, s2), type = "l", lty = 1, col = c("blue", "red"),
        xlab = "Time", ylab = "Amplitude",
        main = "Original Source Tones s1 and s2")
legend("topright", legend = c("s1", "s2"), col = c("blue", "red"), lty = 1)

## b) Source matrix S and deterministic Mixing matrix A
S_mat <- cbind(s1, s2)
S_mat
A <- matrix(c(1, -2, 1.73, 3.41), nrow = 2, ncol = 2, byrow = FALSE)
A
#      [,1]  [,2]
# [1,]    1  1.73
# [2,]   -2  3.41

## c) Mix the two tones: X = S %*% A
X_mixed <- S_mat %*% A
colnames(X_mixed) <- c("x1", "x2")
X_mixed
## d) Scatter plots (joint distributions)
par(mfrow = c(1, 2))
plot(S_mat[, 1], S_mat[, 2], xlab = "s1", ylab = "s2",
     main = "Source Data (S)", pch = 20, col = "darkgreen")
plot(X_mixed[, 1], X_mixed[, 2], xlab = "x1", ylab = "x2",
     main = "Linearly Mixed Data (X)", pch = 20, col = "purple")
par(mfrow = c(1, 1))
# Comment: The source scatter (S) looks like a roughly rectangular,
# uncorrelated cloud (independent components), while the mixed data (X)
# forms a sheared/rotated parallelogram-shaped cloud -- a visual signature
# of the linear mixing induced by A.

## e) Estimate source tones using fastICA
install.packages("fastICA")
library(fastICA)
ica_res <- fastICA(X_mixed, n.comp = 2)

## f) Plot observed vs estimated tones
par(mfrow = c(2, 1))
matplot(t, X_mixed, type = "l", lty = 1, col = c("blue", "red"),
        main = "Observed (Mixed) Signals", xlab = "Time", ylab = "Amplitude")
matplot(t, ica_res$S, type = "l", lty = 1, col = c("blue", "red"),
        main = "ICA-Estimated Source Signals", xlab = "Time", ylab = "Amplitude")
par(mfrow = c(1, 1))
# Comment: fastICA recovers waveforms that closely resemble the original
# s1 and s2 tones, up to arbitrary scaling, sign flip, and possible
# re-ordering of the components (the classical identifiability
# limitations of ICA). This confirms that ICA can successfully "unmix"
# statistically independent, non-Gaussian signals from only their linear
# combinations, without knowing the mixing matrix A in advance.
