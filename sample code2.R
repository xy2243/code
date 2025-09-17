## --------------------------
## Problem 2: Residuals and Prediction
## --------------------------

# Load packages and data
options(repos = c(CRAN = "https://cloud.r-project.org/"))
install.packages("data.table")

load("medical_school_data_2024.RData")  # adjust path if needed
mddata <- data.table::data.table(dat)

# Fit regression
fit <- lm(salary_t1 ~ sex + experience, data = mddata)
summary(fit)

# Diagnostic plots
plot(fit)

# Residual checks
mddata[, res := fit$residuals]
mddata[, resxexp := res * experience]
mean_resxexp <- mean(mddata$resxexp, na.rm = TRUE)
print(mean_resxexp)

# Mean residual by sex
print(mddata[, .(mean_res = mean(res, na.rm = TRUE)), by = sex])

# Orthogonality checks
mddata[, yb := fit$fitted.values]
dot_product_yb_res <- sum(mddata$yb * mddata$res, na.rm = TRUE)
print(dot_product_yb_res)

lhs <- sum(mddata$yb * mddata$salary_t1, na.rm = TRUE)
rhs <- sum(mddata$yb * mddata$yb, na.rm = TRUE)
print(c(lhs, rhs))


## --------------------------
## Problem 3: Frisch–Waugh Theorem
## --------------------------

# Create dummy for Male = 1, Female = 0
dat$maledummy <- ifelse(dat$sex == "Male", 1, 0)
table(dat$maledummy)

# Long regression
long_model <- lm(salary_t1 ~ maledummy + experience + publications, data = dat)
summary(long_model)

# Extract β1 from long regression
beta1_LR <- coef(long_model)["maledummy"]

# Residual regressions
resid_y <- residuals(lm(salary_t1 ~ experience + publications, data = dat))
resid_maledummy <- residuals(lm(maledummy ~ experience + publications, data = dat))

resid_model <- lm(resid_y ~ resid_maledummy)
summary(resid_model)

# Extract β1 from residual regression
beta1_Res <- coef(resid_model)["resid_maledummy"]

# Compare results
cat("β1 from Long Regression:", beta1_LR, "\n")
cat("β1 from Residual Regression:", beta1_Res, "\n")
all.equal(beta1_LR, beta1_Res)

