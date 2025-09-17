# ===========================
# Guns Data: Gun laws and murder rate regressions
# Clean R script version
# ===========================

# ---- 0) Required packages ----
need <- c("AER","plm","sandwich","lmtest")
to_install <- setdiff(need, rownames(installed.packages()))
if(length(to_install)) install.packages(to_install, repos = "https://cloud.r-project.org")

library(AER)       
library(plm)       
library(sandwich)  
library(lmtest)    

# ---- 1) Load data and create log variables ----
data("Guns", package = "AER")
Guns$log_income    <- log(Guns$income)
Guns$log_density   <- log(Guns$density)
Guns$log_prisoners <- log(Guns$prisoners)

pg <- pdata.frame(Guns, index = c("state","year"))

# ---- 2) Model 1: OLS (log murder ~ law) ----
m1 <- lm(log(murder) ~ law, data = Guns)
cat("\n=== Model 1: OLS log(murder) ~ law ===\n")
print(coeftest(m1, vcov = vcovHC(m1, type = "HC1")))

# ---- 3) Model 2: OLS with controls ----
m2 <- lm(log(murder) ~ law + male + afam + cauc + log_income + log_density + log_prisoners,
         data = Guns)
cat("\n=== Model 2: OLS + controls ===\n")
print(coeftest(m2, vcov = vcovHC(m2, type = "HC1")))

law_pct_m2 <- 100*(exp(coef(m2)["lawyes"]) - 1)
cat(sprintf("Approx. %% change in murder when law==yes (Model 2): %.2f%%\n", law_pct_m2))

# ---- 4) Model 3: State fixed effects ----
m3 <- plm(log(murder) ~ law + male + afam + cauc + log_income + log_density + log_prisoners,
          data = pg, model = "within", effect = "individual")
cat("\n=== Model 3: State FE ===\n")
print(coeftest(m3, vcov = vcovHC(m3, type = "HC1")))

# ---- 5) Model 4: Two-way fixed effects ----
m4 <- plm(log(murder) ~ law + male + afam + cauc + log_income + log_density + log_prisoners,
          data = pg, model = "within", effect = "twoways")
cat("\n=== Model 4: Two-ways FE (state & year) ===\n")
print(coeftest(m4, vcov = vcovHC(m4, type = "HC1")))

# ---- 6) Model 5: Two-way FE + clustered SE (by state) ----
cat("\n=== Model 5: Two-ways FE + cluster-robust SE (by state) ===\n")
vcov_cl_state <- vcovHC(m4, type = "HC1", cluster = "group")
print(coeftest(m4, vcov = vcov_cl_state))

# ---- 7) Summary of law coefficient ----
summarize_law <- function(fit, vc){
  bt <- coeftest(fit, vcov = vc)
  if(!"lawyes" %in% rownames(bt)) return(invisible(NULL))
  b  <- bt["lawyes","Estimate"]
  se <- bt["lawyes","Std. Error"]
  t  <- bt["lawyes","t value"]
  pct <- 100*(exp(b)-1)
  data.frame(beta=b, se=se, t=t, pct_change= pct, row.names = deparse(substitute(fit)))
}
out <- rbind(
  summarize_law(m1, vcovHC(m1, type="HC1")),
  summarize_law(m2, vcovHC(m2, type="HC1")),
  summarize_law(m3, vcovHC(m3, type="HC1")),
  summarize_law(m4, vcovHC(m4, type="HC1")),
  summarize_law(m4, vcov_cl_state)
)
cat("\n=== Law coefficient summary (robust SE) ===\n"); print(out)
