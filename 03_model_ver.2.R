# ============================================================
# 03_model.R
# Mixed-effects logistic regression
# Run AFTER 02_analysis.py has produced updated thesis_long_format.csv
# ============================================================

library(lme4)
library(multcomp)

# --- Load data ---
# UPDATE THIS PATH to match your local setup
setwd("C:/Users/owner/Downloads/Big Data/week 10")
df <- read.csv("thesis_long_format.csv")

# --- Factor coding ---
df$discount_f <- factor(df$discount, levels = c(0, 10, 25, 50))
df$reputation_high <- factor(df$reputation_high, levels = c(0, 1))
df$price_high <- factor(df$price_high, levels = c(0, 1))

# Language control (1 = English/version B, 0 = Chinese)
df$language <- ifelse(df$version == "B", 1, 0)

# Center continuous predictors (coerce to numeric explicitly)
df$trust_c <- as.numeric(scale(df$trust_index, center = TRUE, scale = FALSE))
df$risk_c  <- as.numeric(scale(df$risk_aversion, center = TRUE, scale = FALSE))

# Quick data check
cat("Respondents:", length(unique(df$survey_id)), "\n")
cat("Observations:", nrow(df), "\n")
cat("Versions:", paste(names(table(df$version)), table(df$version), collapse=", "), "\n")


# ============================================================
# MAIN MODEL
# ============================================================

cat("\n============================================================\n")
cat("MAIN MODEL RESULTS\n")
cat("============================================================\n")

model <- glmer(
  online_choice ~
    discount_f +                        # H1a, H1b, H1d
    reputation_high +                   # H2a
    price_high +                        # price main effect
    language +                          # control for EN/CN confound
    discount_f : price_high +           # H1c
    discount_f : reputation_high +      # H2b
    reputation_high : price_high +      # H2c
    trust_c +                           # H3a
    risk_c +                            # H4a
    trust_c : reputation_high +         # H3b
    risk_c : reputation_high +          # H4b
    (1 | survey_id),                    # random intercept
  data = df,
  family = binomial(link = "logit"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)

summary(model)

# Odds ratios
cat("\nOdds ratios:\n")
print(round(exp(fixef(model)), 3))

# Confidence intervals
cat("\nConfidence intervals:\n")
print(confint(model, method = "Wald"))


# ============================================================
# H1a: ORDERED TREND TEST
# Tests whether there is a linear trend across discount levels
# ============================================================

cat("\n============================================================\n")
cat("H1a: ORDERED TREND TEST\n")
cat("============================================================\n")

df$discount_poly <- df$discount_f
contrasts(df$discount_poly) <- contr.poly(4)

model_trend <- glmer(
  online_choice ~ discount_poly + reputation_high + price_high +
    language + trust_c + risk_c + (1 | survey_id),
  data = df, family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
cat("\nPolynomial contrasts (.L = linear, .Q = quadratic, .C = cubic):\n")
summary(model_trend)


# ============================================================
# H1b: ANY DISCOUNT VS NO DISCOUNT (planned contrast)
# Tested within the main model, not a separate reduced model
# ============================================================

cat("\n============================================================\n")
cat("H1b: ANY DISCOUNT VS NO DISCOUNT (contrast in main model)\n")
cat("============================================================\n")

K <- matrix(0, nrow = 1, ncol = length(fixef(model)))
colnames(K) <- names(fixef(model))
K[1, "discount_f10"] <- 1/3
K[1, "discount_f25"] <- 1/3
K[1, "discount_f50"] <- 1/3

h1b_test <- glht(model, linfct = K)
cat("\nPlanned contrast result:\n")
summary(h1b_test)
cat("\nOdds ratio for any discount vs none:\n")
cat(round(exp(coef(h1b_test)), 3), "\n")


# ============================================================
# H1d: NON-LINEARITY TEST (fair comparison)
# Both models have same structure, only discount coding differs
# ============================================================

cat("\n============================================================\n")
cat("H1d: NON-LINEARITY TEST\n")
cat("============================================================\n")

df$discount_num <- df$discount

# Dummy-coded discount (flexible, allows non-linearity)
model_dummy <- glmer(
  online_choice ~ discount_f + reputation_high + price_high +
    language + trust_c + risk_c + (1 | survey_id),
  data = df, family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)

# Linear discount (assumes straight line)
model_linear <- glmer(
  online_choice ~ discount_num + reputation_high + price_high +
    language + trust_c + risk_c + (1 | survey_id),
  data = df, family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)

cat("Dummy AIC:", AIC(model_dummy), "\n")
cat("Linear AIC:", AIC(model_linear), "\n")
cat("BIC comparison - Dummy:", BIC(model_dummy), "Linear:", BIC(model_linear), "\n")

if (AIC(model_dummy) < AIC(model_linear)) {
  cat(">> Dummy model fits better. Non-linearity is present.\n")
} else {
  cat(">> Linear model fits better. No evidence of non-linearity.\n")
}

# Likelihood ratio test
cat("\nLikelihood ratio test (dummy vs linear):\n")
print(anova(model_linear, model_dummy))



# ============================================================
# POOLED LOGISTIC REGRESSION (for comparison only — NOT main model)
# ============================================================

# Full interactive model, pooled (no random intercept)
pooled_full <- glm(
  online_choice ~ discount_f + reputation_high + price_high + language +
    discount_f:price_high + discount_f:reputation_high + reputation_high:price_high +
    trust_c + risk_c + trust_c:reputation_high + risk_c:reputation_high,
  data = df,
  family = binomial(link = "logit")
)

cat("\n============================================================\n")
cat("POOLED MODEL — FULL INTERACTIVE\n")
cat("============================================================\n")
print(summary(pooled_full))

# Odds ratios
cat("\nOdds ratios (pooled full):\n")
print(round(exp(coef(pooled_full)), 3))

# Confidence intervals
cat("\nConfidence intervals (pooled full):\n")
print(round(confint(pooled_full), 3))

# Trend model, pooled
pooled_trend <- glm(
  online_choice ~ discount_poly + reputation_high + price_high +
    language + trust_c + risk_c,
  data = df,
  family = binomial(link = "logit")
)

cat("\n============================================================\n")
cat("POOLED MODEL — TREND (POLYNOMIAL DISCOUNT)\n")
cat("============================================================\n")
print(summary(pooled_trend))

cat("\nOdds ratios (pooled trend):\n")
print(round(exp(coef(pooled_trend)), 3))

# ============================================================
# SUPPLEMENTARY: Quadratic discount × price (advisor suggestion)
# Tests whether non-linearity differs by base price level
# ============================================================

df$discount_num <- df$discount

# Full model: quadratic discount × price interaction
cat("\n============================================================\n")
cat("SUPPLEMENTARY: QUADRATIC DISCOUNT × PRICE\n")
model_quad <- glmer(
  online_choice ~ poly(discount_num, 2) * price_high +
    reputation_high + language + trust_c + risk_c +
    (1 | survey_id),
  data = df,
  family = binomial(link = "logit"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)

cat("\n============================================================\n")
cat("SUPPLEMENTARY: QUADRATIC DISCOUNT × PRICE\n")
cat("============================================================\n")
print(summary(model_quad))

cat("\nOdds ratios:\n")
print(round(exp(fixef(model_quad)), 3))

sink("model_results.txt")

cat("============================================================\n")
cat("MAIN MODEL\n")
cat("============================================================\n")
summary(model)
cat("\nOdds ratios:\n")
print(round(exp(fixef(model)), 3))
cat("\nConfidence intervals:\n")
print(confint(model, method = "Wald"))

cat("\n============================================================\n")
cat("H1a: ORDERED TREND TEST\n")
cat("============================================================\n")
summary(model_trend)

cat("\n============================================================\n")
cat("H1b: PLANNED CONTRAST\n")
cat("============================================================\n")
summary(h1b_test)
cat("Odds ratio:", round(exp(coef(h1b_test)), 3), "\n")

cat("\n============================================================\n")
cat("H1d: NON-LINEARITY\n")
cat("============================================================\n")
cat("Dummy AIC:", AIC(model_dummy), "\n")
cat("Linear AIC:", AIC(model_linear), "\n")
print(anova(model_linear, model_dummy))

cat("\n============================================================\n")
cat("POOLED MODEL — FULL INTERACTIVE (comparison only, NOT main result)\n")
cat("============================================================\n")
print(summary(pooled_full))
cat("\nOdds ratios (pooled full):\n")
print(round(exp(coef(pooled_full)), 3))
cat("\nConfidence intervals (pooled full):\n")
print(round(confint(pooled_full), 3))

cat("\n============================================================\n")
cat("POOLED MODEL — TREND (comparison only, NOT main result)\n")
cat("============================================================\n")
print(summary(pooled_trend))
cat("\nOdds ratios (pooled trend):\n")
print(round(exp(coef(pooled_trend)), 3))

cat("\n============================================================\n")
cat("SUPPLEMENTARY: QUADRATIC DISCOUNT × PRICE\n")
cat("============================================================\n")
print(summary(model_quad))
cat("\nOdds ratios:\n")
print(round(exp(fixef(model_quad)), 3))

sink()
cat("\n✓ All results saved to model_results.txt\n")


# ===== Power analysis (simr) — run after `model` and `df` exist =====
library(simr)
simrOptions("progress" = FALSE)
NSIM <- 500          # set to 200 for a faster run
set.seed(42)

# H1d needs two matched models that differ ONLY in discount coding
df$discount_num <- df$discount
model_dummy_simple <- glmer(
  online_choice ~ discount_f + reputation_high + price_high +
    language + trust_c + risk_c + (1 | survey_id),
  data = df, family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
model_linear <- glmer(
  online_choice ~ discount_num + reputation_high + price_high +
    language + trust_c + risk_c + (1 | survey_id),
  data = df, family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

# H1b "any discount" binary model => term is has_discount1
df$has_discount <- factor(ifelse(df$discount > 0, 1, 0))
model_hasdisc <- glmer(
  online_choice ~ has_discount + reputation_high + price_high +
    language + trust_c + risk_c + (1 | survey_id),
  data = df, family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

sink("power_results.txt", split = TRUE)

cat("\n=== H1a: discount per-level ===\n")
print(powerSim(model, test = fixed("discount_f10"), nsim = NSIM, seed = 42))
print(powerSim(model, test = fixed("discount_f25"), nsim = NSIM, seed = 42))
print(powerSim(model, test = fixed("discount_f50"), nsim = NSIM, seed = 42))

cat("\n=== H1b: any discount vs none ===\n")
print(powerSim(model_hasdisc, test = fixed("has_discount1"), nsim = NSIM, seed = 42))

cat("\n=== H1c: discount x price ===\n")
print(powerSim(model, test = fixed("discount_f10:price_high1"), nsim = NSIM, seed = 42))
print(powerSim(model, test = fixed("discount_f25:price_high1"), nsim = NSIM, seed = 42))
print(powerSim(model, test = fixed("discount_f50:price_high1"), nsim = NSIM, seed = 42))

cat("\n=== H1d: non-linearity (dummy vs linear) ===\n")
print(powerSim(model_dummy_simple, test = compare(model_linear), nsim = NSIM, seed = 42))

cat("\n=== H2a: reputation main effect ===\n")
print(powerSim(model, test = fixed("reputation_high1"), nsim = NSIM, seed = 42))

cat("\n=== H2b: reputation x discount ===\n")
print(powerSim(model, test = fixed("discount_f10:reputation_high1"), nsim = NSIM, seed = 42))
print(powerSim(model, test = fixed("discount_f25:reputation_high1"), nsim = NSIM, seed = 42))
print(powerSim(model, test = fixed("discount_f50:reputation_high1"), nsim = NSIM, seed = 42))

cat("\n=== H2c: reputation x price ===\n")
print(powerSim(model, test = fixed("reputation_high1:price_high1"), nsim = NSIM, seed = 42))

cat("\n=== H3a / H3b: trust ===\n")
print(powerSim(model, test = fixed("trust_c"), nsim = NSIM, seed = 42))
print(powerSim(model, test = fixed("reputation_high1:trust_c"), nsim = NSIM, seed = 42))

cat("\n=== H4a / H4b: risk aversion ===\n")
print(powerSim(model, test = fixed("risk_c"), nsim = NSIM, seed = 42))
print(powerSim(model, test = fixed("reputation_high1:risk_c"), nsim = NSIM, seed = 42))

cat("\n=== Power analysis complete ===\n")
sink()


# ============================================================
# CORRECTED MAIN-EFFECT POWER (main effects from no-interaction model)
# Run after df is loaded and factors/centering are done (top of 03_model.R)
# ============================================================
library(lme4)
library(simr)
simrOptions("progress" = FALSE)
NSIM <- 500
set.seed(42)

# No-interaction model: coefficients here ARE the main effects
model_main <- glmer(
  online_choice ~ discount_f + reputation_high + price_high +
    language + trust_c + risk_c + (1 | survey_id),
  data = df, family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)

# sanity check: should print ~2.09, NOT ~1.47
cat("reputation main-effect B =", round(fixef(model_main)["reputation_high1"], 3), "\n")

sink("power_results_maineffects.txt", split = TRUE)

cat("\n=== H1a: discount per-level (MAIN-EFFECTS model) ===\n")
print(powerSim(model_main, test = fixed("discount_f10"), nsim = NSIM, seed = 42))
print(powerSim(model_main, test = fixed("discount_f25"), nsim = NSIM, seed = 42))
print(powerSim(model_main, test = fixed("discount_f50"), nsim = NSIM, seed = 42))

cat("\n=== H2a: reputation main effect (MAIN-EFFECTS model) ===\n")
print(powerSim(model_main, test = fixed("reputation_high1"), nsim = NSIM, seed = 42))

cat("\n=== H3a: trust main effect (MAIN-EFFECTS model) ===\n")
print(powerSim(model_main, test = fixed("trust_c"), nsim = NSIM, seed = 42))

cat("\n=== H4a: risk main effect (MAIN-EFFECTS model) ===\n")
print(powerSim(model_main, test = fixed("risk_c"), nsim = NSIM, seed = 42))

cat("\n=== done ===\n")
sink()


  # SUPPLEMENTARY: Student vs Working
  # Run after the top of 03_model_ver_2.R (df loaded and coded)
  # ============================================================
library(lme4)

# guards (safe if already defined)
if(!"discount_f" %in% names(df)) df$discount_f <- factor(df$discount, levels=c(0,10,25,50))
if(!is.factor(df$reputation_high)) df$reputation_high <- factor(df$reputation_high, levels=c(0,1))
if(!is.factor(df$price_high)) df$price_high <- factor(df$price_high, levels=c(0,1))
if(!"language" %in% names(df)) df$language <- ifelse(df$version=="B",1,0)
if(!"trust_c" %in% names(df)) df$trust_c <- as.numeric(scale(df$trust_index, center=TRUE, scale=FALSE))
if(!"risk_c"  %in% names(df)) df$risk_c  <- as.numeric(scale(df$risk_aversion, center=TRUE, scale=FALSE))

# status_cat: 1=Student, 2=Working, 3=Other
df$status_f <- factor(df$status_cat, levels=c(1,2,3), labels=c("Student","Working","Other"))

sink("status_results.txt", split=TRUE)

# ---------- 1. BIVARIATE (person-level, Student vs Working) ----------
person <- aggregate(online_choice ~ survey_id, data=df, FUN=mean)
names(person)[2] <- "online_rate"
first <- df[!duplicated(df$survey_id),
            c("survey_id","status_f","trust_index","risk_aversion","income_cat","budget_cat")]
person <- merge(person, first, by="survey_id")
pw <- droplevels(subset(person, status_f %in% c("Student","Working")))

# sanity checks: must print 121 and 116
cat("persons total:", nrow(person), " student+working:", nrow(pw), "\n")
cat("\n=== Group sizes (persons) ===\n"); print(table(pw$status_f))

desc <- function(x,g) cat("  Student: mean",round(mean(x[g=="Student"]),3),"SD",round(sd(x[g=="Student"]),3),
                          "| Working: mean",round(mean(x[g=="Working"]),3),"SD",round(sd(x[g=="Working"]),3),"\n")

cat("\n=== Trust index ===\n"); desc(pw$trust_index, pw$status_f)
print(t.test(trust_index ~ status_f, data=pw))
print(wilcox.test(trust_index ~ status_f, data=pw, exact=FALSE))

cat("\n=== Risk aversion ===\n"); desc(pw$risk_aversion, pw$status_f)
print(t.test(risk_aversion ~ status_f, data=pw))
print(wilcox.test(risk_aversion ~ status_f, data=pw, exact=FALSE))

cat("\n=== Online-choice rate (per person) ===\n"); desc(pw$online_rate, pw$status_f)
print(t.test(online_rate ~ status_f, data=pw))
print(wilcox.test(online_rate ~ status_f, data=pw, exact=FALSE))

cat("\n=== Income distribution ===\n"); print(table(pw$status_f, pw$income_cat))
print(chisq.test(table(pw$status_f, pw$income_cat), simulate.p.value=TRUE, B=10000))
cat("\n=== Budget distribution ===\n"); print(table(pw$status_f, pw$budget_cat))
print(chisq.test(table(pw$status_f, pw$budget_cat), simulate.p.value=TRUE, B=10000))

# ---------- 2. REGRESSION (status as covariate in main-effects model) ----------
dfw <- droplevels(subset(df, status_cat %in% c(1,2)))
dfw$working <- factor(ifelse(dfw$status_cat==2,1,0), levels=c(0,1))  # 0=Student (ref), 1=Working

# check both languages appear in both groups before trusting the language covariate
cat("\n=== working x language cross-tab (both cells should be non-empty) ===\n")
print(table(dfw$working, dfw$language))

model_status <- glmer(
  online_choice ~ discount_f + reputation_high + price_high +
    language + trust_c + risk_c + working + (1 | survey_id),
  data=dfw, family=binomial,
  control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))

cat("\n=== Main-effects model + student/working covariate ===\n")
print(summary(model_status))

# odds ratio + 95% CI for the status effect (vcov-based, no confint quirks)
est <- fixef(model_status)["working1"]
se  <- sqrt(diag(vcov(model_status)))["working1"]
cat("\nWorking vs Student  OR:", round(exp(est),3),
    " 95% CI:", round(exp(est-1.96*se),3), "-", round(exp(est+1.96*se),3),
    " p:", round(summary(model_status)$coefficients["working1","Pr(>|z|)"],3), "\n")

# does adding status improve fit? (nested LR test, same subset)
model_nostatus <- glmer(
  online_choice ~ discount_f + reputation_high + price_high +
    language + trust_c + risk_c + (1 | survey_id),
  data=dfw, family=binomial,
  control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
cat("\n=== LR test: does student/working add anything? ===\n")
print(anova(model_nostatus, model_status))

sink()
cat("\nSaved to status_results.txt\n")