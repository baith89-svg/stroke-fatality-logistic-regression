# ==============================================================================
# Stroke Fatality Logistic Regression Analysis (RREAAA Workflow)
# Dataset: stroke-fatality-cooked2.dta
# ==============================================================================

# Load required packages
library(tidyverse)
library(haven)
library(janitor)
library(gtsummary)
library(broom)
library(car)
library(pROC)
library(ResourceSelection)

# Create output directory for figures
dir.create("figures", showWarnings = FALSE)

# ==============================================================================
# 1. REASON & 2. READ
# ==============================================================================
# Objective: Identify independent predictors of patient status at discharge
# (outcome: 0 = alive, 1 = dead) using binary logistic regression.

raw_df <- read_dta("stroke-fatality-cooked2.dta")

# Fix variable names and convert Stata labelled variables to factors
# with clinically meaningful reference levels
stroke_df <- raw_df %>%
  clean_names() %>%
  rename(
    marital_status = maritalstat,
    referral_case  = referralcase,
    stroke_type    = stroketype
  ) %>%
  mutate(
    outcome = factor(as.numeric(outcome), levels = c(0, 1), labels = c("alive", "dead")),
    sex = factor(as.numeric(sex), levels = c(1, 2), labels = c("male", "female")),
    hypertension = factor(as.numeric(hypertension), levels = c(0, 1), labels = c("no", "yes")),
    marital_status = factor(as.numeric(marital_status), levels = c(0, 1), labels = c("no", "yes")),
    malay = factor(as.numeric(malay), levels = c(0, 1), labels = c("no", "yes")),
    diabetes = factor(as.numeric(diabetes), levels = c(0, 1), labels = c("no", "yes")),
    dyslipidaemia = factor(as.numeric(dyslipidaemia), levels = c(0, 1), labels = c("no", "yes")),
    referral_case = factor(as.numeric(referral_case), levels = c(0, 1), labels = c("hospital", "GP/Home/Missing")),
    stroke_type = factor(as.numeric(stroke_type), levels = c(0, 1), labels = c("CI (Ischaemic)", "Haemorrhagic"))
  )

cat("=== 2. READ: Dataset Dimensions & Structure ===\n")
cat("Rows:", nrow(stroke_df), " Columns:", ncol(stroke_df), "\n")
cat("Unique registration numbers (rn):", n_distinct(stroke_df$rn), "\n\n")

# ==============================================================================
# 3. EXPLORE
# ==============================================================================
cat("=== 3. EXPLORE: Missing Data Check ===\n")
missing_tbl <- tibble(
  variable = names(stroke_df),
  n_missing = colSums(is.na(stroke_df)),
  pct_missing = round(100 * colSums(is.na(stroke_df)) / nrow(stroke_df), 2)
)
print(missing_tbl, n = 25)

# Check potential data entry errors (e.g., SBP <= DBP, out-of-range GCS, age <= 0)
cat("\n=== Data Entry / Logical Consistency Checks ===\n")
cat("Cases with SBP <= DBP:", sum(stroke_df$sbp <= stroke_df$dbp), "\n")
cat("Cases with GCS < 3 or GCS > 15:", sum(stroke_df$gcs < 3 | stroke_df$gcs > 15), "\n")
cat("Cases with Age < 0 or Age > 120:", sum(stroke_df$age < 0 | stroke_df$age > 120), "\n")

# IQR-based outlier summary for numerical variables
num_vars <- c("gcs", "sbp", "dbp", "hr", "hb", "plat", "wbc", "na", "potas", "cbs", "urea", "age")
cat("\n=== IQR Outlier Summary (Numerical Variables) ===\n")
outlier_summary <- stroke_df %>%
  select(all_of(num_vars)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  group_by(variable) %>%
  summarise(
    min = min(value),
    q1 = quantile(value, 0.25),
    median = median(value),
    q3 = quantile(value, 0.75),
    max = max(value),
    iqr = IQR(value),
    n_outliers = sum(value < (q1 - 1.5 * iqr) | value > (q3 + 1.5 * iqr)),
    .groups = "drop"
  )
print(outlier_summary, n = 15)

# Histograms for numerical variables
p_hist <- stroke_df %>%
  select(all_of(num_vars)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 20, fill = "#2c7fb8", color = "white", alpha = 0.85) +
  facet_wrap(~ variable, scales = "free", ncol = 4) +
  theme_minimal(base_size = 11) +
  labs(title = "Distribution of Numerical Variables (Histograms)", x = NULL, y = "Count")
ggsave("figures/explore_histograms.png", p_hist, width = 11, height = 7, dpi = 300)

# Boxplots for numerical variables by outcome
p_box <- stroke_df %>%
  select(outcome, all_of(num_vars)) %>%
  pivot_longer(-outcome, names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = outcome, y = value, fill = outcome)) +
  geom_boxplot(alpha = 0.8, outlier.color = "#d95f02") +
  facet_wrap(~ variable, scales = "free_y", ncol = 4) +
  scale_fill_manual(values = c("alive" = "#1b9e77", "dead" = "#d95f02")) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "none") +
  labs(title = "Boxplots of Numerical Variables by Discharge Outcome", x = "Outcome", y = NULL)
ggsave("figures/explore_boxplots.png", p_box, width = 11, height = 7, dpi = 300)

# Bar charts for categorical variables
cat_vars <- c("sex", "hypertension", "marital_status", "malay", "diabetes",
              "dyslipidaemia", "referral_case", "stroke_type")
p_bar <- stroke_df %>%
  select(outcome, all_of(cat_vars)) %>%
  pivot_longer(-outcome, names_to = "variable", values_to = "level") %>%
  ggplot(aes(x = level, fill = outcome)) +
  geom_bar(position = "dodge", alpha = 0.9) +
  facet_wrap(~ variable, scales = "free_x", ncol = 4) +
  scale_fill_manual(values = c("alive" = "#1b9e77", "dead" = "#d95f02")) +
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1)) +
  labs(title = "Categorical Variables by Discharge Outcome", x = NULL, y = "Count", fill = "Outcome")
ggsave("figures/explore_barcharts.png", p_bar, width = 12, height = 6.5, dpi = 300)

# Descriptive summary table using gtsummary
tbl_desc <- stroke_df %>%
  select(-rn) %>%
  tbl_summary(
    by = outcome,
    statistic = list(
      all_continuous() ~ "{median} ({p25}, {p75})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 1
  ) %>%
  add_overall() %>%
  add_p()

cat("\n=== Descriptive Table (gtsummary) ===\n")
print(as_tibble(tbl_desc))

# ==============================================================================
# 5. APPLY: Univariable & Multivariable Logistic Regression (No Interaction)
# ==============================================================================
predictors <- setdiff(names(stroke_df), c("rn", "outcome"))

# Univariable logistic regression via broom
univ_results <- map_dfr(predictors, function(var) {
  fml <- as.formula(paste("outcome ~", var))
  mod <- glm(fml, data = stroke_df, family = binomial(link = "logit"))
  tidy(mod, exponentiate = TRUE, conf.int = TRUE) %>%
    filter(term != "(Intercept)") %>%
    mutate(variable = var, .before = 1)
})

cat("\n=== Univariable Logistic Regression (Crude OR) ===\n")
print(univ_results %>% select(variable, term, estimate, conf.low, conf.high, p.value), n = 25)

# Screen candidates with p < 0.25 in univariable analysis
candidate_vars <- univ_results %>%
  filter(p.value < 0.25) %>%
  pull(variable) %>%
  unique()

cat("\nCandidate variables (univariable p < 0.25):", paste(candidate_vars, collapse = ", "), "\n")

# Fit full candidate multivariable model (no interactions)
fml_full <- as.formula(paste("outcome ~", paste(candidate_vars, collapse = " + ")))
mod_full <- glm(fml_full, data = stroke_df, family = binomial(link = "logit"))

# Backward stepwise selection by AIC to obtain parsimonious final model
mod_final <- step(mod_full, direction = "backward", trace = 0)

cat("\n=== Final Multivariable Logistic Regression Model (Adjusted OR) ===\n")
final_tidy <- tidy(mod_final, exponentiate = TRUE, conf.int = TRUE)
print(final_tidy)
cat("\nModel Formula:", deparse(formula(mod_final)), "\n")

# ==============================================================================
# 4. ASSUMPTION CHECKS (on Final Multivariable Model)
# ==============================================================================
cat("\n=== 4. ASSUMPTIONS: Multicollinearity (VIF) ===\n")
print(car::vif(mod_final))

# Linearity in the logit (Box-Tidwell test for continuous predictors in final model)
final_num_vars <- intersect(names(coef(mod_final))[-1], num_vars)
if (length(final_num_vars) > 0) {
  bt_terms <- paste0(final_num_vars, ":log(", final_num_vars, ")")
  fml_bt <- as.formula(paste("outcome ~", paste(c(attr(terms(mod_final), "term.labels"), bt_terms), collapse = " + ")))
  mod_bt <- glm(fml_bt, data = stroke_df, family = binomial(link = "logit"))
  cat("\n=== Box-Tidwell Test (Linearity in the Logit) ===\n")
  print(tidy(mod_bt) %>% filter(str_detect(term, "log\\(")))
}

# Model specification check (Pregibon's Link Test)
hat_logit <- predict(mod_final, type = "link")
mod_linktest <- glm(outcome ~ hat_logit + I(hat_logit^2), data = stroke_df, family = binomial(link = "logit"))
cat("\n=== Link Test for Model Specification ===\n")
print(tidy(mod_linktest))

# ==============================================================================
# 6. ASSESS: Model Fitness & Diagnostic Stability (using broom)
# ==============================================================================
cat("\n=== 6. ASSESS: Overall Model Fitness ===\n")
print(glance(mod_final))

# McFadden's Pseudo-R2
mcfadden_r2 <- 1 - (mod_final$deviance / mod_final$null.deviance)
cat("McFadden Pseudo-R2:", round(mcfadden_r2, 4), "\n")

# ROC & AUC
roc_obj <- roc(response = stroke_df$outcome, predictor = fitted(mod_final), levels = c("alive", "dead"), direction = "<")
auc_val <- auc(roc_obj)
auc_ci <- ci.auc(roc_obj)
cat(sprintf("AUC: %.4f (95%% CI: %.4f - %.4f)\n", auc_val, auc_ci[1], auc_ci[3]))

# Save ROC plot
png("figures/roc_curve.png", width = 1800, height = 1500, res = 300)
plot(roc_obj, col = "#2c7fb8", lwd = 2.5, main = sprintf("ROC Curve (AUC = %.3f)", auc_val))
abline(a = 0, b = 1, lty = 2, col = "gray50")
dev.off()

# Hosmer-Lemeshow Goodness-of-Fit Test
hl_test <- hoslem.test(as.numeric(stroke_df$outcome) - 1, fitted(mod_final), g = 10)
cat("\n=== Hosmer-Lemeshow Goodness-of-Fit Test ===\n")
print(hl_test)

# Diagnostic stability using broom::augment() + DFBETA + DFFITS
aug_df <- augment(mod_final, type.predict = "response", type.residuals = "deviance") %>%
  mutate(
    obs_id = row_number(),
    pearson_resid = residuals(mod_final, type = "pearson"),
    deviance_resid = residuals(mod_final, type = "deviance"),
    dffits_val = dffits(mod_final),
    max_abs_dfbeta = apply(abs(dfbetas(mod_final)), 1, max)
  )

p_params <- length(coef(mod_final))
n_obs <- nrow(stroke_df)
hat_thresh <- 2 * p_params / n_obs
dffits_thresh <- 2 * sqrt(p_params / n_obs)
dfbeta_thresh <- 2 / sqrt(n_obs)

cat("\n=== Diagnostic Stability Summary ===\n")
cat(sprintf("Thresholds: Leverage > %.4f | |DFFITS| > %.4f | |DFBETA| > %.4f | |Std.Resid| > 2.5\n",
            hat_thresh, dffits_thresh, dfbeta_thresh))
cat("Observations with high leverage:", sum(aug_df$.hat > hat_thresh), "\n")
cat("Observations with |Std. Deviance Resid| > 2.5:", sum(abs(aug_df$.std.resid) > 2.5), "\n")
cat("Observations with |DFFITS| > threshold:", sum(abs(aug_df$dffits_val) > dffits_thresh), "\n")
cat("Observations with max |DFBETA| > threshold:", sum(aug_df$max_abs_dfbeta > dfbeta_thresh), "\n")

dfbeta_by_var <- as_tibble(dfbetas(mod_final)) %>%
  pivot_longer(everything(), names_to = "term", values_to = "dfbeta") %>%
  group_by(term) %>%
  summarise(
    n_exceed_2_sqrt_n = sum(abs(dfbeta) > dfbeta_thresh),
    n_exceed_1 = sum(abs(dfbeta) > 1),
    max_abs_dfbeta = round(max(abs(dfbeta)), 4),
    .groups = "drop"
  )
cat("\n=== DFBETA Instability Breakdown by Model Variable ===\n")
print(dfbeta_by_var)

# Diagnostic plots
p_diag <- aug_df %>%
  select(obs_id, .hat, .std.resid, pearson_resid, dffits_val, max_abs_dfbeta, .cooksd) %>%
  pivot_longer(-obs_id, names_to = "metric", values_to = "value") %>%
  ggplot(aes(x = obs_id, y = value)) +
  geom_point(alpha = 0.7, color = "#2c7fb8", size = 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  facet_wrap(~ metric, scales = "free_y", ncol = 3) +
  theme_minimal(base_size = 11) +
  labs(title = "Logistic Regression Diagnostic Stability Plots", x = "Observation Index", y = NULL)
ggsave("figures/diagnostic_stability.png", p_diag, width = 11, height = 6.5, dpi = 300)

# ==============================================================================
# 7. PRESENTATION: Combined Univariable & Multivariable Table (Log Odds, OR, CI, p)
# ==============================================================================
univ_log_odds <- map_dfr(predictors, function(var) {
  fml <- as.formula(paste("outcome ~", var))
  mod <- glm(fml, data = stroke_df, family = binomial(link = "logit"))
  tidy(mod) %>%
    filter(term != "(Intercept)") %>%
    transmute(
      variable = var,
      term = term,
      univ_log_odds = estimate
    )
})

univ_pres <- univ_results %>%
  left_join(univ_log_odds, by = c("variable", "term")) %>%
  transmute(
    Variable = variable,
    Term = term,
    `Univ. Log Odds (B)` = round(univ_log_odds, 3),
    `Crude OR (95% CI)` = sprintf("%.3f (%.3f - %.3f)", estimate, conf.low, conf.high),
    `Univ. p-value` = ifelse(p.value < 0.001, "<0.001", sprintf("%.3f", p.value))
  )

mv_log_odds <- tidy(mod_final) %>%
  filter(term != "(Intercept)") %>%
  transmute(Term = term, mv_log_odds = estimate)

mv_pres <- final_tidy %>%
  filter(term != "(Intercept)") %>%
  left_join(mv_log_odds, by = c("term" = "Term")) %>%
  transmute(
    Term = term,
    `Multiv. Log Odds (B)` = round(mv_log_odds, 3),
    `Adjusted OR (95% CI)` = sprintf("%.3f (%.3f - %.3f)", estimate, conf.low, conf.high),
    `Multiv. p-value` = ifelse(p.value < 0.001, "<0.001", sprintf("%.3f", p.value))
  )

presentation_table <- univ_pres %>%
  left_join(mv_pres, by = "Term")

cat("\n=== 7. PRESENTATION: Univariable & Multivariable Table (Log Odds, OR, 95% CI, p-value) ===\n")
print(presentation_table, n = 25)

cat("\nAnalysis script completed successfully.\n")
