# Implementation Plan — Stroke Fatality Logistic Regression (RREAAAP Workflow)

## Objective
Perform a complete binary logistic regression analysis on `stroke-fatality-cooked2.dta` following the **RREAAAP** workflow (**Reason, Read, Explore, Assumption, Apply, Assess, Presentation**) to identify independent predictors of patient discharge status (`outcome`: `0` = alive, `1` = dead), and deliver a reproducible R script (`analysis.R`) and rendered Quarto report (`report.qmd` → `report.html`).

---

## RREAAAP Workflow Plan

### 1. Reason
- Define the clinical research question: identify independent predictors associated with patient status at discharge (`outcome`: `alive` vs. `dead`) using binary logistic regression in R.

### 2. Read
- Import `stroke-fatality-cooked2.dta` using `haven::read_dta()`.
- Fix/standardize variable names using `janitor::clean_names()` and clear snake_case naming (`marital_status`, `referral_case`, `stroke_type`).
- Convert Stata `dbl+lbl` variables to factors with clinically meaningful reference levels (`alive`, `male`, `no` for comorbidities, `hospital` for referral source, `CI (Ischaemic)` for stroke type).
- Exclude `rn` (registration number) from regression modeling after verifying uniqueness.

### 3. Explore
- Check for missing data across all 22 variables.
- Check for possible data entry errors and logical inconsistencies (`sbp <= dbp`, out-of-range `gcs` or `age`).
- Check for outliers using the IQR rule (`< Q1 - 1.5*IQR` or `> Q3 + 1.5*IQR`).
- Visualize distributions with `tidyverse` (`ggplot2`):
  - **Numerical variables:** histograms and boxplots stratified by `outcome`.
  - **Categorical variables:** bar charts stratified by `outcome`.
- Produce a descriptive summary table stratified by `outcome` using `gtsummary::tbl_summary()`.

### 4. Assumption
- Verify **binomial distribution** assumption (dichotomous `outcome`, independent observations).
- Check **linearity in the logit** for continuous predictors in the final model (Box-Tidwell test: `x:log(x)`).
- Check **model specification** (Pregibon's Link Test: `hat_logit` and `hat_logit^2`) and **multicollinearity** (`car::vif()`).

### 5. Apply
- Perform **univariable logistic regression** (`glm(outcome ~ var, family = binomial)`) for all 20 candidate predictors.
- Screen candidate independent variables (e.g., univariable $p < 0.25$) and confirm suitable reference levels.
- Build the **multivariable logistic regression** model with **no interaction terms**, refining via backward stepwise AIC selection (`step(..., direction = "backward")`).
- Present combined Crude OR and Adjusted OR tables (with 95% CI and p-values) using `gtsummary::tbl_merge()` and `broom::tidy()`.

### 6. Assess
- **Overall model fitness:**
  - ROC curve and Area Under the Curve (`pROC::roc()`, `pROC::auc()`, 95% CI).
  - Hosmer-Lemeshow Goodness-of-Fit test (`ResourceSelection::hoslem.test()`).
  - AIC, BIC, deviance, and McFadden's pseudo-$R^2$ (`broom::glance()`).
- **Diagnostic stability** using `broom::augment()`:
  - Leverage (`.hat`)
  - Pearson residuals and deviance residuals (`.std.resid`)
  - `DFBETA` and `DFFITS`
  - Cook's distance (`.cooksd`)

### 7. Presentation
- Create publication-ready summary tables combining **univariable** and **multivariable** logistic regression results, explicitly displaying:
  - **Log Odds ($\beta$)**
  - **Odds Ratio** (Crude OR and Adjusted OR)
  - **95% Confidence Intervals (CI)**
  - **p-values**

---

## Deliverables & Verification
- `analysis.R`: Reproducible R script implementing all RREAAAP steps and saving plots to `figures/`.
- `report.qmd` & `report.html`: Quarto report presenting tables, plots, assumption checks, and clinical interpretation.
- Verify end-to-end execution via `Rscript.exe analysis.R` and `quarto render report.qmd` with zero errors.
