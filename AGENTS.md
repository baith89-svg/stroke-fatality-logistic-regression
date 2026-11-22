# AGENTS.md

## Project Overview
This workspace contains the **Logistic Regression Assignment** analyzing stroke fatality at discharge using the Stata dataset `stroke-fatality-cooked2.dta` (226 observations, 22 variables).

## Environment & Tooling
- **R Runtime:** `C:\Program Files\R\R-4.6.1\bin\Rscript.exe`
- **Primary Packages:**
  - Data import & wrangling: `tidyverse`, `haven`
  - Modeling & tidying: `stats::glm(..., family = binomial)`, `broom`
  - Tables & reporting: `gtsummary`, `rmarkdown` / `quarto`
  - Diagnostics & validation: `car` (VIF), `pROC` (ROC/AUC), `ResourceSelection` (Hosmer-Lemeshow)

## Data Conventions
- **Dataset:** `stroke-fatality-cooked2.dta` (read with `haven::read_dta()`).
- **Identifier:** `rn` (registration number) — must be excluded from regression models.
- **Outcome Variable:** `outcome` (`0` = alive, `1` = dead at discharge).
- **Labelled Variables:** Stata `dbl+lbl` variables (`sex`, `outcome`, `hypertension`, `maritalstat`, `malay`, `diabetes`, `dyslipidaemia`, `referralcase`, `stroketype`) should be converted cleanly to factors (e.g., via `haven::as_factor()` or explicit `factor()` with clinically meaningful reference levels).

## Coding & Analysis Standards
- Use `tidyverse` idioms and pipe workflows (`%>%` or `|>`).
- Follow the standard clinical logistic regression workflow:
  1. Data inspection, cleaning, and missing-value check.
  2. Univariable logistic regression screening (Crude OR, 95% CI, p-value).
  3. Multivariable logistic regression modeling (Adjusted OR, 95% CI, p-value).
  4. Model diagnostics (multicollinearity/VIF, goodness-of-fit, discrimination/ROC-AUC).
- Produce both:
  - A reproducible R script (`.R`) for the analysis.
  - A Quarto (`.qmd`) or R Markdown (`.Rmd`) report presenting tables, plots, and clinical interpretation.

## Verification Rules
- Do not report results from memory; always execute the R script and render the report to verify there are no errors or warnings that break output.
- Confirm all reported ORs, 95% CIs, p-values, and diagnostic metrics match the actual script/report output.
