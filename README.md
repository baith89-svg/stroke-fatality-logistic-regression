# Stroke Fatality Logistic Regression

Binary logistic regression analysis of in-hospital stroke fatality at discharge (`outcome`: alive vs. dead) using `stroke-fatality-cooked2.dta` (N = 226) following the **RREAAAP** workflow (**Reason, Read, Explore, Assumption, Apply, Assess, Presentation**).

## Key Files
- `analysis.R` — Reproducible R script (`tidyverse`, `haven`, `gtsummary`, `broom`, `car`, `pROC`, `ResourceSelection`).
- `report.qmd` / `report.html` — Rendered Quarto report with tables, plots, assumption checks, and diagnostic stability metrics.
- `implementations.md` — Step-by-step RREAAAP implementation plan.
- `AGENTS.md` — Project conventions and environment guide.
