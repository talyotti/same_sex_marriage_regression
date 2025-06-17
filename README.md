# Same-Sex Marriage Rights Logistic Regression

**R • tidyverse • statsmodels • glm**

## 1. Introduction

This project applies logistic regression to the Northern Ireland Life and Times Survey (2012 LGBT teaching dataset) to identify demographic and attitude-based factors influencing whether respondents believe “same-sex marriages should have the same rights as traditional marriages” (`ssexmarr`).

## 2. Data

- **Source:** Northern Ireland Life and Times Survey, 2012 (LGBT Issues teaching dataset)  
- **Original size:** 1,064 respondents  
- **Final analysis size:** 984 respondents (after handling missing values and dropping non-significant columns)

## 3. Methods

1. **Data Cleaning & Preparation**  
   - Replace negative codes with `NA`; drop categorical predictors with >10% missing, then recode remaining NAs as “missing”  
   - Remove `persinc2` (21.2% missing) to maximize sample size  
2. **Exploratory Analysis**  
   - Continuous variables binned and plotted with logistic fits  
   - Categorical cross-tabs (e.g. `uprejgay` vs. `glchild`) to check dependencies  
3. **Feature Engineering & Selection**  
   - Merge low-frequency factor levels for interpretability  
   - Check multicollinearity via VIF; remove `rsuper` and `work` (VIF > 10)  
   - Backward elimination of non-significant predictors (ANOVA p > 0.05)  
4. **Modeling**  
   - Fit final logistic regression with 11 variables (28 total predictors)  
   - Evaluate via confusion matrix and percent-correct diagnostics

## 4. Results

- **Final predictors:**  
  `glborn`, `glsocdist`, `glchild`, `uprejgay`, `ruhappy`, `polpart2`, `chattnd2`, `eqnow8`, `highqual`, `rsex`, `rage`  
- **Key odds ratios:**  
  - `polpart2 = Social Democratic & Labour Party`: OR = 3.93 (p < 0.001)  
  - `glchild = Neither comfortable nor uncomfortable`: OR = 0.35 (p < 0.001)  
  - `ruhappy = Very unhappy`: OR = 0.05 (p < 0.01)  
  - `eqnow8 = Women treated fairly`: OR = 0.39 (p < 0.05)  
  - `rsex = Female`: OR = 1.61 (p < 0.05)  
  - `rage (age)`: OR = 0.96 per year (p < 0.001)  
- **Confusion matrix diagnostics:**  
  - % correct “No”: 67%  
  - % correct “Yes”: 90%  
  - **Overall accuracy:** 81%
