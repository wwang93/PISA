##########################################
#  PISA 2022 U.S. Multilevel Models (RQ2–RQ4)
#  Author: Wangwei
#  Based on RQ1 results (Weighted Regression)
##########################################

# --- Step 0. Load required packages ---
library(dplyr)
library(lme4)
library(lmerTest)
library(performance)
library(broom.mixed)
library(ggplot2)

# --- Step 1. Prepare the dataset (use your pisa_dat) ---
# Ensure pisa_dat is already a data.frame (not light.edsurvey.data.frame)
# You can extract it using getData(..., addAttributes = TRUE) and then convert:
math_data <- as.data.frame(pisa_dat)

# --- Step 2. Create School-Level Aggregate (Level 2) Variables ---
# Aggregate ICT quality to school level (mean per school)
school_level <- math_data %>%
  group_by(cntschid) %>%
  summarise(ICTQUAL_school = mean(ictqual, na.rm = TRUE))

# Merge school-level variable back to student data
math_data <- left_join(math_data, school_level, by = "cntschid")

# --- Step 3. Clean Key Variables (center and composite) ---
# Group-mean center continuous student-level predictors to separate within- and between-school effects
math_data <- math_data %>%
  group_by(cntschid) %>%
  mutate(
    homepos_c = homepos - mean(homepos, na.rm = TRUE),
    icteffic_c = icteffic - mean(icteffic, na.rm = TRUE)
  ) %>%
  ungroup()

# Composite ICT familiarity index (based on RQ1 results)
math_data <- math_data %>%
  mutate(ICT_Familiarity = scale(rowMeans(cbind(icteffic, ictout), na.rm = TRUE)))

# --- Step 4. Define the Plausible Values (for loop use) ---
pv_list <- paste0("pv", 1:10, "math")

# --- Step 5. Define Model Formulas ---
form_null <- "~ 1 + (1 | cntschid)"
form_rq2 <- "~ st004d01t + age + homepos_c + hisei + ICT_Familiarity + ICTQUAL_school + (1 | cntschid)"
form_rq3 <- "~ st004d01t + age + homepos_c + hisei + ICT_Familiarity * ICTQUAL_school + (1 | cntschid)"
form_rq4 <- "~ st004d01t + age + ICT_Familiarity + ICTQUAL_school * homepos_c + (1 | cntschid)"

# --- Step 6. Define helper function to run PV models ---
run_pv_models <- function(pv_list, formula_text, data) {
  results <- lapply(pv_list, function(pv) {
    form <- as.formula(paste(pv, formula_text))
    model <- lmer(form, data = data)
    broom.mixed::tidy(model, effects = "fixed")
  })
  bind_rows(results) %>%
    group_by(term) %>%
    summarise(
      est_mean = mean(estimate, na.rm = TRUE),
      se_mean  = mean(std.error, na.rm = TRUE),
      t_mean   = mean(statistic, na.rm = TRUE),
      p_mean   = mean(p.value, na.rm = TRUE)
    )
}

# --- Step 7. Run Each Model Using PV1 (for model comparison) ---
cat("Running Null model to compute ICC...\n")
model_null <- lmer(pv1math ~ 1 + (1 | cntschid), data = math_data)
icc_value <- performance::icc(model_null)
print(icc_value)

cat("\nRunning RQ2 model (main effects)...\n")
model_rq2 <- lmer(pv1math ~ st004d01t + age + homepos_c + hisei + ICT_Familiarity + ICTQUAL_school + (1 | cntschid), data = math_data)

cat("Running RQ3 model (cross-level enhancement effect)...\n")
model_rq3 <- lmer(pv1math ~ st004d01t + age + homepos_c + hisei + ICT_Familiarity * ICTQUAL_school + (1 | cntschid), data = math_data)

cat("Running RQ4 model (cross-level compensatory effect)...\n")
model_rq4 <- lmer(pv1math ~ st004d01t + age + ICT_Familiarity + ICTQUAL_school * homepos_c + (1 | cntschid), data = math_data)

# --- Step 8. Compare Model Fit ---
cat("\n--- Model Comparison (AIC/BIC) ---\n")
AIC(model_null, model_rq2, model_rq3, model_rq4)
BIC(model_null, model_rq2, model_rq3, model_rq4)

cat("\n--- Model Comparison (Likelihood Ratio Test) ---\n")
anova(model_null, model_rq2, model_rq3, model_rq4)

cat("\n--- R² and ICC Summary ---\n")
r2_null <- performance::r2(model_null)
r2_rq2 <- performance::r2(model_rq2)
r2_rq3 <- performance::r2(model_rq3)
r2_rq4 <- performance::r2(model_rq4)

r2_compare <- data.frame(
  Model = c("Null", "RQ2", "RQ3", "RQ4"),
  Marginal_R2 = c(r2_null$R2_marginal, r2_rq2$R2_marginal, r2_rq3$R2_marginal, r2_rq4$R2_marginal),
  Conditional_R2 = c(r2_null$R2_conditional, r2_rq2$R2_conditional, r2_rq3$R2_conditional, r2_rq4$R2_conditional)
)
print(r2_compare)

# --- Step 9. Extract key cross-level effects ---
cat("\n--- Cross-Level Interaction Results ---\n")
summary(model_rq3)$coefficients["ICT_Familiarity:ICTQUAL_school",]
summary(model_rq4)$coefficients["ICTQUAL_school:homepos_c",]

# --- Step 10. (Optional) Visualization of cross-level effects ---
# Enhancement Effect
interactions::interact_plot(model_rq3,
                            pred = ICT_Familiarity,
                            modx = ICTQUAL_school,
                            interval = TRUE,
                            plot.points = TRUE) +
  labs(title = "Cross-Level Enhancement Effect: ICT Familiarity × School ICT Quality",
       x = "Student ICT Familiarity (centered)", y = "Predicted Math Achievement")

# Compensatory Effect
interactions::interact_plot(model_rq4,
                            pred = homepos_c,
                            modx = ICTQUAL_school,
                            interval = TRUE,
                            plot.points = TRUE) +
  labs(title = "Cross-Level Compensatory Effect: Socio-Cultural Capital × School ICT Quality",
       x = "Home Possessions (centered)", y = "Predicted Math Achievement")
