# RQ1: Student-level linear model (OLS equivalent, weighted)
model_rq1 <- lm.sdf(
  formula = math ~ st004d01t + age + immig + workpay + misssc +
    bsmj + relatst + learres + famsupsl + hisei + homepos +
    ictout + ictwkdy + icteffic + ictenq + ictfeed +
    ictsch + icthome + ictqual,
  data = pisa_dat
)

summary(model_rq1)



# ===============================
#  PISA 2022 USA – Multilevel Models for RQ2–RQ4
#  (with automatic p-values via lmerTest)
# ===============================

# ---- Step 0: 加载必要包 ----
library(lme4)
library(lmerTest)     # ✅ 提供 p 值计算
library(broom)
library(broom.mixed)  # ✅ 支持 tidy(lmer)
library(dplyr)

# ---- Step 1: 从现有 pisa_dat 中提取分析变量 ----
math_data <- pisa_dat %>%
  dplyr::select(
    cntschid, st004d01t, age, immig, workpay, misssc,
    bsmj, relatst, learres, famsupsl, hisei, homepos,
    ictout, ictwkdy, icteffic, ictenq, ictfeed,
    ictsch, icthome, ictqual,
    pv1math:pv10math
  )

cat("\n✅ Data subset complete.\n")
cat("Number of students:", nrow(math_data), "\n")
cat("Number of schools:", length(unique(math_data$cntschid)), "\n\n")

# ---- Step 2: 定义 PV 列表 ----
pv_list <- paste0("pv", 1:10, "math")

# ---- Step 3: 定义模型公式 ----

# RQ2: 学生层 + 学校层主效应
form_rq2 <- paste(
  "~ st004d01t + age + immig + workpay + misssc +",
  "bsmj + relatst + learres + famsupsl + hisei + homepos +",
  "ictout + ictwkdy + icteffic + ictenq + ictfeed +",
  "ictsch + ictqual + (1 | cntschid)"
)

# RQ3: 增强效应（ICT质量 × ICT熟悉度）
form_rq3 <- paste(
  "~ st004d01t + age + immig + workpay + misssc +",
  "bsmj + relatst + learres + famsupsl + hisei + homepos +",
  "ictout + ictwkdy + icteffic + ictenq + ictfeed +",
  "ictqual + ictqual:icteffic + (1 | cntschid)"
)

# RQ4: 补偿效应（ICT质量 × 家庭社会资本）
form_rq4 <- paste(
  "~ st004d01t + age + immig + workpay + misssc +",
  "bsmj + relatst + learres + famsupsl + hisei + homepos +",
  "ictout + ictwkdy + icteffic + ictenq + ictfeed +",
  "ictqual + ictqual:homepos + (1 | cntschid)"
)

# ---- Step 4: 定义函数，循环10个PV并计算平均结果 ----
run_pv_models <- function(pv_list, formula_text, data) {
  results <- lapply(pv_list, function(pv) {
    form <- as.formula(paste(pv, formula_text))
    model <- lmer(form, data = data)
    broom.mixed::tidy(model, effects = "fixed")
  })
  
  # 合并10个模型结果并计算平均值
  bind_rows(results) %>%
    group_by(term) %>%
    summarise(
      est_mean = mean(estimate, na.rm = TRUE),
      se_mean  = mean(std.error, na.rm = TRUE),
      t_mean   = mean(statistic, na.rm = TRUE),
      p_mean   = mean(p.value, na.rm = TRUE)
    ) %>%
    arrange(p_mean)
}

# ---- Step 5: 运行 RQ2、RQ3、RQ4 模型 ----
cat("Running RQ2 models (main effects)...\n")
res_rq2 <- run_pv_models(pv_list, form_rq2, math_data)

cat("Running RQ3 models (enhancement effect)...\n")
res_rq3 <- run_pv_models(pv_list, form_rq3, math_data)

cat("Running RQ4 models (compensatory effect)...\n")
res_rq4 <- run_pv_models(pv_list, form_rq4, math_data)

# ---- Step 6: 打印主要结果 ----
cat("\n===== RQ2: Main Effects =====\n")
print(res_rq2)

cat("\n===== RQ3: Enhancement Effect (ICT Quality × ICT Familiarity) =====\n")
print(res_rq3 %>% filter(grepl("ictqual:icteffic", term)))

cat("\n===== RQ4: Compensatory Effect (ICT Quality × Home Possessions) =====\n")
print(res_rq4 %>% filter(grepl("ictqual:homepos", term)))

# ---- Step 7: 导出结果表 ----
write.csv(res_rq2, "RQ2_results.csv", row.names = FALSE)
write.csv(res_rq3, "RQ3_results.csv", row.names = FALSE)
write.csv(res_rq4, "RQ4_results.csv", row.names = FALSE)

cat("\n✅ All models completed successfully. Results exported to CSV.\n")





# ---- Step 8: 模型比较 ----
library(performance)

# 取第一个PV（pv1math）代表性模型做比较分析
cat("\n🔍 Running model comparison on pv1math...\n")

# 建立三个代表性模型
model_rq2_pv1 <- lmer(pv1math ~ st004d01t + age + immig + workpay + misssc +
                        bsmj + relatst + learres + famsupsl + hisei + homepos +
                        ictout + ictwkdy + icteffic + ictenq + ictfeed +
                        ictsch + ictqual + (1 | cntschid),
                      data = math_data)

model_rq3_pv1 <- lmer(pv1math ~ st004d01t + age + immig + workpay + misssc +
                        bsmj + relatst + learres + famsupsl + hisei + homepos +
                        ictout + ictwkdy + icteffic + ictenq + ictfeed +
                        ictqual + ictqual:icteffic + (1 | cntschid),
                      data = math_data)

model_rq4_pv1 <- lmer(pv1math ~ st004d01t + age + immig + workpay + misssc +
                        bsmj + relatst + learres + famsupsl + hisei + homepos +
                        ictout + ictwkdy + icteffic + ictenq + ictfeed +
                        ictqual + ictqual:homepos + (1 | cntschid),
                      data = math_data)

# ---- 比较 1：似然比检验 (χ²) ----
cat("\n--- Likelihood Ratio Test (RQ2 vs RQ3 vs RQ4) ---\n")
anova(model_rq2_pv1, model_rq3_pv1, model_rq4_pv1)

# ---- 比较 2：AIC / BIC ----
cat("\n--- AIC / BIC ---\n")
AIC(model_rq2_pv1, model_rq3_pv1, model_rq4_pv1)
BIC(model_rq2_pv1, model_rq3_pv1, model_rq4_pv1)

# ---- 比较 3：R²（解释力）----
cat("\n--- Marginal / Conditional R² ---\n")
r2_rq2 <- performance::r2(model_rq2_pv1)
r2_rq3 <- performance::r2(model_rq3_pv1)
r2_rq4 <- performance::r2(model_rq4_pv1)

r2_compare <- data.frame(
  Model = c("RQ2", "RQ3", "RQ4"),
  Marginal_R2 = c(r2_rq2$R2_marginal, r2_rq3$R2_marginal, r2_rq4$R2_marginal),
  Conditional_R2 = c(r2_rq2$R2_conditional, r2_rq3$R2_conditional, r2_rq4$R2_conditional)
)

print(r2_compare)

# 导出比较结果
write.csv(r2_compare, "Model_R2_comparison.csv", row.names = FALSE)

cat("\n✅ Model comparison completed and saved.\n")
