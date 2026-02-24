# =============================================================
# Mixed-Effects Analysis for Pool of Experts Framework Comparison
# =============================================================

# -----------------------------
# 1. Setup and Load Libraries
# -----------------------------
library(lme4)
library(lmerTest)    # p-values for fixed effects
library(emmeans)     # post-hoc comparisons
library(car)         # Anova (Type III tests)
library(broom.mixed) # tidy model outputs

# -----------------------------
# 2. Load and Prepare Data
# -----------------------------
df_fdm <- read.csv("data/results_fdm.csv")
df_mv <- read.csv("data/results_majorities.csv")

# Compute correctness for FDM
df_fdm$is_correct <- as.integer(
  as.character(df_fdm$final_answer_norm) == as.character(df_fdm$golden_answer_norm)
)

# Compute correctness for MV
df_mv$is_correct <- as.integer(
  as.character(df_mv$majority_rand) == as.character(df_mv$golden_answer_norm)
)

# Create unique question identifier
df_fdm$question_id <- paste(df_fdm$dataset, df_fdm$query_idx, sep = "_")
df_mv$question_id <- paste(df_mv$dataset, df_mv$query_idx, sep = "_")

# Set reference levels
df_fdm$desc_framework <- relevel(factor(df_fdm$desc_framework), ref = "no-description")
df_fdm$dataset <- factor(df_fdm$dataset)
df_fdm$model <- factor(df_fdm$model)

df_mv$desc_framework <- relevel(factor(df_mv$desc_framework), ref = "no-description")
df_mv$dataset <- factor(df_mv$dataset)
df_mv$model <- factor(df_mv$model)

# Check structure
cat("FDM data structure:\n")
cat("  N observations:", nrow(df_fdm), "\n")
cat("  N questions:", length(unique(df_fdm$question_id)), "\n")
cat("  N frameworks:", length(unique(df_fdm$desc_framework)), "\n")
cat("  N models:", length(unique(df_fdm$model)), "\n")
cat("  N datasets:", length(unique(df_fdm$dataset)), "\n")

# -----------------------------
# 3. Model 1: Main Effects Only
# -----------------------------
cat("\n========== MODEL 1: Main Effects ==========\n")

m1_fdm <- glmer(
  is_correct ~ desc_framework + dataset + model + (1 | question_id),
  data = df_fdm,
  family = binomial,
  nAGQ = 0, # Laplace approximation
)

summary(m1_fdm)


# Type III Anova - tests overall significance of each factor
cat("\n--- Type III Analysis of Deviance ---\n")
anova_m1 <- Anova(m1_fdm, type = "III")
print(anova_m1)

# Save main effect results
m1_summary <- tidy(m1_fdm, effects = "fixed", conf.int = TRUE)
write.csv(m1_summary, "results/m1_fdm_coefficients.csv", row.names = FALSE)

# -----------------------------
# 4. Model 2: Framework × Dataset Interaction
# -----------------------------
cat("\n========== MODEL 2: Framework × Dataset Interaction ==========\n")

m2_fdm <- glmer(
  is_correct ~ desc_framework * dataset + model + (1 | question_id),
  data = df_fdm,
  family = binomial,
  nAGQ = 0, # Laplace approximation
)

summary(m2_fdm)

# Type III Anova
cat("\n--- Type III Analysis of Deviance ---\n")
anova_m2 <- Anova(m2_fdm, type = "III")
print(anova_m2)

# Compare M1 vs M2: Does interaction improve fit?
cat("\n--- Model Comparison: M1 vs M2 ---\n")
model_comparison <- anova(m1_fdm, m2_fdm)
print(model_comparison)

# Save interaction coefficients
m2_summary <- tidy(m2_fdm, effects = "fixed", conf.int = TRUE)
write.csv(m2_summary, "results/m2_fdm_coefficients.csv", row.names = FALSE)

# -----------------------------
# 5. Model 3: Framework × Model Interaction
# -----------------------------
cat("\n========== MODEL 3: Framework × Model Interaction ==========\n")

m3_fdm <- glmer(
  is_correct ~ desc_framework * model + dataset + (1 | question_id),
  data = df_fdm,
  family = binomial,
  nAGQ = 0, # Laplace approximation
)

# Compare M1 vs M3
cat("\n--- Model Comparison: M1 vs M3 ---\n")
model_comparison_m3 <- anova(m1_fdm, m3_fdm)
print(model_comparison_m3)

# Save
m3_summary <- tidy(m3_fdm, effects = "fixed", conf.int = TRUE)
write.csv(m3_summary, "results/m3_fdm_coefficients.csv", row.names = FALSE)

# -----------------------------
# 6. Post-hoc: Framework Comparisons
# -----------------------------
cat("\n========== POST-HOC COMPARISONS ==========\n")

# 6a. Overall framework comparisons (from M1)
cat("\n--- Overall Framework Effects (M1) ---\n")
emm_framework <- emmeans(m1_fdm, ~ desc_framework, type = "response")
print(emm_framework)

# Pairwise vs no-description
contrast_vs_nd <- contrast(emm_framework, method = "trt.vs.ctrl", ref = "no-description", adjust = "holm")
print(contrast_vs_nd)

# Save
emm_df <- as.data.frame(emm_framework)
write.csv(emm_df, "results/emmeans_framework_overall.csv", row.names = FALSE)

contrast_df <- as.data.frame(contrast_vs_nd)
write.csv(contrast_df, "results/contrasts_vs_nd_overall.csv", row.names = FALSE)

# 6b. Framework comparisons within each dataset (from M2)
cat("\n--- Framework Effects by Dataset (M2) ---\n")
emm_fw_dataset <- emmeans(m2_fdm, ~ desc_framework | dataset, type = "response")
print(emm_fw_dataset)

# Pairwise vs no-description within each dataset
contrast_by_dataset <- contrast(emm_fw_dataset, method = "trt.vs.ctrl", ref = "no-description", adjust = "holm")
print(contrast_by_dataset)

# Save
emm_fw_dataset_df <- as.data.frame(emm_fw_dataset)
write.csv(emm_fw_dataset_df, "results/emmeans_framework_by_dataset.csv", row.names = FALSE)

contrast_by_dataset_df <- as.data.frame(contrast_by_dataset)
write.csv(contrast_by_dataset_df, "results/contrasts_vs_nd_by_dataset.csv", row.names = FALSE)

# -----------------------------
# 7. Dataset-Specific Models (Optional: More Power)
# -----------------------------
cat("\n========== DATASET-SPECIFIC MODELS ==========\n")

datasets <- unique(df_fdm$dataset)
dataset_results <- list()

for (ds in datasets) {
  cat("\n--- Dataset:", ds, "---\n")
  
  df_subset <- df_fdm[df_fdm$dataset == ds, ]
  
  m_ds <- glmer(
    is_correct ~ desc_framework + model + (1 | question_id),
    data = df_subset,
    family = binomial,
    nAGQ = 0, # Laplace approximation
  )
  
  # Anova for framework effect
  anova_ds <- Anova(m_ds, type = "III")
  cat("Framework effect p-value:", anova_ds["desc_framework", "Pr(>Chisq)"], "\n")
  
  # Framework emmeans
  emm_ds <- emmeans(m_ds, ~ desc_framework, type = "response")
  contrast_ds <- contrast(emm_ds, method = "trt.vs.ctrl", ref = "no-description", adjust = "holm")
  
  # Store results
  dataset_results[[ds]] <- list(
    anova = anova_ds,
    emmeans = as.data.frame(emm_ds),
    contrasts = as.data.frame(contrast_ds)
  )
  
  # Save
  write.csv(as.data.frame(emm_ds), paste0("results/emmeans_", ds, ".csv"), row.names = FALSE)
  write.csv(as.data.frame(contrast_ds), paste0("results/contrasts_", ds, ".csv"), row.names = FALSE)
}

# -----------------------------
# 8. Summary Table: Framework Rankings
# -----------------------------
cat("\n========== FRAMEWORK RANKINGS ==========\n")

# Extract framework coefficients from M1
fw_coefs <- m1_summary[grepl("desc_framework", m1_summary$term), ]
fw_coefs$framework <- gsub("desc_framework", "", fw_coefs$term)
fw_coefs <- fw_coefs[, c("framework", "estimate", "std.error", "conf.low", "conf.high", "p.value")]
fw_coefs <- fw_coefs[order(-fw_coefs$estimate), ]

cat("\nFramework effects (log-odds relative to no-description):\n")
print(fw_coefs)

write.csv(fw_coefs, "results/framework_rankings.csv", row.names = FALSE)

# -----------------------------
# 9. Repeat for Majority Voting (Optional)
# -----------------------------
cat("\n========== MAJORITY VOTING ANALYSIS ==========\n")

m1_mv <- glmer(
  is_correct ~ desc_framework + dataset + model + (1 | question_id),
  data = df_mv,
  family = binomial,
  nAGQ = 0, # Laplace approximation
)

anova_m1_mv <- Anova(m1_mv, type = "III")
cat("\n--- Type III Analysis of Deviance (MV) ---\n")
print(anova_m1_mv)

m1_mv_summary <- tidy(m1_mv, effects = "fixed", conf.int = TRUE)
write.csv(m1_mv_summary, "results/m1_mv_coefficients.csv", row.names = FALSE)

# Compare FDM vs MV framework effects
cat("\nFDM framework p-value:", anova_m1["desc_framework", "Pr(>Chisq)"], "\n")
cat("MV framework p-value:", anova_m1_mv["desc_framework", "Pr(>Chisq)"], "\n")

# -----------------------------
# 10. Export Summary for Python Plotting
# -----------------------------
cat("\n========== EXPORTING FOR PYTHON ==========\n")

# Create comprehensive summary
summary_export <- data.frame(
  analysis = c("M1_framework_effect", "M2_interaction_effect", "M1_vs_M2_comparison"),
  chi_sq = c(
    anova_m1["desc_framework", "Chisq"],
    anova_m2["desc_framework:dataset", "Chisq"],
    model_comparison$Chisq[2]
  ),
  df = c(
    anova_m1["desc_framework", "Df"],
    anova_m2["desc_framework:dataset", "Df"],
    model_comparison$Df[2]
  ),
  p_value = c(
    anova_m1["desc_framework", "Pr(>Chisq)"],
    anova_m2["desc_framework:dataset", "Pr(>Chisq)"],
    model_comparison$`Pr(>Chisq)`[2]
  )
)

write.csv(summary_export, "results/analysis_summary.csv", row.names = FALSE)

cat("\nAll results exported to 'results/' directory.\n")
cat("Files created:\n")
cat("  - m1_fdm_coefficients.csv\n")
cat("  - m2_fdm_coefficients.csv\n")
cat("  - m3_fdm_coefficients.csv\n")
cat("  - emmeans_framework_overall.csv\n")
cat("  - contrasts_vs_nd_overall.csv\n")
cat("  - emmeans_framework_by_dataset.csv\n")
cat("  - contrasts_vs_nd_by_dataset.csv\n")
cat("  - emmeans_<dataset>.csv (per dataset)\n")
cat("  - contrasts_<dataset>.csv (per dataset)\n")
cat("  - framework_rankings.csv\n")
cat("  - m1_mv_coefficients.csv\n")
cat("  - analysis_summary.csv\n")

