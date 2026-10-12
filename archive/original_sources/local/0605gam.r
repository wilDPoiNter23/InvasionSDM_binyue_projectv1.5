# Original source: 0605gam.r
# Original source location: <LOCAL_CODE_ROOT>/0605gam.r
# Original source SHA-256: 1048194290bef6332ab1ac3bc7e0e2e3cd725389540af610ccb93a739695d2a8
# Public-safe original snapshot: archive/original_sources/local/0605gam.r
# Classification: HISTORICAL
# Publication modifications:
# - machine-specific absolute path literals replaced by configured publication_path aliases
# - local encoding decoded to UTF-8; source bytes hashed above
# - comments and historical private machine paths sanitized
# - working-directory calls retained with configured paths; no analyses executed
# Scientific modification: NONE
# This is full historical source, not a certified unattended/reproducible run.
# Snapshot for inspection; path placeholders are not executable locations.
# Historical PowerShell launcher (run in PowerShell, not R): & "C:\Program Files\R\R-4.5.2\bin\R.exe" --vanilla
.libPaths("<HISTORICAL_PATH_010>")
library(ggplot2)
library(jsonlite)
library(dplyr)
library(tidyr)
library(scales)
library(fmsb)
library(tibble)
library(stringr)
library(lme4)
library(lmerTest) # 加载后，lmer()的结果会自动包含p值
library(performance)
library(mgcv)
library(gratia)
library(patchwork)
library(broom)
windows()
sessionInfo()

install.packages('performance')


setwd("<LOCAL_ANALYSIS_ROOT>/figure/gam_draft")
options(repos = c(CRAN = "https://cloud.r-project.org"))
list.files()
m_idx_rclass <- readRDS('gam_logrecl_riskclass')
# 例：Anthropogenic 组
city_df <- read.csv('gamtest_withpca.csv')
head(city_df)
# ---- GROUP DEFINITIONS (tailored to your names) ----
# Climate: use BIO variables only (avoid duplicating temperature_mean/_range)
clim_cols <- grep("^wc2\\.1_2\\.5m_bio_", names(city_df), value = TRUE)

# Soil: pick core edaphic properties
soil_cols <- c("bdod_mean_2024_2_5min_aligned", "phh2o_mean_2024_2_5min_aligned",
               "soc_mean_2024_2_5min_aligned", 
               "sand_mean_2024_2_5min_aligned", "silt_mean_2024_2_5min_aligned",
               "ocd_mean_2024_2_5min_aligned", "nitrogen_mean_2024_2_5min_aligned",
               "cfvo_mean_2024_2_5min_aligned")

# Anthropogenic: you already computed anthro_pc1, but keep the set for reproducibility
anth_cols <- c("BUILT_S_E2025_2.5min", "VIIRS_2023_2.5min",
               "GDP2020_2.5min", "Pop2020_2.5min")

# Marine (coastal)
marine_cols <- c("salinity_mean", "salinity_range","temperature_mean","temperature_range")

# Geomorphology: use slope & elevation; convert aspect to sine/cosine (optional)
geom_cols <- c("slope", "wc2.1_2.5m_elev")   # aspect will be transformed below

# Reclamation is special: keep as intensity + size, or your PCA already: recl_pressure_pc1
# You already have: recl_frac, log_recl, recl_pressure_pc1
city_df$log_city_area <- log1p(city_df$area_km2)
city_df_df <- as.data.frame(city_df)
make_pc <- function(df, cols, ncomp = 1) {
  X <- df[, cols, drop = FALSE]
  # remove near-constant columns (optional but helpful)
  keep <- sapply(X, function(v) sd(v, na.rm = TRUE) > 1e-8)
  X <- X[, keep, drop = FALSE]
  # complete cases within the group
  cc <- complete.cases(X)
  pc <- prcomp(scale(X[cc, , drop = FALSE]), center = TRUE, scale. = TRUE)
  # scores for rows used to fit PCA
  scores <- rep(NA_real_, nrow(df))
  scores[cc] <- pc$x[, 1:ncomp, drop = FALSE] %*% rep(1, ncomp)  # combine if ncomp>1
  list(score = as.numeric(scores),
       loadings = pc$rotation[, 1:ncomp, drop = FALSE],
       sdev = pc$sdev[1:ncomp],
       varexpl = summary(pc)$importance["Proportion of Variance", 1:ncomp],
       cumvar = summary(pc)$importance["Cumulative Proportion", 1:ncomp])
}

flip_to <- function(idx, ref) ifelse(stats::cor(idx, ref, use = "pairwise") < 0, -idx, idx)
# Example: 1 PC per group (you can set ncomp=2 for Climate if needed)
pc_clim  <- make_pc(city_df_df, clim_cols,  ncomp = 1)
pc_soil  <- make_pc(city_df_df, soil_cols,  ncomp = 1)
pc_marine<- make_pc(city_df_df, marine_cols,ncomp = 1)
pc_geom  <- make_pc(city_df_df, geom_cols,  ncomp = 1)
pc_anth  <- make_pc(city_df_df, anth_cols,  ncomp = 1)

write.csv(city_df,'gamtest_withpca.csv')
names(city_df)
# Attach (with interpretable sign)
city_df$anthro_pc1  <- pc_anth$score
city_df$climate_pc1 <- pc_clim$score
city_df$soil_pc1    <- pc_soil$score
city_df$marine_pc1  <- pc_marine$score
city_df$geom_pc1    <- pc_geom$score
city_df$reclamation_level <- as.factor(city_df$reclamation_level)
m_idx <- gam(
  risk ~ s(reclamation_area, k=5) + s(log_city_area, k=5) +       # reclamation intensity + size
         s(anthro_pc1, k=5) + s(climate_pc1, k=5) +
         s(soil_pc1, k=5) + s(marine_pc1, k=5) + s(geom_pc1, k=5) +
         s(Regions, bs="re"),
  data = city_df, method = "REML"
)
summary(m_idx); AIC(m_idx); concurvity(m_idx, full = TRUE); k.check(m_idx)

m_idx_fs <- gam(
  risk ~ s(reclamation_area, risk_class, bs="fs", k=5) +          # stage-specific curves
         s(log_city_area, k=5) +
         s(anthro_pc1, k=5) + s(climate_pc1, k=5) +
         s(soil_pc1, k=5) + s(marine_pc1, k=5) + s(geom_pc1, k=5) +
         s(Regions, bs="re"),
  data = city_df, method = "REML"
)
summary(m_idx_fs); AIC(m_idx_fs); concurvity(m_idx_fs, full = TRUE); k.check(m_idx_fs)
m_idx_rclass <- gam(
  risk ~ s(log_recl, risk_class, bs="fs", k=5) +          # stage-specific curves
         s(log_city_area, k=5) +
         s(anthro_pc1, k=5) + s(climate_pc1, k=5) +
         s(soil_pc1, k=5) + s(marine_pc1, k=5) + s(geom_pc1, k=5) +
         s(Regions, bs="re"),
  data = city_df, method = "REML"
)
summary(m_idx_rclass); AIC(m_idx_rclass); concurvity(m_idx_rclass, full = TRUE); k.check(m_idx_rclass)
smooths(m_idx_rclass)
draw(m_idx_rclass, select = "s(log_recl,risk_class)", partial_match = TRUE, residuals = TRUE) +
  theme_classic() +
  labs(x = "log(Reclamation area)",
       y = "Partial effect on invasion risk",
       title = "Partial effect of reclamation by risk class")
p_smooths <- draw(m_idx_rclass, residuals = TRUE) + 
  theme_minimal() +
  labs(title = "Supplementary Figure 3: Partial response curves of GAMM smooth terms")

p_smooths
plot(ggpredict(m_idx_rclass, terms = c("log_recl", "risk_class"))) +
  ggtitle("Predicted Risk by log(Reclamation Area) and Risk Class")


# -------------------------------------------------------------------------
# 2. 模型诊断图 (Diagnostics)
# -------------------------------------------------------------------------
# appraise() 会一次性画出 QQ图, Residuals vs Linear 等4张图
p_diag <- appraise(m_idx_rclass) + 
  plot_annotation(title = "Supplementary Figure 4: Model diagnostics")
p_diag <- appraise(m_idx_rclass) + 
  plot_annotation(title = "Supplementary Figure 4: Model diagnostics") & 
  theme_test()
p_diag 

diagnostic_data <- broom::augment(m_idx_rclass, type.predict = "response", type.residuals = "deviance")
 write.csv(diagnostic_data, "Model_Diagnostic_Data.csv")



saveRDS(m_idx_rclass, 'gam_logrecl_riskclass')

 # test the glmm reslut 
lmm_model_1 <- lmer(
  risk ~ 
    log_recl * risk_class +  # 核心：log_recl 与 risk_class 的交互（固定效应）
    log_city_area +
    anthro_pc1 + 
    climate_pc1 +
    soil_pc1 + 
    marine_pc1 + 
    geom_pc1 +
    (1 | Regions),           # 随机截距：不同Regions有不同的基线
  data = city_df,
  REML = TRUE               # 与GAMM的method='REML'保持一致
)
summary(lmm_model_1)

lmm_model_2 <- lmer(
  risk ~ 
    log_recl * risk_class +  # 固定效应交互
    log_city_area +
    anthro_pc1 + 
    climate_pc1 +
    soil_pc1 + 
    marine_pc1 + 
    geom_pc1 +
    (1 | Regions),           # 随机截距
  data = city_df,
  REML = TRUE
)
summary(lmm_model_2)
# 比较AIC和BIC
model_compare <- compare_performance(m_idx_rclass, lmm_model_1, lmm_model_2, rank = TRUE)
print(model_compare)

# 创建一个简洁的表格来比较核心结论
# 查看LMM中交互项的显著性
anova(lmm_model_1) # 


# Generate predicted values for each risk_class
preds <- ggpredict(m_idx_rclass, terms = c("log_recl", "risk_class"))

# Plot using ggplot
plot(preds) +
  labs(
    title = "Predicted Invasion Risk by log(Reclamation Area)",
    x = "log(Reclamation Area)",
    y = "Predicted Risk"
  ) +
  theme_minimal()

ggsave("log_recl_riskclass_smooth.png", width = 7, height = 5, dpi = 300)

sm0 <- smooth_estimates(m_idx_rclass, partial_match = TRUE)
sm_logrecl <- sm0

str(sm_logrecl)
unique(sm_logrecl$risk_class) # 确认 risk_class 的具体因子水平


lab <- grep("log_recl", smooths(m_idx_rclass), value = TRUE)

sm <- smooth_estimates(m_idx_rclass, select = lab)
sm
ggplot(sm, aes(x = log_recl, y = .estimate, colour = risk_class, fill = risk_class)) +
  geom_ribbon(aes(ymin = .estimate - 2*.se, ymax = .estimate + 2*.se),
              alpha = 0.20, colour = NA) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "log(Reclamation area)",
       y = "Partial effect (s)",
       title = "Partial effect of reclamation area",
       subtitle = "Shaded bands ≈ 95% CI") +
  theme_classic() +
  theme(legend.title = element_blank())


sm_logrecl <- data.frame(
  log_recl = rep(seq(0, 10, length.out = 100), 2),
  .estimate = c(
    -0.05 + 0.3 * exp(-((seq(0, 10, length.out = 100) - 3)^2)/5) - 0.1 * seq(0, 10, length.out = 100) / 10, # High Risk example
    0.05 + 0.1 * exp(-((seq(0, 10, length.out = 100) - 4)^2)/8) - 0.05 * seq(0, 10, length.out = 100) / 10  # Low Risk example
  ),
  .se = rep(0.03, 200) * (1 + 0.1 * sin(seq(0, 10, length.out = 100))), # example se
  risk_class = factor(rep(c("1", "2"), each = 100)) # 假设原始是 "1", "2"
)

# 确保 risk_class 是因子，并且设置好水平顺序和标签
sm_logrecl <- sm_logrecl %>%
  mutate(risk_class = factor(risk_class, levels = c("1", "2"), labels = c("Low Risk", "High Risk")))

p1 <- ggplot(sm_logrecl,
             aes(x = log_recl, y = .estimate,
                 colour = risk_class, fill = risk_class)) +
  geom_ribbon(aes(ymin = .estimate - 2 * .se,
                  ymax = .estimate + 2 * .se),
              alpha = 0.15, # colour = NA 可能会导致边缘问题，移除或设为与 fill 相同
              colour = NA,  # 尝试保留 NA，如果问题解决，那可能是其他原因
              linetype = 0 # 尝试将置信区间的边界线设为透明
              ) +
  geom_line(size = 1.1) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3) +
  labs(x = "Log-transformed Reclamation Area", # 更具体的X轴标签
       y = "Partial Effect on Risk", # 专业Y轴标签
       title = "Nonlinear Effect of Reclamation by Invasion Risk Class",
       subtitle = "GAM smooths (fs-basis); Ribbons = ±2 SE") + # 修正拼写
  theme_minimal(base_size = 14) + # 使用 theme_minimal 并设置基础字体大小
  theme(legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5), # 标题居中
        plot.subtitle = element_text(hjust = 0.5)) + # 副标题居中
  scale_colour_manual(values = c("Low Risk" = "steelblue", "High Risk" = "firebrick")) + # 自定义颜色
  scale_fill_manual(values = c("Low Risk" = "steelblue", "High Risk" = "firebrick"))

p1
st <- summary(m_idx_rclass)$s.table
st

p4a <- draw(m_idx_rclass, select = "s(climate_pc1)") + labs(
  title = "Partial effect: Climate PC1", y = "Partial effect on risk")
p4b <- draw(m_idx_rclass, select = "s(soil_pc1)") + labs(
  title = "Partial effect: Soil PC1", y = "Partial effect on risk")
p4a; p4b
typ <- make_typical(city_df)
# 首先，列出模型中使用的所有变量
model_vars <- c("log_recl,risk_class", "risk_class", "log_city_area", "anthro_pc1", 
                "climate_pc1", "soil_pc1", "marine_pc1", "geom_pc1", "Regions")
city_df$Regions
# 创建典型值数据框，只包含模型需要的变量
typ <- city_df %>%
  select(all_of(model_vars)) %>%
  summarize(across(where(is.numeric), mean, na.rm = TRUE),
            across(where(is.factor), ~ levels(.x)[which.max(table(.x))]))
typ <- city_df %>%
  select(all_of(model_vars))

typ$Regions <- as.factor(typ$Regions)
# 创建网格数据
grid2d <- expand_grid(
  log_recl = seq(min(city_df$log_recl, na.rm = TRUE),
                 max(city_df$log_recl, na.rm = TRUE), 
                 length.out = 80),
  anthro_pc1 = seq(min(city_df$anthro_pc1, na.rm = TRUE),
                   max(city_df$anthro_pc1, na.rm = TRUE),
                   length.out = 80)
) %>%
  # 添加其他变量的典型值
  mutate(
    log_city_area = typ$log_city_area,
    climate_pc1 = typ$climate_pc1,
    soil_pc1 = typ$soil_pc1,
    marine_pc1 = typ$marine_pc1,
    geom_pc1 = typ$geom_pc1,
    Regions = typ$Regions,
    risk_class = factor(
      names(sort(table(city_df$risk_class), decreasing = TRUE))[1],
      levels = levels(city_df$risk_class)
    )
  )

# 现在尝试预测
pr2 <- predict(m_idx_rclass, newdata = grid2d, type = "response")

pr2 <- predict(m_idx_rclass, newdata = grid2d, type = "response")
grid2d$fit <- pr2

p3 <- ggplot(grid2d, aes(log_recl, anthro_pc1, z = fit)) +
  geom_contour_filled(bins = 12) +
  labs(x = "log(Reclamation area)", y = "Socioeconomic index (PC1)",
       title = "Joint response of risk to reclamation and socioeconomic pressure") +
  guides(fill = guide_colorsteps(title = "Predicted risk")) +
  theme_classic()
p3



# list all smooths in the model
sm_names <- gratia::smooths(m_idx_rclass)
sm_names
# pick those for log_recl (handles fs-basis)
logrecl_smooths <- grep("log_recl", sm_names, value = TRUE)
logrecl_smooths

der <- gratia::derivatives(
  m_idx_rclass,
  select = logrecl_smooths,   # <- use `select`, not `term`
  n = 200,                    # grid resolution along log_recl
  type = "central",           # central finite differences
  interval = "confidence",    # returns lower/upper (approx 95% CI)
  partial_match = TRUE
)

der2 <- der %>%
  rename(x = log_recl,
         lower = .lower_ci,
         upper = .upper_ci,
         grp   = risk_class) 

ggplot(der2, aes(x = x, y = .derivative, colour = grp, fill = grp)) +
  geom_ribbon(aes(ymin = lower, ymax = upper),
              alpha = 0.15, colour = NA) +
  geom_line(linewidth = 1.05) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "log(Reclamation area)",
       y = "d(Risk) / d log(Reclamation area)",
       title = "Rate of change of invasion risk with reclamation") +
  theme_classic() +
  theme(legend.title = element_blank())

# 提取所有平滑项
sm_all <- smooth_estimates(m_idx_rclass)

# 挑选几个最重要的变量
sel_terms <- c("s(log_recl)", "s(anthro_pc1)", "s(climate_pc1)", "s(soil_pc1)")

sm_plot <- sm_all %>% filter(smooth %in% sel_terms)

ggplot(sm_plot, aes(x = x, y = est)) +
  geom_ribbon(aes(ymin = est - 2*se, ymax = est + 2*se), alpha = 0.2) +
  geom_line(size = 1) +
  facet_wrap(~smooth, scales = "free_x") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_classic()


plot_smooth_panel <- function(model, term, label = NULL, data = NULL,
                              stars = TRUE, group = TRUE) {
  all_smooths <- smooths(model)
  lab <- grep(term, all_smooths, value = TRUE)
  if (length(lab) == 0) stop(sprintf("Term '%s' not found in model", term))
  lab <- lab[1]

  if (is.null(data)) data <- model.frame(model)
  sm  <- smooth_estimates(model, select = lab, partial_match = TRUE)
  res <- add_partial_residuals(model = model, data = data, smooth = lab)

  xvar <- sub("^s\\(([^,\\)]+).*$", "\\1", lab)
  resid_col <- lab

  # 提取显著性
  st <- summary(model)$s.table
  row_id <- grep(term, rownames(st))
  ptxt <- NULL; ttl <- label %||% term
  if (length(row_id) > 0) {
    pv <- tryCatch(st[row_id[1], "p-value"], error = function(e) NA)
    if (!is.na(pv)) {
      ptxt <- paste0("p = ", signif(pv, 3))
      if (stars) {
        ttl <- paste0(ttl,
                      if (pv < .001) " ***"
                      else if (pv < .01) " **"
                      else if (pv < .05) " *"
                      else "")
      }
    }
  }

  # ==============================
  # 情况 A: 如果 term 里有 log_recl → 用 risk_class 分组 + 红蓝
  # ==============================
  if (grepl("log_recl", term)) {
    p <- ggplot() +
      geom_point(data = res,
                 aes(x = !!sym(xvar), y = !!sym(resid_col),
                     colour = risk_class),
                 alpha = 0.6, size = 2) +
      geom_ribbon(data = sm,
                  aes(x = !!sym(xvar),
                      ymin = .estimate - 2*.se,
                      ymax = .estimate + 2*.se,
                      fill = risk_class),
                  alpha = 0.2, colour = NA) +
      geom_line(data = sm,
                aes(x = !!sym(xvar), y = .estimate, colour = risk_class),
                linewidth = 1.2) +
      scale_color_manual(values = c("1" = "#D55E00", "2" = "#0072B2")) +
      scale_fill_manual(values = c("1" = "#D55E00", "2" = "#0072B2"))
  } else {
  # ==============================
  # 情况 B: 其他变量 → label 映射到 panel_colors
  # ==============================
    sm$var_label <- label
    res$var_label <- label

    p <- ggplot() +
      geom_point(data = res,
                 aes(x = !!sym(xvar), y = !!sym(resid_col),
                     colour = var_label),
                 alpha = 0.6, size = 2) +
      geom_ribbon(data = sm,
                  aes(x = !!sym(xvar),
                      ymin = .estimate - 2*.se,
                      ymax = .estimate + 2*.se,
                      fill = var_label),
                  alpha = 0.2, colour = NA) +
      geom_line(data = sm,
                aes(x = !!sym(xvar), y = .estimate,
                    colour = var_label),
                linewidth = 1.2) +
      scale_color_manual(values = panel_colors) +
      scale_fill_manual(values = panel_colors)
  }

  p +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x = label %||% xvar, y = "Partial effect (s)",
         title = ttl, subtitle = ptxt) +
    theme_classic() + 
    theme(legend.title = element_blank(),
          plot.title = element_text(size = 10),
          plot.subtitle = element_text(size = 9, colour = "grey40"),  
          legend.position = "inside",
          legend.position.inside = c(0.05, 0.95),       # 图例放在左上角
      legend.justification = c("left", "top"), # 左上角对齐
      legend.background = element_rect(fill = alpha("white", 0.6), colour = NA), # 半透明背景
      legend.key.size = unit(0.6, "lines"),    # 缩小图例
      legend.text = element_text(size = 8))     # 小字号)
}

panels <- list(
  plot_smooth_panel(m_idx_rclass, "anthro_pc1",     "Socioeconomic PC1", group = FALSE),
  plot_smooth_panel(m_idx_rclass, "climate_pc1",    "Climate PC1", group = FALSE),
  plot_smooth_panel(m_idx_rclass, "soil_pc1",       "Soil PC1", group = FALSE),
  plot_smooth_panel(m_idx_rclass, "log_city_area",  "log(City area)", group = FALSE),
  plot_smooth_panel(m_idx_rclass, "marine_pc1",     "Marine PC1", group = FALSE),
  plot_smooth_panel(m_idx_rclass, "geom_pc1",       "Topography PC1", group = FALSE)
)

panel_colors <- c(
  "Soil PC1"         = "#8C510A",  # brown
  "Marine PC1"       = "#1b00b2",  # blue
  "Climate PC1"      = "#755977",  # bluish green
  "Socioeconomic PC1"= "#948049",  # orange
  "Topography PC1"   = "#436845",  # purple
  "log(City area)"   = "#8d862b"   # yellow
)

wrap_plots(panels, ncol = 2) +
  plot_annotation(title = "Partial effects with fixed top-journal palette")

windows()


lab <- grep("log_recl", smooths(m_idx_rclass), value = TRUE)


# 提取平滑估计
sm <- smooth_estimates(m_idx_rclass, select = lab, partial_match = TRUE)

# 提取偏残差
res <- add_partial_residuals(model = m_idx_rclass, data =  model.frame(m_idx_rclass), smooth = lab)
sm <- sm %>%
  dplyr::rename(risk_class = .by) 
head(sm)
names(res)
ggplot() +
  # 残差散点
  geom_point(data = res,
             aes(x = log_recl,
                 y = `s(log_recl,risk_class)`,
                 colour = risk_class),
             alpha = 0.7, size = 5) +
  # 阴影带
  geom_ribbon(data = sm,
              aes(x = log_recl,
                  ymin = .estimate - 2*.se,
                  ymax = .estimate + 2*.se,
                  fill = risk_class),
              alpha = 0.2, colour = NA) +
  # 平滑曲线
  geom_line(data = sm,
            aes(x = log_recl, y = .estimate, colour = risk_class),
            linewidth = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "log(Reclamation area)",
       y = "Partial effect (s)",
       title = "Partial effect of reclamation with partial residuals") +
  theme_classic() +
  theme(legend.title = element_blank())


st <- summary( m_idx_rclass)$s.table
rownames(st)
st

  row_id <- grep(term, rownames(st))
  pval <- if (length(row_id) > 0) signif(st[row_id[1], "p-value"], 3)



m_idx_landclass <- gam(
  risk ~ s(log_recl, Land_Use_Types, bs="fs", k=5) +          # stage-specific curves
         s(log_city_area, k=5) +
         s(anthro_pc1, k=5) + s(climate_pc1, k=5) +
         s(soil_pc1, k=5) + s(marine_pc1, k=5) + s(geom_pc1, k=5) +
         s(Regions, bs="re"),
  data = city_df, method = "REML"
)
summary(m_idx_landclass); AIC(m_idx_landclass); concurvity(m_idx_landclass, full = TRUE); k.check(m_idx_landclass)

m_idx_reclaclass <- gam(
  risk ~ s(log_recl, reclamation_level, bs="fs", k=5) +          # stage-specific curves
         s(log_city_area, k=5) +
         s(anthro_pc1, k=5) + s(climate_pc1, k=5) +
         s(soil_pc1, k=5) + s(marine_pc1, k=5) + s(geom_pc1, k=5) +
         s(Regions, bs="re"),
  data = city_df, method = "REML"
)
summary(m_idx_reclaclass); AIC(m_idx_reclaclass); concurvity(m_idx_reclaclass, full = TRUE); k.check(m_idx_reclaclass)

# Generate predicted values for each risk_class
preds <- ggpredict(m_idx_reclaclass, terms = c("log_recl", "reclamation_level"))

# Plot using ggplot
plot(preds) +
  labs(
    title = "Predicted Invasion Risk by log(Reclamation Area)",
    x = "log(Reclamation Area)",
    y = "Predicted Risk"
  ) +
  theme_minimal()

ggsave("log_recl_reclamation_level_smooth.png", width = 7, height = 5, dpi = 300)

names(city_df)
# 确保数据中有经纬度（Longitude, Latitude）
model_vars <- c("risk", "log_recl", "risk_class", "log_city_area",
                "anthro_pc1", "climate_pc1", "soil_pc1",
                "marine_pc1", "geom_pc1", "Regions",
                "Longitude", "Latitude")

# 创建一个只包含这些变量的新数据框，并删除任何含有NA的行
city_df_clean <- city_df %>%
  select(all_of(model_vars)) %>%  # 只选择需要的变量
  drop_na()                       # 删除任何含有NA的行

# 检查新数据集的大小
dim(city_df_clean)
city_df_clean <- city_df_clean %>%
  mutate(
    risk_class = as.factor(risk_class),
    Regions = as.factor(Regions)
  )
dim(city_df)

m_spatial <- bam(
  risk ~ 
    s(log_recl, risk_class, bs="fs", k=5) +
    s(log_city_area, k=5) +
    s(anthro_pc1, k=5) + s(climate_pc1, k=5) +
    s(soil_pc1, k=5) + s(marine_pc1, k=5) + s(geom_pc1, k=5) +
    s(Regions, bs="re") +
    # 加入空间平滑项：高斯过程平滑，非常适合地理坐标
    s(Longitude, Latitude, bs = "gp", k = 50), # k可以大一些，gp平滑能有效防止过拟合
    
  data = city_df_clean, 
  method = "REML"
)

# 比较模型：空间模型是否更好？
AIC(m_idx_rclass, m_spatial)

m_interaction <- bam(
  risk ~ 
    # 主效应
    s(log_recl, k=5) + 
    s(climate_pc1, k=5) +
    # 纯粹的交互效应（注意：必须包含上述主效应）
    ti(log_recl, climate_pc1, k=5) +
    
    s(log_city_area, k=5) +
    s(anthro_pc1, k=5) + 
    s(soil_pc1, k=5) + s(marine_pc1, k=5) + s(geom_pc1, k=5) +
    s(Regions, bs="re"),
  data = city_df_clean, 
  method = "REML"
)
# 可视化：绘制2D等高线图或3D曲面图
plot(m_interaction, scheme = 2, pages = 1) # scheme=2绘制等高线图
vis.gam(m_interaction, view = c("log_recl", "climate_pc1"), plot.type = "persp") # 3D图
ti(log_recl, climate_pc1)


recl_vars <- c("log_recl", "reclamation_area", "recl_frac")
segments <- c("risk_class", "Land_Use_Types", "climate_zone", "reclamation_level", "reclamation_level_manual")

results <- list()

for (recl in recl_vars) {
  for (seg in c("None", segments)) {
    fmla <- if (seg == "None") {
      as.formula(paste0(
        "risk ~ s(", recl, ", k=5) + ",
        "s(log_city_area, k=5) + ",
        "s(anthro_pc1, k=5) + s(climate_pc1, k=5) + ",
        "s(soil_pc1, k=5) + s(marine_pc1, k=5) + s(geom_pc1, k=5) + ",
        "s(Regions, bs='re')"
      ))
    } else {
      as.formula(paste0(
        "risk ~ s(", recl, ", ", seg, ", bs='fs', k=5) + ",
        "s(log_city_area, k=5) + ",
        "s(anthro_pc1, k=5) + s(climate_pc1, k=5) + ",
        "s(soil_pc1, k=5) + s(marine_pc1, k=5) + s(geom_pc1, k=5) + ",
        "s(Regions, bs='re')"
      ))
    }

    m <- gam(fmla, data = city_df, method = "REML")

    results[[paste(recl, seg, sep = "_")]] <- list(
      adj_r2 = summary(m)$r.sq,
      dev_exp = summary(m)$dev.expl,
      aic = AIC(m),
      recl_signif = summary(m)$s.table[1, 4] < 0.05
    )
  }
}

results 
# 创建数据框
data <- data.frame(
  Reclamation_Variable = c(rep("log_recl", 6), rep("reclamation_area", 6), rep("recl_frac", 6)),
  Segment = rep(c("risk_class", "Land_Use_Types", "climate_zone", 
                 "reclamation_level", "reclamation_level_manual", "None"), 3),
  Adj_R2 = c(0.877, 0.686, 0.727, 0.696, 0.697, 0.721,
             0.858, 0.686, 0.714, 0.689, 0.689, 0.690,
             0.871, 0.728, 0.735, 0.732, 0.756, 0.782),
  Dev_Explained = c(0.905, 0.747, 0.786, 0.751, 0.752, 0.772,
                    0.892, 0.747, 0.778, 0.751, 0.751, 0.754,
                    0.901, 0.805, 0.793, 0.788, 0.811, 0.829),
  AIC = c(-263.1234, -184.4492, -195.1138, -190.9371, -191.1493, -198.7322,
          -246.394, -184.4496, -189.1681, -184.2693, -184.2761, -184.3205,
          -257.3783, -182.3434, -198.1962, -198.4889, -205.4484, -210.8844),
  Reclamation_Signif = c(TRUE, FALSE, TRUE, TRUE, TRUE, TRUE,
                         TRUE, FALSE, TRUE, FALSE, FALSE, FALSE,
                         TRUE, FALSE, TRUE, TRUE, TRUE, FALSE)
)

# 对AIC进行归一化处理
min_aic <- min(data$AIC)
max_aic <- max(data$AIC)
data$AIC_normalized <- 1 - ((data$AIC - min_aic) / (max_aic - min_aic))


# 选择顶刊常用的三色方案
colors <- brewer.pal(3, "Set1")
names(colors) <- c("log_recl", "reclamation_area", "recl_frac")

# 准备雷达图数据
prepare_radar_data <- function(data) {
  # 转换为长格式
  radar_long <- data %>%
    select(Reclamation_Variable, Segment, Adj_R2, Dev_Explained, AIC_normalized, Reclamation_Signif) %>%
    pivot_longer(cols = c(Adj_R2, Dev_Explained, AIC_normalized), 
                 names_to = "Metric", values_to = "Value")
  
  # 为雷达图创建角度坐标
  metrics <- c("Adj_R2", "Dev_Explained", "AIC_normalized")
  angle_step <- 2 * pi / length(metrics)
  
  radar_long <- radar_long %>%
    mutate(
      Metric = factor(Metric, levels = metrics),
      Angle = (as.numeric(Metric) - 1) * angle_step,
      x = Value * sin(Angle),
      y = Value * cos(Angle),
      Signif_Label = ifelse(Reclamation_Signif, "Sig", "Not sig"),
      Group_Label = paste(Reclamation_Variable, Signif_Label)
    )
  
  return(radar_long)
}

# 创建单个Segment的雷达图
create_radar_plot <- function(segment_name, radar_data, colors) {
  segment_data <- radar_data %>% filter(Segment == segment_name)
  
  # 创建刻度圆环
  circles <- data.frame(
    r = c(0.2, 0.4, 0.6, 0.8, 1.0)
  )
  
  # 创建轴线数据
  metrics <- c("Adj_R2", "Dev_Explained", "AIC_normalized")
  axis_data <- data.frame(
    Metric = rep(metrics, each = 2),
    Value = rep(c(0, 1), length(metrics))
  ) %>%
    mutate(
      Metric = factor(Metric, levels = metrics),
      Angle = (as.numeric(Metric) - 1) * (2 * pi / length(metrics)),
      x = Value * sin(Angle),
      y = Value * cos(Angle)
    )
  
  # 创建轴标签
  axis_labels <- data.frame(
    Metric = metrics,
    Angle = (0:(length(metrics)-1)) * (2 * pi / length(metrics)),
    x = 1.15 * sin((0:(length(metrics)-1)) * (2 * pi / length(metrics))),
    y = 1.15 * cos((0:(length(metrics)-1)) * (2 * pi / length(metrics))),
    Label = c("Adj_R²", "Dev_Explained", "AIC_norm")
  )
  
  # 创建刻度标签
  scale_labels <- data.frame(
    r = c(0.2, 0.4, 0.6, 0.8, 1.0),
    x = 0,
    y = c(0.2, 0.4, 0.6, 0.8, 1.0)
  )
  
  ggplot() +
    # 绘制刻度圆环
    geom_path(data = circles %>% 
                crossing(Angle = seq(0, 2*pi, length.out = 100)) %>%
                mutate(x = r * sin(Angle), y = r * cos(Angle)),
              aes(x = x, y = y, group = r), 
              color = "grey80", size = 0.3, linetype = "solid") +
    
    # 绘制轴线
    geom_path(data = axis_data %>% group_by(Metric),
              aes(x = x, y = y), 
              color = "grey60", size = 0.5) +
    
    # 添加刻度标签
    geom_text(data = scale_labels,
              aes(x = x + 0.05, y = y, label = r),
              size = 3, color = "grey50", hjust = 0) +
    
    # 绘制数据多边形
    geom_polygon(data = segment_data, 
                 aes(x = x, y = y, group = Reclamation_Variable, 
                     fill = Reclamation_Variable, color = Reclamation_Variable,
                     linetype = Reclamation_Signif),
                 alpha = 0.2, size = 1) +
    
    # 绘制数据点
    geom_point(data = segment_data, 
               aes(x = x, y = y, color = Reclamation_Variable,
                   shape = Reclamation_Signif),
               size = 3) +
    
    # 添加轴标签
    geom_text(data = axis_labels, 
              aes(x = x, y = y, label = Label), 
              size = 4, color = "black", fontface = "bold") +
    
    # 设置坐标系统
    coord_equal() +
    scale_x_continuous(limits = c(-1.3, 1.3), expand = c(0, 0)) +
    scale_y_continuous(limits = c(-1.3, 1.3), expand = c(0, 0)) +
    
    # 设置颜色和样式
    scale_color_manual(values = colors) +
    scale_fill_manual(values = colors) +
    scale_linetype_manual(values = c("TRUE" = "solid", "FALSE" = "dashed")) +
    scale_shape_manual(values = c("TRUE" = 16, "FALSE" = 4)) +
    
    # 主题设置
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold", margin = margin(b = 10)),
      legend.position = "right",
      legend.title = element_text(face = "bold"),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA)
    ) +
    
    # 标签和标题
    labs(
      title = segment_name,
      color = "Reclamation indicators",
      fill = "Reclamation indicators",
      linetype = "Significant",
      shape = "Significant"
    ) +
    
    # 图例指导
    guides(
      color = guide_legend(override.aes = list(size = 3, linetype = 0)),
      fill = "none",
      linetype = guide_legend(override.aes = list(color = "black", fill = NA)),
      shape = guide_legend(override.aes = list(color = "black"))
    )
}

# 准备数据
radar_data <- prepare_radar_data(data)

# 获取所有Segment
segments <- unique(radar_data$Segment)

# 为每个Segment创建雷达图
plots <- list()
for (segment in segments) {
  plots[[segment]] <- create_radar_plot(segment, radar_data, colors)
}

combined_plot <- arrangeGrob(
  plots[["risk_class"]], plots[["Land_Use_Types"]], plots[["climate_zone"]],
  plots[["reclamation_level"]], plots[["reclamation_level_manual"]], plots[["None"]],
  nrow = 2, ncol = 3
)
combined_plot
ggsave("combined_radar_plots.png", combined_plot, 
       width = 18, height = 12, dpi = 300, bg = "#ffffff00")

# 筛选log_recl数据
log_recl_data <- data %>%
  filter(Reclamation_Variable == "log_recl") %>%
  select(Segment, Adj_R2, Dev_Explained, AIC, Reclamation_Signif)

# 查看log_recl在各Segment的表现
print("log_recl在各Segment的性能表现:")
print(log_recl_data)

# 准备log_recl专用数据
log_recl_radar_data <- data %>%
  filter(Reclamation_Variable == "log_recl") %>%
  select(Segment, Adj_R2, Dev_Explained, AIC, Reclamation_Signif)

# 对AIC进行最小-最大归一化
min_aic <- min(log_recl_radar_data$AIC)
max_aic <- max(log_recl_radar_data$AIC)
log_recl_radar_data$AIC_normalized <- 1 - ((log_recl_radar_data$AIC - min_aic) / (max_aic - min_aic))

# 创建log_recl专用的雷达图函数
create_log_recl_radar_plot <- function(segment_name, radar_data) {
  segment_data <- radar_data %>% filter(Segment == segment_name)
  
  # 准备雷达图数据
  metrics <- c("Adj_R2", "Dev_Explained", "AIC_normalized")
  values <- c(segment_data$Adj_R2, segment_data$Dev_Explained, segment_data$AIC_normalized)
  
  # 创建数据框
  plot_data <- data.frame(
    Metric = factor(metrics, levels = metrics),
    Value = values,
    Angle = (0:2) * (2 * pi / 3)
  ) %>%
    mutate(
      x = Value * sin(Angle),
      y = Value * cos(Angle)
    )
  
  # 创建完整的多边形数据（闭合）
  polygon_data <- rbind(plot_data, plot_data[1, ])
  
  # 创建刻度圆环和轴线
  circles <- data.frame(r = c(0.2, 0.4, 0.6, 0.8, 1.0))
  axis_labels <- data.frame(
    Metric = metrics,
    Label = c("Adj_R²", "Dev_Explained", "AIC_norm"),
    Angle = (0:2) * (2 * pi / 3),
    x = 1.15 * sin((0:2) * (2 * pi / 3)),
    y = 1.15 * cos((0:2) * (2 * pi / 3))
  )
  
  # 设置颜色（根据显著性）
  line_color <- ifelse(segment_data$Reclamation_Signif, "#E41A1C", "#377EB8")  # 红色=显著，蓝色=不显著
  fill_color <- ifelse(segment_data$Reclamation_Signif, "#E41A1C", "#377EB8")
  
  ggplot() +
    # 绘制刻度圆环
    geom_path(data = circles %>% 
                crossing(Angle = seq(0, 2*pi, length.out = 100)) %>%
                mutate(x = r * sin(Angle), y = r * cos(Angle)),
              aes(x = x, y = y, group = r), 
              color = "grey80", size = 0.3) +
    
    # 绘制轴线
    geom_segment(data = axis_labels,
                 aes(x = 0, y = 0, xend = x*0.95, yend = y*0.95),
                 color = "grey60", size = 0.5) +
    
    # 绘制数据多边形
    geom_polygon(data = polygon_data,
                 aes(x = x, y = y),
                 fill = alpha(fill_color, 0.2), color = line_color, 
                 size = 1.5, linetype = ifelse(segment_data$Reclamation_Signif, "solid", "dashed")) +
    
    # 绘制数据点
    geom_point(data = plot_data,
               aes(x = x, y = y),
               color = line_color, size = 4, shape = ifelse(segment_data$Reclamation_Signif, 16, 4)) +
    
    # 添加轴标签
    geom_text(data = axis_labels,
              aes(x = x, y = y, label = Label),
              size = 5, color = "black", fontface = "bold") +
    
    # 添加刻度标签
    geom_text(data = data.frame(x = 0.05, y = c(0.2, 0.4, 0.6, 0.8, 1.0), label = c("0.2", "0.4", "0.6", "0.8", "1.0")),
              aes(x = x, y = y, label = label),
              size = 3, color = "grey50", hjust = 0) +
    
    # 添加性能数值标签
    geom_text(data = plot_data,
              aes(x = x * 0.85, y = y * 0.85, 
                  label = paste0(round(Value, 3))),
              size = 4, color = "darkred", fontface = "bold") +
    
    coord_equal() +
    scale_x_continuous(limits = c(-1.3, 1.3)) +
    scale_y_continuous(limits = c(-1.3, 1.3)) +
    
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold", margin = margin(b = 15)),
      plot.subtitle = element_text(hjust = 0.5, size = 12, color = "grey50"),
      panel.background = element_rect(fill = "white", color = NA)
    ) +
    
    labs(
      title = paste("Segment:", segment_name),
      subtitle = paste("log_recl指标 |", ifelse(segment_data$Reclamation_Signif, "统计显著", "统计不显著"))
    )
}

# 为每个Segment创建雷达图
segments <- unique(log_recl_radar_data$Segment)
log_recl_plots <- list()

for (segment in segments) {
  log_recl_plots[[segment]] <- create_log_recl_radar_plot(segment, log_recl_radar_data)
}

# 排列并显示所有雷达图
grid.arrange(
  log_recl_plots[["risk_class"]], log_recl_plots[["Land_Use_Types"]], log_recl_plots[["climate_zone"]],
  log_recl_plots[["reclamation_level"]], log_recl_plots[["reclamation_level_manual"]], log_recl_plots[["None"]],
  nrow = 2, ncol = 3,
  top = textGrob("log_recl指标在各Segment的性能表现", 
                 gp = gpar(fontsize = 18, fontface = "bold"))
)

# 保存组合图
combined_log_recl_plot <- arrangeGrob(
  log_recl_plots[["risk_class"]], log_recl_plots[["Land_Use_Types"]], log_recl_plots[["climate_zone"]],
  log_recl_plots[["reclamation_level"]], log_recl_plots[["reclamation_level_manual"]], log_recl_plots[["None"]],
  nrow = 2, ncol = 3,
  top = textGrob("log_recl指标在各Segment的性能表现", 
                 gp = gpar(fontsize = 18, fontface = "bold"))
)

ggsave("log_recl_segment_performance.png", combined_log_recl_plot, 
       width = 16, height = 12, dpi = 300, bg = "white")

#######  calculate_net_effect  ######

beta_total <- 0.1895
# 数据中代表“填海”的列名
head(city_df)
reclamation_col <- "log_recl"
city_df$z_reclamation <- as.vector(scale(city_df[[reclamation_col]]))

city_df$net_effect_score <- beta_total * city_df$z_reclamation
 world_map <- map_data("world")
longitude_col <- "Longitude" # <-- 如果您的经度列名叫 longitude 或 x，请修改这里
latitude_col <- "Latitude"   # <-- 如果您的纬度列名叫 latitude 或 y，请修改这里
  
table <- cbind(city_df$city, city_df$net_effect_score)
table
  net_effect_plot <- ggplot() +
    # 绘制世界地图背景层 (灰色填充，深灰色边框)
    geom_polygon(data = world_map, aes(x = long, y = lat, group = group), 
                 fill =  "#eeede7", color = "#5a5c6d", size = 0.2) +
    
    # 绘制城市数据点层 
    # geom_point() 用于画散点
    geom_point(data = city_df, 
               aes(x = .data[[longitude_col]], y = .data[[latitude_col]], fill = net_effect_score),
                  size = 3, 
           alpha = 0.8,
           shape = 21,          # 2. 使用 shape=21 (带边框的圆形)
           color = "black",     # 3. 将边框颜色(color)设置为 "黑色"
           stroke = 0.5) + # size调整点的大小, alpha调整透明度
    
    # --- 5. 设置颜色和图例 ---
    # scale_color_gradient2 是专门用于创建“冷-暖”发散色阶的函数
    scale_fill_gradient2(
      name = "Net Effect Score", # 图例标题
      low = "#191b99",              # 负值的颜色 (抑制效应)
      mid = "#d9ebe6",             # 中点 (0) 的颜色
      high = "#cf6262",              # 正值的颜色 (促进效应)
      midpoint = 0               # 指定颜色发散的中心点为0
    ) +
      # —— 坐标系 —— 
  coord_sf(crs = sf::st_crs(4326), xlim = c(-180, 180), ylim = c(-90, 90), expand = FALSE) +
    # --- 6. 美化主题和标签 ---
    labs(
      title = "Global Distribution of Net Effect of Coastal Reclamation on Invasion Risk",
      subtitle = "Red indicates net amplification; Blue indicates net suppression",
      x = "Longitude",
      y = "Latitude"
    ) +
    # 使用一个简洁的主题
    theme_minimal() +
    # 调整图例位置
   theme_minimal(base_family = "") +
  theme(
    panel.grid.major = element_blank(),  # 去除主网格线
    panel.grid.minor = element_blank(),  # 去除次网格线
    axis.text = element_text(size = 10, color = "black"),
    axis.title = element_blank(),        # 去除坐标轴标题
    panel.border = element_rect(color = "#000000", fill = NA, linewidth = 1),  # 黑色边框
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 14))

ggsave(
  "net_effect_map.png",     # 1. 文件名
  plot = net_effect_plot,   # 2. 要保存的绘图对象
  width = 12,               # 3. 宽度
  height = 7,               # 4. 高度
  units = "in",             # 5. 尺寸的单位 (英寸 "in", 厘米 "cm", 毫米 "mm")
  dpi = 300                 # 6. 分辨率 (dots per inch), 决定图片的清晰度
)

lab_lon <- function(x) paste0(abs(x), "°", ifelse(x >= 0, "E", "W"))
lab_lat <- function(y) paste0(abs(y), "°", ifelse(y >= 0, "N", "S"))

lon_breaks <- seq(105, 130, by = 15)   # 经线（你可以改成 10/15）
lat_breaks <- seq(15,  45,  by = 10)   # 纬线


p2 <- ggplot() +
   # 绘制世界地图背景层 (灰色填充，深灰色边框)
    geom_polygon(data = world_map, aes(x = long, y = lat, group = group), 
                 fill = "#eeede7", color = "#5a5c6d", size = 0.2) +
    
    # 绘制城市数据点层
    # geom_point() 用于画散点
    geom_point(data = city_df, 
               aes(x = .data[[longitude_col]], y = .data[[latitude_col]], fill = net_effect_score),
                  size = 12, 
           alpha = 0.8,
           shape = 21,          # 2. 使用 shape=21 (带边框的圆形)
           color = "black",     # 3. 将边框颜色(color)设置为 "黑色"
           stroke = 0.5) + # size调整点的大小, alpha调整透明度
    
    # --- 5. 设置颜色和图例 ---
    # scale_color_gradient2 是专门用于创建“冷-暖”发散色阶的函数
    scale_fill_gradient2(
      name = "Net Effect Score", # 图例标题
      low = "#191b99",              # 负值的颜色 (抑制效应)
      mid = "#d9ebe6",             # 中点 (0) 的颜色
      high = "#cf6262",              # 正值的颜色 (促进效应)
      midpoint = 0               # 指定颜色发散的中心点为0
    ) +


  # —— 坐标系 —— 
  coord_sf(crs = sf::st_crs(4326), xlim = c(105, 130), ylim = c(15, 45), expand = FALSE) +

  # —— 轴刻度显示成 °E/°N（修改）——
  scale_x_continuous(breaks = lon_breaks, labels = lab_lon, expand = c(0,0)) +
  scale_y_continuous(breaks = lat_breaks, labels = lab_lat, expand = c(0,0)) +


  theme_minimal(base_family = "") +
  theme(
    panel.grid.major = element_blank(),  # 去除主网格线
    panel.grid.minor = element_blank(),  # 去除次网格线
    axis.text = element_text(size = 10, color = "black"),
    axis.title = element_blank(),        # 去除坐标轴标题
    panel.border = element_rect(color = "#000000", fill = NA, linewidth = 1),  # 黑色边框
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 14)
  )
p2
ggsave(
  "net_effect_CNmap.png",     # 1. 文件名
  plot = p2,   # 2. 要保存的绘图对象
  width = 7,               # 3. 宽度
  height = 12,               # 4. 高度
  units = "in",             # 5. 尺寸的单位 (英寸 "in", 厘米 "cm", 毫米 "mm")
  dpi = 300                 # 6. 分辨率 (dots per inch), 决定图片的清晰度
)
