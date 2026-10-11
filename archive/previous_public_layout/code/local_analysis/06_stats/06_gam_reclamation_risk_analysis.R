# ARCHIVE: historical reference; symbolic paths are intentionally not runnable.
# Original SHA256: 8236995c3cbc30982a61a79682e1233040d578e016d456f950cdb5e5a86ed017
& "${PRIVATE_EXTERNAL_PATH} Files\R\R-4.5.2\bin\R.exe" --vanilla
.libPaths("${PRIVATE_EXTERNAL_PATH}")
library(ggplot2)
library(jsonlite)
library(dplyr)
library(tidyr)
library(scales)
library(fmsb)
library(tibble)
library(stringr)
library(lme4)
library(lmerTest) # After loading, lmer() results automatically include p-values
library(performance)
library(mgcv)
library(gratia)
library(patchwork)
library(broom)
windows()
sessionInfo()

install.packages('performance')


setwd('${LOCAL_ANALYSIS_ROOT}/figure/gam_draft')
options(repos = c(CRAN = "https://cloud.r-project.org"))
list.files()
m_idx_rclass <- readRDS('gam_logrecl_riskclass')
# Example: Anthropogenic group
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
# 2. Model diagnostics
# -------------------------------------------------------------------------
# appraise() will generate 4 plots at once: QQ plot, Residuals vs Linear, etc.
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
    log_recl * risk_class +  # Core: interaction between log_recl and risk_class (fixed effects)
    log_city_area +
    anthro_pc1 + 
    climate_pc1 +
    soil_pc1 + 
    marine_pc1 + 
    geom_pc1 +
    (1 | Regions),           # Random intercept: different Regions have different baselines
  data = city_df,
  REML = TRUE               # Consistent with GAMM's method='REML'
)
summary(lmm_model_1)

lmm_model_2 <- lmer(
  risk ~ 
    log_recl * risk_class +  # Fixed effects interaction
    log_city_area +
    anthro_pc1 + 
    climate_pc1 +
    soil_pc1 + 
    marine_pc1 + 
    geom_pc1 +
    (1 | Regions),           # Random intercept
  data = city_df,
  REML = TRUE
)
summary(lmm_model_2)
# Compare AIC and BIC
model_compare <- compare_performance(m_idx_rclass, lmm_model_1, lmm_model_2, rank = TRUE)
print(model_compare)

# Create a concise table to compare core conclusions
# View significance of interaction terms in LMM
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
unique(sm_logrecl$risk_class) # Confirm specific factor levels of risk_class


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
  risk_class = factor(rep(c("1", "2"), each = 100)) # Assume original is "1", "2"
)

# Ensure risk_class is a factor with proper level order and labels
sm_logrecl <- sm_logrecl %>%
  mutate(risk_class = factor(risk_class, levels = c("1", "2"), labels = c("Low Risk", "High Risk")))

p1 <- ggplot(sm_logrecl,
             aes(x = log_recl, y = .estimate,
                 colour = risk_class, fill = risk_class)) +
  geom_ribbon(aes(ymin = .estimate - 2 * .se,
                  ymax = .estimate + 2 * .se),
              alpha = 0.15, # colour = NA might cause edge issues, remove or set to same as fill
              colour = NA,  # Try keeping NA, if issue resolved, could be other reasons
              linetype = 0 # Try setting confidence interval border line to transparent
              ) +
  geom_line(size = 1.1) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3) +
  labs(x = "Log-transformed Reclamation Area", # More specific X-axis label
       y = "Partial Effect on Risk", # Professional Y-axis label
       title = "Nonlinear Effect of Reclamation by Invasion Risk Class",
       subtitle = "GAM smooths (fs-basis); Ribbons = ±2 SE") + # Fix spelling
  theme_minimal(base_size = 14) + # Use theme_minimal and set base font size
  theme(legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5), # Title centered
        plot.subtitle = element_text(hjust = 0.5)) + # Subtitle centered
  scale_colour_manual(values = c("Low Risk" = "steelblue", "High Risk" = "firebrick")) + # Custom colors
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
# First, list all variables used in the model
model_vars <- c("log_recl,risk_class", "risk_class", "log_city_area", "anthro_pc1", 
                "climate_pc1", "soil_pc1", "marine_pc1", "geom_pc1", "Regions")
city_df$Regions
# Create typical value dataframe, containing only variables needed for the model
typ <- city_df %>%
  select(all_of(model_vars)) %>%
  summarize(across(where(is.numeric), mean, na.rm = TRUE),
            across(where(is.factor), ~ levels(.x)[which.max(table(.x))]))
typ <- city_df %>%
  select(all_of(model_vars))

typ$Regions <- as.factor(typ$Regions)
# Create grid data
grid2d <- expand_grid(
  log_recl = seq(min(city_df$log_recl, na.rm = TRUE),
                 max(city_df$log_recl, na.rm = TRUE), 
                 length.out = 80),
  anthro_pc1 = seq(min(city_df$anthro_pc1, na.rm = TRUE),
                   max(city_df$anthro_pc1, na.rm = TRUE),
                   length.out = 80)
) %>%
  # Add typical values for other variables
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

# Now try to predict
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

# Extract all smooth terms
sm_all <- smooth_estimates(m_idx_rclass)

# Select a few most important variables
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

  # Extract significance
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
  # Case A: If term contains log_recl -> Group by risk_class + Red/Blue
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
  # Case B: Other variables -> Map label to panel_colors
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
          legend.position.inside = c(0.05, 0.95),       # Legend placed at top left
      legend.justification = c("left", "top"), # Top left alignment
      legend.background = element_rect(fill = alpha("white", 0.6), colour = NA), # Semi-transparent background
      legend.key.size = unit(0.6, "lines"),    # Shrink legend
      legend.text = element_text(size = 8))     # Small font size)
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


# Extract smooth estimates
sm <- smooth_estimates(m_idx_rclass, select = lab, partial_match = TRUE)

# Extract partial residuals
res <- add_partial_residuals(model = m_idx_rclass, data =  model.frame(m_idx_rclass), smooth = lab)
sm <- sm %>%
  dplyr::rename(risk_class = .by) 
head(sm)
names(res)
ggplot() +
  # Residual scatter points
  geom_point(data = res,
             aes(x = log_recl,
                 y = `s(log_recl,risk_class)`,
                 colour = risk_class),
             alpha = 0.7, size = 5) +
  # Shaded band
  geom_ribbon(data = sm,
              aes(x = log_recl,
                  ymin = .estimate - 2*.se,
                  ymax = .estimate + 2*.se,
                  fill = risk_class),
              alpha = 0.2, colour = NA) +
  # Smooth curve
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
# Ensure data has longitude and latitude (Longitude, Latitude)
model_vars <- c("risk", "log_recl", "risk_class", "log_city_area",
                "anthro_pc1", "climate_pc1", "soil_pc1",
                "marine_pc1", "geom_pc1", "Regions",
                "Longitude", "Latitude")

# Create a new dataframe containing only these variables and remove any rows with NA
city_df_clean <- city_df %>%
  select(all_of(model_vars)) %>%  # Select only needed variables
  drop_na()                       # Remove any rows with NA

# Check the size of the new dataset
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
    # Add spatial smooth term: Gaussian process smooth, very suitable for geographic coordinates
    s(Longitude, Latitude, bs = "gp", k = 50), # k can be larger, gp smooth can effectively prevent overfitting
    
  data = city_df_clean, 
  method = "REML"
)

# Compare models: Is the spatial model better?
AIC(m_idx_rclass, m_spatial)

m_interaction <- bam(
  risk ~ 
    # Main effects
    s(log_recl, k=5) + 
    s(climate_pc1, k=5) +
    # Pure interaction effects (Note: must include the above main effects)
    ti(log_recl, climate_pc1, k=5) +
    
    s(log_city_area, k=5) +
    s(anthro_pc1, k=5) + 
    s(soil_pc1, k=5) + s(marine_pc1, k=5) + s(geom_pc1, k=5) +
    s(Regions, bs="re"),
  data = city_df_clean, 
  method = "REML"
)
# Visualization: Plot 2D contour or 3D surface plot
plot(m_interaction, scheme = 2, pages = 1) # scheme=2 plots contour map
vis.gam(m_interaction, view = c("log_recl", "climate_pc1"), plot.type = "persp") # 3D plot
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

# Create dataframe
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

# Normalize AIC
min_aic <- min(data$AIC)
max_aic <- max(data$AIC)
data$AIC_normalized <- 1 - ((data$AIC - min_aic) / (max_aic - min_aic))


# Choose three-color scheme commonly used in top journals
colors <- brewer.pal(3, "Set1")
names(colors) <- c("log_recl", "reclamation_area", "recl_frac")

# Prepare radar chart data
prepare_radar_data <- function(data) {
  # Convert to long format
  radar_long <- data %>%
    select(Reclamation_Variable, Segment, Adj_R2, Dev_Explained, AIC_normalized, Reclamation_Signif) %>%
    pivot_longer(cols = c(Adj_R2, Dev_Explained, AIC_normalized), 
                 names_to = "Metric", values_to = "Value")
  
  # Create angle coordinates for radar chart
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

# Create radar chart for single Segment
create_radar_plot <- function(segment_name, radar_data, colors) {
  segment_data <- radar_data %>% filter(Segment == segment_name)
  
  # Create scale circles
  circles <- data.frame(
    r = c(0.2, 0.4, 0.6, 0.8, 1.0)
  )
  
  # Create axis line data
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
  
  # Create axis labels
  axis_labels <- data.frame(
    Metric = metrics,
    Angle = (0:(length(metrics)-1)) * (2 * pi / length(metrics)),
    x = 1.15 * sin((0:(length(metrics)-1)) * (2 * pi / length(metrics))),
    y = 1.15 * cos((0:(length(metrics)-1)) * (2 * pi / length(metrics))),
    Label = c("Adj_R²", "Dev_Explained", "AIC_norm")
  )
  
  # Create scale labels
  scale_labels <- data.frame(
    r = c(0.2, 0.4, 0.6, 0.8, 1.0),
    x = 0,
    y = c(0.2, 0.4, 0.6, 0.8, 1.0)
  )
  
  ggplot() +
    # Draw scale circles
    geom_path(data = circles %>% 
                crossing(Angle = seq(0, 2*pi, length.out = 100)) %>%
                mutate(x = r * sin(Angle), y = r * cos(Angle)),
              aes(x = x, y = y, group = r), 
              color = "grey80", size = 0.3, linetype = "solid") +
    
    # Draw axis lines
    geom_path(data = axis_data %>% group_by(Metric),
              aes(x = x, y = y), 
              color = "grey60", size = 0.5) +
    
    # Add scale labels
    geom_text(data = scale_labels,
              aes(x = x + 0.05, y = y, label = r),
              size = 3, color = "grey50", hjust = 0) +
    
    # Draw data polygons
    geom_polygon(data = segment_data, 
                 aes(x = x, y = y, group = Reclamation_Variable, 
                     fill = Reclamation_Variable, color = Reclamation_Variable,
                     linetype = Reclamation_Signif),
                 alpha = 0.2, size = 1) +
    
    # Draw data points
    geom_point(data = segment_data, 
               aes(x = x, y = y, color = Reclamation_Variable,
                   shape = Reclamation_Signif),
               size = 3) +
    
    # Add axis labels
    geom_text(data = axis_labels, 
              aes(x = x, y = y, label = Label), 
              size = 4, color = "black", fontface = "bold") +
    
    # Set coordinate system
    coord_equal() +
    scale_x_continuous(limits = c(-1.3, 1.3), expand = c(0, 0)) +
    scale_y_continuous(limits = c(-1.3, 1.3), expand = c(0, 0)) +
    
    # Set colors and styles
    scale_color_manual(values = colors) +
    scale_fill_manual(values = colors) +
    scale_linetype_manual(values = c("TRUE" = "solid", "FALSE" = "dashed")) +
    scale_shape_manual(values = c("TRUE" = 16, "FALSE" = 4)) +
    
    # Theme settings
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold", margin = margin(b = 10)),
      legend.position = "right",
      legend.title = element_text(face = "bold"),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA)
    ) +
    
    # Labels and titles
    labs(
      title = segment_name,
      color = "Reclamation indicators",
      fill = "Reclamation indicators",
      linetype = "Significant",
      shape = "Significant"
    ) +
    
    # Legend guide
    guides(
      color = guide_legend(override.aes = list(size = 3, linetype = 0)),
      fill = "none",
      linetype = guide_legend(override.aes = list(color = "black", fill = NA)),
      shape = guide_legend(override.aes = list(color = "black"))
    )
}

# Prepare data
radar_data <- prepare_radar_data(data)

# Get all Segments
segments <- unique(radar_data$Segment)

# Create radar chart for each Segment
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

# Filter log_recl data
log_recl_data <- data %>%
  filter(Reclamation_Variable == "log_recl") %>%
  select(Segment, Adj_R2, Dev_Explained, AIC, Reclamation_Signif)

# View performance of log_recl in each Segment
print("Performance of log_recl in each Segment:")
print(log_recl_data)

# Prepare data specific to log_recl
log_recl_radar_data <- data %>%
  filter(Reclamation_Variable == "log_recl") %>%
  select(Segment, Adj_R2, Dev_Explained, AIC, Reclamation_Signif)

# Min-max normalize AIC
min_aic <- min(log_recl_radar_data$AIC)
max_aic <- max(log_recl_radar_data$AIC)
log_recl_radar_data$AIC_normalized <- 1 - ((log_recl_radar_data$AIC - min_aic) / (max_aic - min_aic))

# Create radar chart function specific to log_recl
create_log_recl_radar_plot <- function(segment_name, radar_data) {
  segment_data <- radar_data %>% filter(Segment == segment_name)
  
  # Prepare radar chart data
  metrics <- c("Adj_R2", "Dev_Explained", "AIC_normalized")
  values <- c(segment_data$Adj_R2, segment_data$Dev_Explained, segment_data$AIC_normalized)
  
  # Create dataframe
  plot_data <- data.frame(
    Metric = factor(metrics, levels = metrics),
    Value = values,
    Angle = (0:2) * (2 * pi / 3)
  ) %>%
    mutate(
      x = Value * sin(Angle),
      y = Value * cos(Angle)
    )
  
  # Create complete polygon data (closed)
  polygon_data <- rbind(plot_data, plot_data[1, ])
  
  # Create scale circles and axis lines
  circles <- data.frame(r = c(0.2, 0.4, 0.6, 0.8, 1.0))
  axis_labels <- data.frame(
    Metric = metrics,
    Label = c("Adj_R²", "Dev_Explained", "AIC_norm"),
    Angle = (0:2) * (2 * pi / 3),
    x = 1.15 * sin((0:2) * (2 * pi / 3)),
    y = 1.15 * cos((0:2) * (2 * pi / 3))
  )
  
  # Set colors (based on significance)
  line_color <- ifelse(segment_data$Reclamation_Signif, "#E41A1C", "#377EB8")  # Red=Significant, Blue=Not significant
  fill_color <- ifelse(segment_data$Reclamation_Signif, "#E41A1C", "#377EB8")
  
  ggplot() +
    # Draw scale circles
    geom_path(data = circles %>% 
                crossing(Angle = seq(0, 2*pi, length.out = 100)) %>%
                mutate(x = r * sin(Angle), y = r * cos(Angle)),
              aes(x = x, y = y, group = r), 
              color = "grey80", size = 0.3) +
    
    # Draw axis lines
    geom_segment(data = axis_labels,
                 aes(x = 0, y = 0, xend = x*0.95, yend = y*0.95),
                 color = "grey60", size = 0.5) +
    
    # Draw data polygons
    geom_polygon(data = polygon_data,
                 aes(x = x, y = y),
                 fill = alpha(fill_color, 0.2), color = line_color, 
                 size = 1.5, linetype = ifelse(segment_data$Reclamation_Signif, "solid", "dashed")) +
    
    # Draw data points
    geom_point(data = plot_data,
               aes(x = x, y = y),
               color = line_color, size = 4, shape = ifelse(segment_data$Reclamation_Signif, 16, 4)) +
    
    # Add axis labels
    geom_text(data = axis_labels,
              aes(x = x, y = y, label = Label),
              size = 5, color = "black", fontface = "bold") +
    
    # Add scale labels
    geom_text(data = data.frame(x = 0.05, y = c(0.2, 0.4, 0.6, 0.8, 1.0), label = c("0.2", "0.4", "0.6", "0.8", "1.0")),
              aes(x = x, y = y, label = label),
              size = 3, color = "grey50", hjust = 0) +
    
    # Add performance value labels
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
      subtitle = paste("log_recl metric |", ifelse(segment_data$Reclamation_Signif, "Statistically Significant", "Not Significant"))
    )
}

# Create radar chart for each Segment
segments <- unique(log_recl_radar_data$Segment)
log_recl_plots <- list()

for (segment in segments) {
  log_recl_plots[[segment]] <- create_log_recl_radar_plot(segment, log_recl_radar_data)
}

# Arrange and display all radar charts
grid.arrange(
  log_recl_plots[["risk_class"]], log_recl_plots[["Land_Use_Types"]], log_recl_plots[["climate_zone"]],
  log_recl_plots[["reclamation_level"]], log_recl_plots[["reclamation_level_manual"]], log_recl_plots[["None"]],
  nrow = 2, ncol = 3,
  top = textGrob("Performance of log_recl metric in each Segment", 
                 gp = gpar(fontsize = 18, fontface = "bold"))
)

# Save combined plot
combined_log_recl_plot <- arrangeGrob(
  log_recl_plots[["risk_class"]], log_recl_plots[["Land_Use_Types"]], log_recl_plots[["climate_zone"]],
  log_recl_plots[["reclamation_level"]], log_recl_plots[["reclamation_level_manual"]], log_recl_plots[["None"]],
  nrow = 2, ncol = 3,
  top = textGrob("Performance of log_recl metric in each Segment", 
                 gp = gpar(fontsize = 18, fontface = "bold"))
)

ggsave("log_recl_segment_performance.png", combined_log_recl_plot, 
       width = 16, height = 12, dpi = 300, bg = "white")

#######  calculate_net_effect  ######

beta_total <- 0.1895
# Column name representing "reclamation" in data
head(city_df)
reclamation_col <- "log_recl"
city_df$z_reclamation <- as.vector(scale(city_df[[reclamation_col]]))

city_df$net_effect_score <- beta_total * city_df$z_reclamation
 world_map <- map_data("world")
longitude_col <- "Longitude" # <-- If your longitude column is named longitude or x, change here
latitude_col <- "Latitude"   # <-- If your latitude column is named latitude or y, change here
  
table <- cbind(city_df$city, city_df$net_effect_score)
table
  net_effect_plot <- ggplot() +
    # Draw world map background layer (grey fill, dark grey border)
    geom_polygon(data = world_map, aes(x = long, y = lat, group = group), 
                 fill =  "#eeede7", color = "#5a5c6d", size = 0.2) +
    
    # Draw city data point layer
    # geom_point() used to draw scatter points
    geom_point(data = city_df, 
               aes(x = .data[[longitude_col]], y = .data[[latitude_col]], fill = net_effect_score),
                  size = 3, 
           alpha = 0.8,
           shape = 21,          # 2. Use shape=21 (circle with border)
           color = "black",     # 3. Set border color to "black"
           stroke = 0.5) + # size adjusts point size, alpha adjusts transparency
    
    # --- 5. Set colors and legend ---
    # scale_color_gradient2 is a function specifically for creating "cold-warm" divergent color scales
    scale_fill_gradient2(
      name = "Net Effect Score", # Legend title
      low = "#191b99",              # Negative value color (suppression effect)
      mid = "#d9ebe6",             # Midpoint (0) color
      high = "#cf6262",              # Positive value color (promotion effect)
      midpoint = 0               # Specify color divergence center point as 0
    ) +
      # -- Coordinate System -- 
  coord_sf(crs = sf::st_crs(4326), xlim = c(-180, 180), ylim = c(-90, 90), expand = FALSE) +
    # --- 6. Beautify theme and labels ---
    labs(
      title = "Global Distribution of Net Effect of Coastal Reclamation on Invasion Risk",
      subtitle = "Red indicates net amplification; Blue indicates net suppression",
      x = "Longitude",
      y = "Latitude"
    ) +
    # Use a concise theme
    theme_minimal() +
    # Adjust legend position
   theme_minimal(base_family = "") +
  theme(
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.text = element_text(size = 10, color = "black"),
    axis.title = element_blank(),        # Remove axis titles
    panel.border = element_rect(color = "#000000", fill = NA, linewidth = 1),  # Black border
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 14))

ggsave(
  "net_effect_map.png",     # 1. Filename
  plot = net_effect_plot,   # 2. Plot object to save
  width = 12,               # 3. Width
  height = 7,               # 4. Height
  units = "in",             # 5. Unit of size (inch "in", cm "cm", mm "mm")
  dpi = 300                 # 6. Resolution (dots per inch), determines image clarity
)

lab_lon <- function(x) paste0(abs(x), "°", ifelse(x >= 0, "E", "W"))
lab_lat <- function(y) paste0(abs(y), "°", ifelse(y >= 0, "N", "S"))

lon_breaks <- seq(105, 130, by = 15)   # Longitude lines (you can change to 10/15)
lat_breaks <- seq(15,  45,  by = 10)   # Latitude lines


p2 <- ggplot() +
   # Draw world map background layer (grey fill, dark grey border)
    geom_polygon(data = world_map, aes(x = long, y = lat, group = group), 
                 fill = "#eeede7", color = "#5a5c6d", size = 0.2) +
    
    # Draw city data point layer
    # geom_point() used to draw scatter points
    geom_point(data = city_df, 
               aes(x = .data[[longitude_col]], y = .data[[latitude_col]], fill = net_effect_score),
                  size = 12, 
           alpha = 0.8,
           shape = 21,          # 2. Use shape=21 (circle with border)
           color = "black",     # 3. Set border color to "black"
           stroke = 0.5) + # size adjusts point size, alpha adjusts transparency
    
    # --- 5. Set colors and legend ---
    # scale_color_gradient2 is a function specifically for creating "cold-warm" divergent color scales
    scale_fill_gradient2(
      name = "Net Effect Score", # Legend title
      low = "#191b99",              # Negative value color (suppression effect)
      mid = "#d9ebe6",             # Midpoint (0) color
      high = "#cf6262",              # Positive value color (promotion effect)
      midpoint = 0               # Specify color divergence center point as 0
    ) +


  # -- Coordinate System -- 
  coord_sf(crs = sf::st_crs(4326), xlim = c(105, 130), ylim = c(15, 45), expand = FALSE) +

  # -- Axis labels displayed as °E/°N (Modified) --
  scale_x_continuous(breaks = lon_breaks, labels = lab_lon, expand = c(0,0)) +
  scale_y_continuous(breaks = lat_breaks, labels = lab_lat, expand = c(0,0)) +


  theme_minimal(base_family = "") +
  theme(
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.text = element_text(size = 10, color = "black"),
    axis.title = element_blank(),        # Remove axis titles
    panel.border = element_rect(color = "#000000", fill = NA, linewidth = 1),  # Black border
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 14)
  )
p2
ggsave(
  "net_effect_CNmap.png",     # 1. Filename
  plot = p2,   # 2. Plot object to save
  width = 7,               # 3. Width
  height = 12,               # 4. Height
  units = "in",             # 5. Unit of size (inch "in", cm "cm", mm "mm")
  dpi = 300                 # 6. Resolution (dots per inch), determines image clarity
)
