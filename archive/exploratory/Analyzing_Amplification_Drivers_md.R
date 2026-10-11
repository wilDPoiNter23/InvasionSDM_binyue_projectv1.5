# EXPLORATORY: extracted code only; no discussion/manuscript text included.
# Original note SHA256: 66708d1c9bbcc23a9887f912bacd704bb769d1229ac4070f9e2b49f008f381c1

# Fragment 1; not a complete analysis or runnable stage.
city_df$recl_frac <- city_df$reclamation_area / city_df$area_km2
# reclamation area as proportion of city admin area (0–1)


# Fragment 2; not a complete analysis or runnable stage.
library(gratia)
appraise(m_scale_A) & theme_minimal()


# Fragment 3; not a complete analysis or runnable stage.
install.packages("gratia")
library(gratia)
appraise(m_scale_A) & theme_minimal()


# Fragment 4; not a complete analysis or runnable stage.
library(ggeffects)


# Fragment 5; not a complete analysis or runnable stage.
pred_B <- ggpredict(
  m_scale_B,
  terms = c(
    "reclamation_area [all]",
    paste0("area_km2 [", paste(round(q_admin, 1), collapse = ", "), "]")
  )
)


# Fragment 6; not a complete analysis or runnable stage.
# 先确认是否安装
"ggeffects" %in% rownames(installed.packages())

# 如果 FALSE，先安装
install.packages("ggeffects")
library(ggeffects)


# Fragment 7; not a complete analysis or runnable stage.
# 方案一（推荐）：直接 log1p，处理右偏的小比例数据
city_df$log_recl_frac <- log1p(city_df$recl_frac)

# 方案二：logit 变换（严格的概率变量变换）
# city_df$logit_recl_frac <- log(city_df$recl_frac / (1 - city_df$recl_frac))
# 注意：如果有 recl_frac = 0，logit 会产生 -Inf，需要加一个小常数


# Fragment 8; not a complete analysis or runnable stage.
# 在数据准备部分统一改成：
city_df$log_recl      <- log1p(city_df$reclamation_area)   # ✅ 已有
city_df$log_city_area <- log1p(city_df$area_km2)           # ✅ 已有
city_df$log_recl_frac <- log1p(city_df$recl_frac)          # ❌ 缺这个！需要加
# risk 不变                                                  # ✅ 直接用原始值


# Fragment 9; not a complete analysis or runnable stage.
m_scale_A <- gam(
  risk ~
    s(log_recl,      k = 5) +    # log 绝对面积
    s(log_recl_frac, k = 5) +    # log 相对占比
    s(anthro_pc1, k = 5) + ...
)


# Fragment 10; not a complete analysis or runnable stage.
# 方案A：按中位数劈成两半（最干净的二分）
q_admin <- quantile(city_df$log_city_area, probs = c(0.25, 0.75), na.rm = TRUE)
# P25 = 代表小城市, P75 = 代表大城市
# labels = c("Small city (≤P25)", "Large city (≥P75)")

# 方案B：用你现有的P50/P90，但label改成"Medium"和"Large"
# 逻辑上没问题，只是不叫"二类城市"而是"中型 vs 大型"


# Fragment 11; not a complete analysis or runnable stage.
city_df$city_class <- ifelse(city_df$log_city_area > median(city_df$log_city_area),
                              "Large", "Small")
city_df$city_class <- factor(city_df$city_class)

m_scale_by <- gam(risk ~ s(log_recl, by = city_class, k = 4) +
                          s(log_recl_frac, k = 5) + ...,
                  data = city_df, method = "REML")
summary(m_scale_by)
# → summary 里会分别输出 s(log_recl):city_classLarge 和 s(log_recl):city_classSmall 的 p 值


# Fragment 12; not a complete analysis or runnable stage.
sm_rel <- smooth_estimates(m_scale_A, select = "s(log_recl_frac)") |> add_confint()
# 找曲线过零点（效应从负变正或从正变负的位置）
sm_rel[which.min(abs(sm_rel$.estimate)), "log_recl_frac"]


# Fragment 13; not a complete analysis or runnable stage.
library(spdep)

# ── 用 k 最近邻 (k=5) 代替距离断点，规避 sub-graph 问题
coords <- as.matrix(city_df[, c("Longitude", "Latitude")])
knn5   <- knearneigh(coords, k = 5)
nb5    <- knn2nb(knn5)
wts5   <- nb2listw(nb5, style = "W")

# Moran at single lag (knn = no 'lags', just global)
moran.test(residuals(m_scale_A), wts5)
moran.test(residuals(m3_direct),  wts5)


# Fragment 14; not a complete analysis or runnable stage.
moran_corr_safe <- function(resids, coords, n_lags = 8) {
  max_d  <- max(dist(coords)) * 0.6
  brk    <- seq(0, max_d, length.out = n_lags + 1)
  out    <- data.frame(lag_mid = (brk[-1] + brk[-(n_lags+1)]) / 2,
                       MoranI = NA_real_, p_value = NA_real_)
  n_res <- length(resids)             # ← 关键：记录残差长度
  for (i in seq_len(n_lags)) {
    nb <- dnearneigh(coords, brk[i], brk[i+1])
    # 只保留有邻居的城市
    has_nb <- which(card(nb) > 0)
    if (length(has_nb) < 3) next      # 太少则跳过
    wts <- nb2listw(nb, style = "W", zero.policy = TRUE)
    # zero.policy = TRUE 让孤立节点权重为 0，长度与原向量一致
    mt  <- moran.test(resids, wts, zero.policy = TRUE)
    out$MoranI[i]  <- mt$estimate["Moran I statistic"]
    out$p_value[i] <- mt$p.value
  }
  out
}

cor_A  <- moran_corr_safe(residuals(m_scale_A), coords)
cor_m3 <- moran_corr_safe(residuals(m3_direct),  coords)
cor_A$model  <- "m_scale_A (Step 1A)"
cor_m3$model <- "m3_direct (Step 2)"
cor_all <- rbind(cor_A, cor_m3)
print(cor_all, digits = 3)


# Fragment 15; not a complete analysis or runnable stage.
# 直接用分位数，不绕弯子
q_small <- quantile(..., 0.25)   # 确实来自 < P50 的那一半城市
q_large <- quantile(..., 0.75)   # 确实来自 ≥ P50 的那一半城市


# Fragment 16; not a complete analysis or runnable stage.
q_small <- median(...)   # 假设 = 6.5
q_large <- median(...)   # 也是 = 6.5  ← 完全一样的值！


# Fragment 17; not a complete analysis or runnable stage.
q_small <- quantile(city_df$log_city_area, 0.25, na.rm = TRUE)   # < P50 组的代表值
q_large <- quantile(city_df$log_city_area, 0.75, na.rm = TRUE)   # ≥ P50 组的代表值


# Fragment 18; not a complete analysis or runnable stage.
# ── 系统搜索：哪个 log_city_area 分位点让斜率从正变负 ──────────────────────
probs_to_test <- seq(0.10, 0.90, by = 0.10)
slope_results <- data.frame(quantile = probs_to_test, slope = NA_real_)

for (i in seq_along(probs_to_test)) {
  q_val <- quantile(city_df$log_city_area, probs_to_test[i], na.rm = TRUE)
  pred  <- ggpredict(m_scale_B,
                     terms = c("log_recl [all]",
                               paste0("log_city_area [", round(q_val, 3), "]")))
  df    <- as.data.frame(pred)
  # 斜率 = 最大 x 时的 predicted - 最小 x 时的 predicted
  slope_results$slope[i] <- df$predicted[which.max(df$x)] -
                             df$predicted[which.min(df$x)]
}

print(slope_results)
# 正斜率 = 更多围填海 → 更高入侵
# 负斜率 = 更多围填海 → 更低入侵
# 找到 slope 从正变负（或反向）的转折点


# Fragment 19; not a complete analysis or runnable stage.
q_small <- quantile(city_df$log_city_area, 0.10, na.rm = TRUE)
q_large <- quantile(city_df$log_city_area, 0.90, na.rm = TRUE)


# Fragment 20; not a complete analysis or runnable stage.
te(log_recl, area_km2, k = c(5, 8))  # 城市面积维度用更多节点


# Fragment 21; not a complete analysis or runnable stage.
p_B_cond


# Fragment 22; not a complete analysis or runnable stage.
q_small = P10 的 area_km2 ≈ 某个很小的城市面积具体值（如 300 km²）
q_large = P90 的 area_km2 ≈ 某个很大的城市面积具体值（如 3000 km²）


# Fragment 23; not a complete analysis or runnable stage.
m_null_area <- gam(risk ~ s(log_city_area, k=5) + s(anthro_pc1, k=5) +
                           s(climate_pc1, k=5) + s(soil_pc1, k=5) +
                           s(marine_pc1, k=5) + s(geom_pc1, k=5) +
                           s(Regions, bs="re"),
                   data = city_df, method = "REML")
AIC(m_null_area, m_scale_B)   # 如果 m_scale_B 的 AIC 低很多，说明围填海提供了独立信息


# Fragment 24; not a complete analysis or runnable stage.
q_small <- quantile(city_df$area_km2, 0.5, na.rm = TRUE)  # 中位数城市
q_large <- quantile(city_df$area_km2, 0.9, na.rm = TRUE)  # 大城市
# → 图标题应改为 "Medium vs Large city"，不叫 "Small vs Large"


# Fragment 25; not a complete analysis or runnable stage.
# ── 扫描所有分位点组合，找斜率符号翻转的那个点 ─────────────────────────────
probs_test <- seq(0.05, 0.95, by = 0.05)
slope_scan <- data.frame(quantile_prob = probs_test,
                         q_val         = NA_real_,
                         slope         = NA_real_)

for (i in seq_along(probs_test)) {
  q_val <- quantile(city_df$area_km2, probs_test[i], na.rm = TRUE)
  pred  <- ggpredict(m_scale_B,
                     terms = c("log_recl [all]",
                               paste0("area_km2 [", round(q_val, 1), "]")))
  df    <- as.data.frame(pred)
  slope_scan$q_val[i]  <- q_val
  slope_scan$slope[i]  <- df$predicted[which.max(df$x)] -
                          df$predicted[which.min(df$x)]
}

slope_scan$sign <- ifelse(slope_scan$slope > 0, "positive (+)", "negative (-)")
print(slope_scan[, c("quantile_prob", "q_val", "slope", "sign")], digits = 3)
cat("\n→ Sign flips between these consecutive rows:\n")
flip_idx <- which(diff(sign(slope_scan$slope)) != 0)
if (length(flip_idx) > 0) {
  for (idx in flip_idx)
    cat(sprintf("  P%.0f (%.0f km²) → P%.0f (%.0f km²)\n",
                probs_test[idx]*100,   slope_scan$q_val[idx],
                probs_test[idx+1]*100, slope_scan$q_val[idx+1]))
} else {
  cat("  No sign flip found — slopes all same direction\n")
}


# Fragment 26; not a complete analysis or runnable stage.
# 加上 area_km2 作为控制变量，检验 s(log_recl) 是否仍显著
m_control_area <- gam(
  risk ~ s(log_recl, k=5) + s(log_recl_frac, k=5) +
         s(area_km2, k=5) +                       # ← 控制城市面积
         s(anthro_pc1, k=5) + s(climate_pc1, k=5) +
         s(soil_pc1, k=5) + s(marine_pc1, k=5) +
         s(geom_pc1, k=5) + s(Regions, bs="re"),
  data = city_df, method = "REML"
)
summary(m_control_area)
# → 如果 s(log_recl) 仍然 p < 0.05，说明效应不是城市大小混淆的


# Fragment 27; not a complete analysis or runnable stage.
# 查看 m_scale_A 里 s(log_recl) 的 edf
# edf ≈ 1 → 线性效应（规模效应是恒定的）
# edf > 1 → 非线性（小规模和大规模围填海效应强度不同）
summary(m_scale_A)
# s(log_recl) 的 edf 是多少？


# Fragment 28; not a complete analysis or runnable stage.
# 固定 area_km2 = P50（中等城市），看 log_recl 从小到大的边际效应
library(gratia)
# 计算 te() 曲面在 area_km2 = P50 处的一维切片
p50_area <- median(city_df$area_km2, na.rm = TRUE)
ggpredict(m_scale_B,
          terms = c("log_recl [all]",
                    paste0("area_km2 [", round(p50_area, 0), "]")))
# → 曲线是直线 = 线性尺度效应
# → 曲线弯曲 = 非线性（有阈值/饱和点）


# Fragment 29; not a complete analysis or runnable stage.
# 只用相对比例项，看 AIC 是否更好
m_frac_only <- gam(
  risk ~ s(log_recl_frac, k = 5) +
         s(anthro_pc1, k=5) + s(climate_pc1, k=5) +
         s(soil_pc1, k=5) + s(marine_pc1, k=5) +
         s(geom_pc1, k=5) + s(Regions, bs="re"),
  data = city_df, method = "REML"
)
AIC(m_frac_only, m_scale_A, m_scale_B)


# Fragment 30; not a complete analysis or runnable stage.
s(recl_frac) + s(log_city_area) + controls   # 一个围填海项


# Fragment 31; not a complete analysis or runnable stage.
s(log_recl) + s(log_recl_frac) + s(area_km2) + controls   # 两个围填海项（增加参数惩罚）


# Fragment 32; not a complete analysis or runnable stage.
m_control_logarea <- gam(
  risk ~ s(log_recl, k=5) + s(log_recl_frac, k=5) +
         s(log_city_area, k=5) +          # ← 改成 log 变换
         s(anthro_pc1, k=5) + s(climate_pc1, k=5) +
         s(soil_pc1, k=5) + s(marine_pc1, k=5) +
         s(geom_pc1, k=5) + s(Regions, bs="re"),
  data = city_df, method = "REML"
)
AIC(m_control_logarea, m_control_area, m_scale_A)


# Fragment 33; not a complete analysis or runnable stage.
risk ~ s(recl_frac, k=5) + s(log_city_area, k=5) + controls


# Fragment 34; not a complete analysis or runnable stage.
# 就是这行（seg == "None" 时的公式）：
risk ~ s(recl_frac, k=5) +        # ← 原始比例（0-1），不是 log 变换！
       s(log_city_area, k=5) +    # ← log 变换的城市面积
       s(anthro_pc1, k=5) + s(climate_pc1, k=5) +
       s(soil_pc1, k=5) + s(marine_pc1, k=5) + s(geom_pc1, k=5) +
       s(Regions, bs="re")


# Fragment 35; not a complete analysis or runnable stage.
m_frac_raw <- gam(
  risk ~ s(recl_frac, k=5) +      # 原始比例，不取 log
         s(log_city_area, k=5) +  # log 城市面积（不是 area_km2）
         s(anthro_pc1, k=5) + s(climate_pc1, k=5) +
         s(soil_pc1, k=5) + s(marine_pc1, k=5) + s(geom_pc1, k=5) +
         s(Regions, bs="re"),
  data = city_df, method = "REML"
)
AIC(m_frac_raw)   # 应该接近 -210.88


# Fragment 36; not a complete analysis or runnable stage.
source("060503scale_dependentEffect.r")


# Fragment 37; not a complete analysis or runnable stage.
# 0605gam.r 第 671-687 行：
data <- data.frame(
  ...
  AIC = c(-263.12, -184.45, ..., -210.88),   # ← 手动填进去的数字
  ...
)


# Fragment 38; not a complete analysis or runnable stage.
library(lme4)
lmm_check <- lmer(
  risk ~ log_recl + log_recl_frac +           # 线性固定效应
         anthro_pc1 + climate_pc1 + soil_pc1 +
         marine_pc1 + geom_pc1 +
         (1 | Regions),                        # 区域随机截距
  data = city_df, REML = TRUE
)
AIC(lmm_check, m_scale_A)   # 如果 GAM 好很多 → 非线性有意义
summary(lmm_check)           # 看 log_recl 和 log_recl_frac 的固定效应方向是否一致


# Fragment 39; not a complete analysis or runnable stage.
city_df$log_recl_frac <- log1p(city_df$recl_frac)
#                        ^^^^^ = log(x + 1)


# Fragment 40; not a complete analysis or runnable stage.
# p_A_abs:
x = "Reclamation area (log₁₊-transformed)"

# p_A_rel:
x = "Reclamation fraction (log₁₊-transformed)"


# Fragment 41; not a complete analysis or runnable stage.
p_B_draw <- draw(m_scale_B, select = "te(log_recl,area_km2)", n = 80, contour = TRUE) +
  scale_fill_viridis_c(na.value = "white") +  # <--- 把 NA 的灰色变成纯白
  labs(...) + ...
