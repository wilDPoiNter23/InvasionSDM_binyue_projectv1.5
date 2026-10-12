# Original source: 060502indirect_re_caculated.r
# Original source location: <LOCAL_CODE_ROOT>/060502indirect_re_caculated.r
# Original source SHA-256: 9cf75a475470a5279e6341c03acbf24483a145359225b92e00458ff957711deb
# Public-safe original snapshot: archive/original_sources/local/060502indirect_re_caculated.r
# Classification: SUPPORTING
# Publication modifications:
# - machine-specific absolute path literals replaced by configured publication_path aliases
# - local encoding decoded to UTF-8; source bytes hashed above
# - comments and historical private machine paths sanitized
# - working-directory calls retained with configured paths; no analyses executed
# Scientific modification: NONE
# This is full historical source, not a certified unattended/reproducible run.
# Snapshot for inspection; path placeholders are not executable locations.
library(ggplot2)
library(dplyr)
library(ggrepel)
library(ggtext)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(biscale)
library(cowplot)
library(readxl)
install.packages("readxl")
install.packages('biscale')
detach('package:ggtext', unload = TRUE)
library(ggplot2, lib.loc = .libPaths()[1])  # 从主库加载
df <-  read.csv("<LOCAL_ANALYSIS_ROOT>/figure/gam_draft/gamtest_withpca.csv")
df <-read_excel("<LOCAL_ANALYSIS_ROOT>/data/organized_table/05_effectScore_Qudurat.xlsx")
dt1 <- read.csv("<LOCAL_ANALYSIS_ROOT>/figure/gam_draft/dt1_psem_preprocessed_scaled.csv")
head(df)
# --- 1) 用非标准化系数 (Estimate) ---
b_direct      <- -0.562
b_path_socio  <- 1.074 * 0.904  # = 0.971
b_path_soil   <- 0.674 * 0.287  # = 0.193
b_ind_total   <- b_path_socio + b_path_soil  # = 1.164
b_total       <- b_direct + b_ind_total      # = 0.602
# Risk ~ Socioeconomic + Climate + Soil 的系数（Estimate）
b_soc  <- 0.904
b_cli  <- 0.661
b_soil <- 0.287
# --- 2) 不做 PCA：直接用你 psem 前构造好的 composite（必须已存在于 df 中）---
# df 必须包含这些列：reclamation, socioeconomic, Climate, Soil, risk
# reclamation = beta_reclamation_area*reclamation_area + beta_area_ratio*area_ratio
# socioeconomic / Climate / Soil 也应是你用 risk 回归权重构造的复合变量

df_updated <- dt1 %>%
  mutate(
    # 这就是你 SEM 里真正的“填海机制变量”（不是 reclamation_area）
    X_recl = reclamation,

    # 这就是你结果变量（注意：如果 risk 在你模型里是 centered/relative risk，就用同一个）
    Y = risk,

    # --- reclamation 的 direct / indirect / total 贡献（都在 Y 的同一尺度上）---
    Direct_Effect   = b_direct     * X_recl,
    Ind_Socio       = b_path_socio * X_recl,
    Ind_Soil        = b_path_soil  * X_recl,
    Indirect_Effect = Ind_Socio + Ind_Soil,
    Total_Effect    = b_total      * X_recl,

    # --- 其他驱动：用 SEM risk 方程里的 composite（不要 PCA）---
    OtherDrivers = b_soc  * socioeconomic +
                   b_cli  * Climate +
                   b_soil * Soil,

    # ✅ 机制占比：围填海机制 vs 其他机制
    Dependency_mech = abs(Total_Effect) /
      (abs(Total_Effect) + abs(OtherDrivers)) * 100,

    # （可选）强度占比：围填海效应幅度 vs 结果幅度（不是机制占比）
    Dependency_intensity = abs(Total_Effect) /
      (abs(Total_Effect) + abs(Y)) * 100
  ) 
  df_updated 
  df_updated_2 <- df %>% mutate(

   
    # --- E. 象限（你原来的逻辑）---
    x_med = median(Pressure, na.rm = TRUE),
    y_thr = 50,
    Quadrant_Num = case_when(
      Pressure >= x_med & Dependency_mech  >= y_thr ~ 1,
      Pressure <  x_med & Dependency_mech >= y_thr ~ 2,
      Pressure <  x_med & Dependency_mech <  y_thr ~ 3,
      Pressure >= x_med & Dependency_mech  <  y_thr ~ 4
    ),
    Quadrant_Label = case_when(
      Quadrant_Num == 1 ~ "Q1: Amplified Hotspots",
      Quadrant_Num == 2 ~ "Q2: Latent Risk",
      Quadrant_Num == 3 ~ "Q3: Safe Coldspots",
      Quadrant_Num == 4 ~ "Q4: Natural Hotspots"
    ),
     Quadrant_intensity_Num = case_when(
      Pressure >= x_med & Dependency_intensity  >= y_thr ~ 1,
      Pressure <  x_med & Dependency_intensity >= y_thr ~ 2,
      Pressure <  x_med & Dependency_intensity <  y_thr ~ 3,
      Pressure >= x_med & Dependency_intensity  <  y_thr ~ 4
    ),
    Quadrant_intensity_Label = case_when(
      Quadrant_intensity_Num == 1 ~ "Q1: Amplified Hotspots",
      Quadrant_intensity_Num == 2 ~ "Q2: Latent Risk",
      Quadrant_intensity_Num == 3 ~ "Q3: Safe Coldspots",
      Quadrant_intensity_Num == 4 ~ "Q4: Natural Hotspots"
    )
    )
df_updated_2
# 4. 检查结果
df_updated_2$Quadrant_Label
head(df_updated %>% select(City, Pressure, Net, Dependency, Quadrant_Label))
write.csv(df_analysis, "<LOCAL_ANALYSIS_ROOT>/data/organized_table/0502_risk_city_netscoreStdVersion.csv", row.names = FALSE)
write.csv(df_updated_2,"<LOCAL_ANALYSIS_ROOT>/data/organized_table/0501_risk_city_netscorewithQ.csv")
df_updated <- read.csv("<LOCAL_ANALYSIS_ROOT>/data/organized_table/0502_risk_city_netscoreStdVersion.csv")
# 1. 准备数据：计算基于真实系数的效应 (包含 Soil)
plot_data_real <- df %>%
  mutate(
    # 归一化填海面积 (0 到 1)
    Recl_Rescaled = (reclamation_area - min(reclamation_area, na.rm=TRUE)) / 
                    (max(reclamation_area, na.rm=TRUE) - min(reclamation_area, na.rm=TRUE)),
    
    # --- A. 直接效应 (Cost) ---
    Effect_Direct = Recl_Rescaled * (-0.562),
    
    # --- B. 间接效应 (Benefit/Facilitation) ---
    # 路径 1: Socioeconomic
    Ind_Socio = Recl_Rescaled * (1.074 * 0.904), # = 0.971 * A
    # 路径 2: Soil (新增!)
    Ind_Soil  = Recl_Rescaled * (0.674 * 0.287), # = 0.193 * A
    
    # 总间接效应 (把所有正向的加起来)
    Effect_Indirect_Total = Ind_Socio + Ind_Soil
  )
plot_data_real
write.csv(plot_data_real, "<LOCAL_ANALYSIS_ROOT>/figure/gam_draft/netscore_updated.csv", row.names = FALSE)
# 2. 绘图 (X轴：Direct, Y轴：Total Indirect)
p_cost_benefit_v2 <- ggplot(plot_data_real, aes(x = Effect_Direct, y = Effect_Indirect_Total)) +
  
  # 辅助线
  geom_hline(yintercept = 0, color = "grey80") +
  geom_vline(xintercept = 0, color = "grey80") +
  # 1:1 对角线
  geom_abline(intercept = 0, slope = -1, linetype = "dashed", color = "grey60") +
  
  # 散点
  geom_point(aes(fill = risk, size = Recl_Rescaled), 
             shape = 21, color = "white", stroke = 0.4, alpha = 0.9) +
  
  # 标签 (只标重点)
  geom_text_repel(data = filter(plot_data_real, risk > quantile(risk, 0.95) | Recl_Rescaled > 0.5), 
                  aes(label = city), size = 3, fontface = "italic", box.padding = 0.5) +
  
  scale_fill_viridis_c(option = "magma", direction = -1, name = "Invasion Risk") +
  scale_size_continuous(range = c(2, 6), guide = "none") +
  
  # 标题与坐标 (更新描述)
  labs(
    title = "<b>Cumulative Cost-Benefit Trade-off of Reclamation</b>",
    subtitle = "Direct suppression vs. Cumulative indirect facilitation (Socioeconomic + Soil)",
    x = "Direct Ecological Effect (Habitat Suppression) [-]",
    y = "Total Indirect Effect (Facilitation) [+]"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_markdown(face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.grid = element_blank(),
    legend.position = c(0.85, 0.25)
  )

print(p_cost_benefit_v2)
windows()
ggsave("Cost_Benefit_Total_Indirect.pdf", width = 7, height = 7)


# 1. 准备底图
world <- ne_countries(scale = "medium", returnclass = "sf")

# 2. 准备数据 (计算net effect)
map_data_bi <- df %>%
  mutate(
    # 归一化填海强度
    Recl_Norm = (reclamation_area - min(reclamation_area, na.rm=TRUE)) / 
                (max(reclamation_area, na.rm=TRUE) - min(reclamation_area, na.rm=TRUE)),
    # 计算净效应 (使用你的真实系数)
    # 假设系数: Direct(-0.562) + Indirect(1.164) = 0.602
    Net_Effect = Recl_Norm * 0.602
  )
head(df_updated)
# ==============================================================================
# 2. 计算分位断点 (关键步骤：为了在图例上显示数值)
# ==============================================================================
# 我们使用 3x3 的 quantile 分类
# 计算 X轴 (Net Effect) 的 0%, 33%, 66%, 100% 分位数
x_breaks <- quantile(abs(df_updated$Total_Effect), probs = c(0, 1/4, 2/4,3/4, 1), na.rm = TRUE)
x_breaks
# 计算 Y轴 (Total Risk) 的 0%, 33%, 66%, 100% 分位数
y_breaks <- quantile(df_updated$Pressure, probs = c(0, 1/4, 2/4,3/4, 1), na.rm = TRUE)
y_breaks
x_breaks
# 格式化数值 (保留2-3位小数)
x_labs <- sprintf("%.3f", x_breaks)
y_labs <- sprintf("%.3f", y_breaks)
x_labs
y_labs
# 创建双变量分类列
df_updated <- df_updated %>%
  mutate(Total_Effect_abs = abs(Total_Effect))
data_bi <- bi_class(df_updated, x = Total_Effect_abs , y = Pressure, style = "quantile", dim = 4)
data_bi
# ==============================================================================

map <- ggplot() +
  geom_sf(data = world, fill =  "#eeede7", color = "#727583", size = 0.1) +
   geom_point(data = data_bi, 
             aes(x = Longitude, y = Latitude, fill = bi_class), # 1. 这里改成 fill
             shape = 21,       # 2. 关键：使用支持边框的形状 21
             color = " grey30",  # 3. 这里设置边框颜色 (推荐白色或 grey30)
             stroke = 0.4,     # 4. 这里设置边框粗细 (0.3 - 0.5 比较精致)
             size = 4,       # 点稍微大一点，方便看清颜色
             alpha = 0.7) +
  
  # 【关键配套修改】因为上面用了 fill，这里必须用 bi_scale_fill
  bi_scale_fill("DkBlue2", dim = 4) +
  coord_sf(crs = sf::st_crs(4326), xlim = c(-180, 180), ylim = c(-90, 90), expand = FALSE) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_markdown(face = "bold"),
    plot.subtitle = element_markdown(size = 10, color = "grey30"),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    panel.background = element_rect(fill = '#cde0f4', color = NA) # 淡蓝色海洋背景
  )


# ==============================================================================
# 4. 【核心】手绘带数值刻度的图例
# ==============================================================================
legend <- bi_legend(pal = "DkBlue2", dim = 4, xlab = "Reclamation-Induced Pressure", ylab = "Invasion Pressure Index")
# 5. 组合
final_plot <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  # 调整图例的位置 (x, y, width, height)
  # 放在左下角海洋处
  draw_plot(legend, 0.05, 0.1, 0.25, 0.25) 

print(final_plot)
ggsave( "<LOCAL_ANALYSIS_ROOT>/figure/gam_draft/plot5_netscore_bicovirate.png", plot = final_plot, width = 10, height = 12, bg = "transparent", dpi = 1200, ,device = grDevices::png)


map_CN <- ggplot() +
  geom_sf(data = world, fill =  "#eeede7", color = "#727583", size = 0.1) +
   geom_point(data = data_bi, 
             aes(x = Longitude, y = Latitude, fill = bi_class), # 1. 这里改成 fill
             shape = 21,       # 2. 关键：使用支持边框的形状 21
             color = " grey50",  # 3. 这里设置边框颜色 (推荐白色或 grey30)
             stroke = 0.55,     # 4. 这里设置边框粗细 (0.3 - 0.5 比较精致)
             size = 15,       # 点稍微大一点，方便看清颜色
             alpha = 0.7) +
  
  # 【关键配套修改】因为上面用了 fill，这里必须用 bi_scale_fill
  bi_scale_fill("DkBlue2", dim = 4) +
  coord_sf(crs = sf::st_crs(4326), xlim = c(105, 130), ylim = c(15, 45), expand = FALSE)  +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_markdown(face = "bold"),
    plot.subtitle = element_markdown(size = 10, color = "grey30"),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    panel.background = element_rect(fill = '#cde0f4', color = NA) # 淡蓝色海洋背景
  )
  map_CN
ggsave( "<LOCAL_ANALYSIS_ROOT>/figure/gam_draft/CN_plot5_netscore_bicovirate.png", plot = map_CN, width = 10, height = 12, bg = "transparent", dpi = 1200, device = grDevices::png)
