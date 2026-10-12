# Original source: 初稿SEM1.0.R
# Original source location: <LOCAL_SCIENTIFIC_ROOT/data/sem_processandresult>/初稿SEM1.0.R
# Original source SHA-256: b042cf50b449e3f2f5aae6e26245507e5da31b478cd65e3db7ffa79426611e81
# Public-safe original snapshot: archive/original_sources/local/piecewise_sem_historical.R
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
library(piecewiseSEM)
library(glmmTMB)
dt1 <- read.csv(file.choose(),header = T)

head(dt1)
write.csv(dt1, "<LOCAL_ANALYSIS_ROOT>/figure/gam_draft/dt1_psem_preprocessed_scaled.csv")
# 按风险等级拆分数据
dt1<- subset(dt1, risk_class == 1)


dt1<- subset(dt1, risk_class == 2)


drop_bad_cols <- function(dat, cols, max_na = 0.5){
  keep <- cols[sapply(cols, function(v){
    if(!v %in% names(dat)) return(FALSE)
    na_rate <- mean(is.na(dat[[v]]))
    na_rate <= max_na && na_rate < 1
  })]
  keep
}

#构造复合变量Climate,拟合线性模型Climate_model
Climate_model <- lm(risk ~ wc2.1_2.5m_bio_1+
                      wc2.1_2.5m_bio_2+
                      wc2.1_2.5m_bio_3+
                      wc2.1_2.5m_bio_4+
                      wc2.1_2.5m_bio_5+
                      wc2.1_2.5m_bio_6+
                      
                      wc2.1_2.5m_bio_8+
                      wc2.1_2.5m_bio_9+
                      wc2.1_2.5m_bio_10+
                      wc2.1_2.5m_bio_11+
                      wc2.1_2.5m_bio_12+
                      wc2.1_2.5m_bio_13+
                      wc2.1_2.5m_bio_14+
                      wc2.1_2.5m_bio_15+
                      wc2.1_2.5m_bio_16+
                      wc2.1_2.5m_bio_17+
                      wc2.1_2.5m_bio_18+
                      wc2.1_2.5m_bio_19 , dt1)
#输出模型运算结果
summary(Climate_model)
coefs(Climate_model, standardize = 'scale')

#提取系数以生成因子分数，来预测目标变量
beta_wc2.1_2.5m_bio_1 <-  summary(Climate_model)$coefficients[2, 1]
beta_wc2.1_2.5m_bio_2 <-  summary(Climate_model)$coefficients[3, 1]
beta_wc2.1_2.5m_bio_3 <-  summary(Climate_model)$coefficients[4, 1]
beta_wc2.1_2.5m_bio_4 <-  summary(Climate_model)$coefficients[5, 1]
beta_wc2.1_2.5m_bio_5 <-  summary(Climate_model)$coefficients[6, 1]
beta_wc2.1_2.5m_bio_6 <-  summary(Climate_model)$coefficients[7, 1]

beta_wc2.1_2.5m_bio_8 <-  summary(Climate_model)$coefficients[8, 1]
beta_wc2.1_2.5m_bio_9 <-  summary(Climate_model)$coefficients[9, 1]
beta_wc2.1_2.5m_bio_10 <-  summary(Climate_model)$coefficients[10, 1]
beta_wc2.1_2.5m_bio_11 <-  summary(Climate_model)$coefficients[11, 1]
beta_wc2.1_2.5m_bio_12 <-  summary(Climate_model)$coefficients[12, 1]
beta_wc2.1_2.5m_bio_13 <-  summary(Climate_model)$coefficients[13, 1]
beta_wc2.1_2.5m_bio_14 <-  summary(Climate_model)$coefficients[14, 1]
beta_wc2.1_2.5m_bio_15 <-  summary(Climate_model)$coefficients[15, 1]
beta_wc2.1_2.5m_bio_16 <-  summary(Climate_model)$coefficients[16, 1]
beta_wc2.1_2.5m_bio_17 <-  summary(Climate_model)$coefficients[17, 1]
beta_wc2.1_2.5m_bio_18 <-  summary(Climate_model)$coefficients[18, 1]
beta_wc2.1_2.5m_bio_19 <-  summary(Climate_model)$coefficients[19, 1]



Climate<- beta_wc2.1_2.5m_bio_1*dt1$wc2.1_2.5m_bio_1+ 
  beta_wc2.1_2.5m_bio_2*dt1$wc2.1_2.5m_bio_2+
  beta_wc2.1_2.5m_bio_3*dt1$wc2.1_2.5m_bio_3+ 
  beta_wc2.1_2.5m_bio_4*dt1$wc2.1_2.5m_bio_4+
  beta_wc2.1_2.5m_bio_5*dt1$wc2.1_2.5m_bio_5+ 
  beta_wc2.1_2.5m_bio_6*dt1$wc2.1_2.5m_bio_6+
  
  beta_wc2.1_2.5m_bio_8*dt1$wc2.1_2.5m_bio_8+
  beta_wc2.1_2.5m_bio_9*dt1$wc2.1_2.5m_bio_9+ 
  beta_wc2.1_2.5m_bio_10*dt1$wc2.1_2.5m_bio_10+
  beta_wc2.1_2.5m_bio_11*dt1$wc2.1_2.5m_bio_11+ 
  beta_wc2.1_2.5m_bio_12*dt1$wc2.1_2.5m_bio_12+
  beta_wc2.1_2.5m_bio_13*dt1$wc2.1_2.5m_bio_13+ 
  beta_wc2.1_2.5m_bio_14*dt1$wc2.1_2.5m_bio_14+
  beta_wc2.1_2.5m_bio_15*dt1$wc2.1_2.5m_bio_15+ 
  beta_wc2.1_2.5m_bio_16*dt1$wc2.1_2.5m_bio_16+
  beta_wc2.1_2.5m_bio_17*dt1$wc2.1_2.5m_bio_17+ 
  beta_wc2.1_2.5m_bio_18*dt1$wc2.1_2.5m_bio_18+
  beta_wc2.1_2.5m_bio_19*dt1$wc2.1_2.5m_bio_19



dt1$Climate<-Climate
summary(lm(risk~ Climate, dt1))

#使用PecewiseSEM中的函数来获得标准化系数：Coefs
coefs(lm(risk~ Climate, dt1))










#构造复合变量Water,拟合线性模型Water_model
Water_model <- lm(risk ~ salinity_mean+
                    salinity_range+
                    temperature_range+
                    temperature_mean
                  , dt1)
#输出模型运算结果
summary(Water_model)
coefs(Water_model, standardize = 'scale')

#提取系数以生成因子分数，来预测目标变量
beta_salinity_mean <-  summary(Water_model)$coefficients[2, 1]
beta_salinity_range <-  summary(Water_model)$coefficients[3, 1]
beta_temperature_range <-  summary(Water_model)$coefficients[4, 1]
beta_temperature_mean <-  summary(Water_model)$coefficients[5, 1]
Water <- beta_salinity_mean*dt1$salinity_mean+ beta_salinity_range*dt1$salinity_range+
  beta_temperature_range*dt1$temperature_range+ beta_temperature_mean*dt1$temperature_mean
dt1$Water <- Water
summary(lm(risk~ Water, dt1))


#使用PecewiseSEM中的函数来获得标准化系数：Coefs
coefs(lm(risk~ Water, dt1))









#构造复合变量reclamation,拟合线性模型reclamation_model
reclamation_model <- lm(risk ~ reclamation_area+area_ratio
                        , dt1)
#输出模型运算结果
summary(reclamation_model)
coefs(reclamation_model, standardize = 'scale')

#提取系数以生成因子分数，来预测目标变量
beta_reclamation_area <-  summary(reclamation_model)$coefficients[2, 1]
beta_area_ratio <-  summary(reclamation_model)$coefficients[3, 1]
reclamation <- beta_reclamation_area*dt1$reclamation_area+ beta_area_ratio*dt1$area_ratio
dt1$reclamation <- reclamation
summary(lm(risk~ reclamation, dt1))


#使用PecewiseSEM中的函数来获得标准化系数：Coefs
coefs(lm(risk~ reclamation, dt1))






##继续构造其他复合变量,并拟合相应线性模型，不再多做赘述
#构造复合变量Soil,拟合线性模型Soil_model   
#9.24 又删掉了soc
Soil_model <- lm(risk ~ bdod_mean_2024_2_5min_aligned+
                   cfvo_mean_2024_2_5min_aligned+
                   
                   nitrogen_mean_2024_2_5min_aligned+
                   ocd_mean_2024_2_5min_aligned+
                   phh2o_mean_2024_2_5min_aligned+
                   sand_mean_2024_2_5min_aligned+
                   silt_mean_2024_2_5min_aligned
                 , dt1)
summary(Soil_model)
coefs(Soil_model, standardize = 'scale')
#提取系数以生成因子分数，来预测目标变量
beta_bdod_mean_2024_2_5min_aligned<-  summary(Soil_model)$coefficients[2, 1]
beta_cfvo_mean_2024_2_5min_aligned<-  summary(Soil_model)$coefficients[3, 1]

beta_nitrogen_mean_2024_2_5min_aligned<-  summary(Soil_model)$coefficients[4, 1]
beta_ocd_mean_2024_2_5min_aligned<-  summary(Soil_model)$coefficients[5, 1]
beta_phh2o_mean_2024_2_5min_aligned<-  summary(Soil_model)$coefficients[6, 1]
beta_sand_mean_2024_2_5min_aligned<-  summary(Soil_model)$coefficients[7, 1]
beta_silt_mean_2024_2_5min_aligned<-  summary(Soil_model)$coefficients[8, 1]


Soil<-beta_bdod_mean_2024_2_5min_aligned*dt1$bdod_mean_2024_2_5min_aligned + 
  beta_cfvo_mean_2024_2_5min_aligned*dt1$cfvo_mean_2024_2_5min_aligned+ 
  
  beta_nitrogen_mean_2024_2_5min_aligned*dt1$nitrogen_mean_2024_2_5min_aligned + 
  beta_ocd_mean_2024_2_5min_aligned*dt1$ocd_mean_2024_2_5min_aligned+ 
  beta_phh2o_mean_2024_2_5min_aligned*dt1$phh2o_mean_2024_2_5min_aligned +
  beta_sand_mean_2024_2_5min_aligned*dt1$sand_mean_2024_2_5min_aligned + 
  beta_silt_mean_2024_2_5min_aligned*dt1$silt_mean_2024_2_5min_aligned

dt1$Soil<- Soil
summary(lm(risk~ Soil, dt1))
#复合项显著预测AGB(P<0.01)
#使用PecewiseSEM中的函数来获得标准化系数：Coefs
coefs(lm(risk~ Soil, dt1))
















#构造复合变量socioeconomic,拟合线性模型climate_model
socioeconomic_model <- lm(risk ~ BUILT_S_E2025_2.5min+
                            GDP2020_2.5min+
                            VIIRS_2023_2.5min+
                            Pop2020_2.5min+
                            area_km2
                          , dt1)
summary(socioeconomic_model)
coefs(socioeconomic_model, standardize = 'scale')
#提取系数以生成因子分数，来预测目标变量
beta_BUILT_S_E2025_2.5min<-  summary(socioeconomic_model)$coefficients[2, 1]
beta_GDP2020_2.5min<-  summary(socioeconomic_model)$coefficients[3, 1]
beta_VIIRS_2023_2.5min<-  summary(socioeconomic_model)$coefficients[4, 1]
beta_Pop2020_2.5min<-  summary(socioeconomic_model)$coefficients[5, 1]
beta_area_km2<-  summary(socioeconomic_model)$coefficients[6, 1]
socioeconomic <- beta_BUILT_S_E2025_2.5min*dt1$BUILT_S_E2025_2.5min+ 
  beta_GDP2020_2.5min*dt1$GDP2020_2.5min + 
  beta_VIIRS_2023_2.5min*dt1$VIIRS_2023_2.5min + 
  beta_Pop2020_2.5min*dt1$Pop2020_2.5min + 
  beta_area_km2*dt1$area_km2 
dt1$socioeconomic <- socioeconomic
summary(lm(risk~ socioeconomic, dt1))
#复合项显著预测socioeconomic(P<0.01)
#使用PecewiseSEM中的函数来获得标准化系数：Coefs
coefs(lm(risk~ socioeconomic, dt1))



















#9.17   删掉water→risk
dt1_psem <- psem(
  lm(risk~reclamation+socioeconomic+Climate+Soil,data = dt1),
  lm(Water~reclamation+socioeconomic+Climate, data = dt1),
  lm(Soil~reclamation+socioeconomic+Climate+Water, data = dt1),
  lm(reclamation~Climate, data = dt1),
  lm(socioeconomic~reclamation+Climate, data = dt1)
)
summary(dt1_psem, .progressBar = FALSE)
p1 <- plot(dt1_psem)
p1






#----------重新绘制效应图  剔除不显著路径-----------------
library(piecewiseSEM)
library(dplyr)
library(tidyr)
library(igraph)
library(ggplot2)

# 1. 提取系数并修复空列名（如果有）
coefs_all <- coefs(dt1_psem, standardize = "scale")
if(any(names(coefs_all) == "")) names(coefs_all)[names(coefs_all) == ""] <- "Signif"

# 2. 计算效应的函数（可选剔除不显著路径）
calc_effects_signif <- function(coefs_df,
                                response = "risk",
                                sig_threshold = 0.05,
                                method = c("pvalue","signif")) {
  method <- match.arg(method)
  
  # 选择 Std.Estimate 而不是 Estimate
  edges <- coefs_df %>%
    select(Predictor, Response, Std.Estimate, P.Value, Signif) %>%
    rename(Estimate = Std.Estimate) %>%
    mutate(Estimate_orig = Estimate)
  
  # 显著性筛选：只保留显著路径
  if (method == "pvalue") {
    edges <- edges %>%
      mutate(Estimate_eff = ifelse(!is.na(P.Value) & P.Value < sig_threshold, Estimate, 0))
  } else { # method == "signif"
    edges <- edges %>%
      mutate(Estimate_eff = ifelse(!is.na(Signif) & Signif != "", Estimate, 0))
  }
  
  # 只保留显著边用于建图
  edges_used <- edges %>% filter(Estimate_eff != 0)
  if(nrow(edges_used) == 0) {
    warning("显著性筛选后没有保留任何边，请检查阈值")
  }
  g <- graph_from_data_frame(edges_used %>% select(Predictor, Response), directed = TRUE)
  
  vars <- unique(c(coefs_df$Predictor, coefs_df$Response))
  res_list <- list()
  
  for (v in vars) {
    if (v == response) next
    
    # 直接效应（只计显著路径）
    direct <- edges_used %>% filter(Predictor == v, Response == response) %>% pull(Estimate_eff)
    if (length(direct) == 0) direct <- 0
    
    # 间接效应（只走显著路径）
    indirect <- 0
    if (v %in% V(g)$name && response %in% V(g)$name) {
      paths <- all_simple_paths(g, from = v, to = response, mode = "out")
      if (length(paths) > 0) {
        for (p in paths) {
          p_nodes <- as_ids(p)
          if (length(p_nodes) <= 2) next
          prod_val <- 1
          valid_path <- TRUE
          for (i in seq_len(length(p_nodes) - 1)) {
            coef_i <- edges_used %>%
              filter(Predictor == p_nodes[i], Response == p_nodes[i+1]) %>%
              pull(Estimate_eff)
            if (length(coef_i) == 0 || is.na(coef_i) || coef_i == 0) { valid_path <- FALSE; break }
            prod_val <- prod_val * coef_i
          }
          if (valid_path) indirect <- indirect + prod_val
        }
      }
    }
    
    total <- direct + indirect
    res_list[[v]] <- data.frame(Variable = v,
                                Direct = direct,
                                Indirect = indirect,
                                Total = total,
                                stringsAsFactors = FALSE)
  }
  res_df <- bind_rows(res_list)
  return(list(effects = res_df, edges = edges_used)) # 只返回显著边
}





# 3. 运行（默认：剔除 p>=0.05 的路径）
res_sig <- calc_effects_signif(coefs_all,
                               response = "risk",
                               sig_threshold = 0.05,
                          
                               method = "pvalue")

effects_df <- res_sig$effects



# 4. 把 effects_df 转成长格式用于绘图
effects_long <- effects_df %>%
  mutate(Variable = factor(Variable, levels = unique(Variable))) %>% # 保持原顺序
  pivot_longer(cols = c("Direct","Indirect","Total"),
               names_to = "EffectType", values_to = "Value")

# 5. 绘图（颜色与样式按你要求）
effect_colors <- c("Direct" = "#ea7bbe",
                   "Indirect" = "#71d9fe",
                   "Total" = "#aeaade")

p <- ggplot(effects_long, aes(x = Variable, y = Value, fill = EffectType)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  scale_fill_manual(values = effect_colors, name = NULL) +
  theme_classic(base_size = 14) +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.7),
    axis.line = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_line(colour = "black"),
    axis.text.x = element_text(angle = 0, vjust = 0.5)
  ) +
  geom_hline(yintercept = 0, colour = "black", linewidth = 0.5) +
  ylab("Standardized effect on risk") +
  xlab("")

print(p)

library(gridExtra)
library(eoffice)
grid.arrange(pa, sa,ncol = 2)
topptx(filename="直接间接效应.pptx", width =13, height = 6)















