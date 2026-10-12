# Original source: 0205tidysdm.R
# Original source location: <LOCAL_CODE_ROOT>/0205tidysdm.R
# Original source SHA-256: f38a36a4a4474dcfb10c1def09239f2a455e8160961c7ad0866059d9ae91f6e3
# Public-safe original snapshot: archive/original_sources/local/0205tidysdm.R
# Classification: SUPPORTING
# Publication modifications:
# - machine-specific absolute path literals replaced by configured publication_path aliases
# - local encoding decoded to UTF-8; source bytes hashed above
# - comments and historical private machine paths sanitized
# - working-directory calls retained with configured paths; no analyses executed
# Scientific modification: NONE
# This is full historical source, not a certified unattended/reproducible run.
if (!exists("publication_path", mode="function")) stop("Load config/bootstrap.R first")
# Load packages
library(raster)
library(rgdal)
library(biomod2)
library(DBI)
library(duckdb)
library(dplyr)
library(tidysdm)
library(sf)
conflicts()
# detach pakage
detach("package:ggplot2", unload = TRUE, character.only = TRUE)
unloadNamespace("raster")
unloadNamespace("terra")
# connect to the db database
con <- dbConnect(duckdb::duckdb(), dbdir = publication_path("LOCAL_DATABASE_ROOT", "speciesocc_data.duckdb"))
con <- dbConnect(duckdb::duckdb(), dbdir = publication_path("HPC_INPUT_ROOT", "script/speciesocc_data.duckdb"), read_only = FALSE)
tables <- dbListTables(con)
table_structure <- dbGetQuery(con, "PRAGMA table_info('speciesocc_data')")
print(tables)

species_list <- dbGetQuery(con, "SELECT DISTINCT species FROM speciesocc_data;")$species
species_list
species_name <- "Casuarina glauca"
query <- "SELECT * FROM speciesocc_data WHERE species = ?;"
result <- dbGetQuery(con, query, params = list(species_name))
names(result)
dbDisconnect(con, shutdown = TRUE)

existing_conns <- dbListConnections(duckdb::duckdb())
# 查看数据结构
glimpse(result)

# 加载环境变量栅格数据
env_layers <- rast(list.files(publication_path("LOCAL_ANALYSIS_ROOT", "data/data/climate/wc2.1_2.5m/"), 
                              pattern = ".tif$", full.names = TRUE))
# 创建 sdm_dataset 对象，将物种数据和环境变量结合
sdm_data <- sdm_dataset(species = species_data, 
                        predictors = env_layers, 
                        response = "presence", # 假设存在 presence 列
                        coords = c("longitude", "latitude"))
# 检查缺失值并移除
sdm_data <- sdm_data %>%
  drop_na()

# 查看环境变量的相关性，移除高度相关的变量
cor_matrix <- sdm_data %>%
  select(-presence) %>%
  cor()

# 可视化相关性矩阵（可选）
library(corrplot)
corrplot(cor_matrix, method = "circle")

# 根据相关性结果，选择合适的环境变量
sdm_data <- sdm_data %>%
  select(-highly_correlated_variable)
# 使用 rsample 包进行数据集划分
set.seed(123)
data_split <- initial_split(sdm_data, prop = 0.8, strata = presence)

train_data <- training(data_split)
test_data <- testing(data_split)
# 定义一个随机森林模型规范
rf_spec <- rand_forest(mtry = tune(), trees = 500, min_n = tune()) %>%
  set_engine("ranger") %>%
  set_mode("classification")

# 创建一个参数调优网格
rf_grid <- grid_regular(mtry(range = c(2, 10)), min_n(range = c(5, 15)), levels = 5)

# 定义交叉验证方法
cv_folds <- vfold_cv(train_data, v = 5, strata = presence)

# 创建工作流程
rf_workflow <- workflow() %>%
  add_model(rf_spec) %>%
  add_formula(presence ~ .)

# 进行参数调优
library(tune)
set.seed(123)
rf_tuned <- rf_workflow %>%
  tune_grid(resamples = cv_folds, grid = rf_grid, metrics = metric_set(roc_auc))

# 查看最佳参数
best_params <- rf_tuned %>%
  select_best(metric = "roc_auc")

# 使用最佳参数更新模型规范
rf_final_spec <- finalize_model(rf_spec, best_params)

# 训练最终模型
rf_final_workflow <- rf_workflow %>%
  update_model(rf_final_spec)

rf_fit <- rf_final_workflow %>%
  fit(data = train_data)
# 对测试集进行预测
test_predictions <- rf_fit %>%
  predict(test_data) %>%
  bind_cols(test_data)

# 计算评估指标
library(yardstick)
metrics <- test_predictions %>%
  roc_auc(truth = presence, .pred_class)

print(metrics)

# 在整个研究区域进行预测
prediction_map <- predict(rf_fit, new_data = env_layers, type = "prob")

# 将预测结果转换为栅格
prediction_raster <- rast(prediction_map)

# 可视化预测结果
plot(prediction_raster, main = "物种分布预测概率")

# 可视化 ROC 曲线
roc_curve <- test_predictions %>%
  roc_curve(truth = presence, .pred_1)

autoplot(roc_curve)

