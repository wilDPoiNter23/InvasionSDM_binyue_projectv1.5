# City-level structural equation model
# Input: archived city composite table
# Output: standardized coefficients, path plot and effect summaries

source(file.path(Sys.getenv("INVASIONSDM_PROJECT_ROOT", "."), "config", "bootstrap.R"))
dir.create(publication_path("SEM_OUTPUT"), recursive=TRUE, showWarnings=FALSE)
library(piecewiseSEM)
dt1 <- read.csv(publication_path("SEM_INPUT"), check.names=FALSE)
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






library(piecewiseSEM)
library(dplyr)
library(tidyr)
library(igraph)
library(ggplot2)

# Extract coefficients and fix empty column names if present.
coefs_all <- coefs(dt1_psem, standardize = "scale")
if(any(names(coefs_all) == "")) names(coefs_all)[names(coefs_all) == ""] <- "Signif"

# Calculate effects, optionally excluding nonsignificant paths.
calc_effects_signif <- function(coefs_df,
                                response = "risk",
                                sig_threshold = 0.05,
                                method = c("pvalue","signif")) {
  method <- match.arg(method)

  # Use Std.Estimate rather than Estimate.
  edges <- coefs_df %>%
    select(Predictor, Response, Std.Estimate, P.Value, Signif) %>%
    rename(Estimate = Std.Estimate) %>%
    mutate(Estimate_orig = Estimate)

  # Keep significant paths only.
  if (method == "pvalue") {
    edges <- edges %>%
      mutate(Estimate_eff = ifelse(!is.na(P.Value) & P.Value < sig_threshold, Estimate, 0))
  } else { # method == "signif"
    edges <- edges %>%
      mutate(Estimate_eff = ifelse(!is.na(Signif) & Signif != "", Estimate, 0))
  }

  # Build the graph from significant edges only.
  edges_used <- edges %>% filter(Estimate_eff != 0)
  if(nrow(edges_used) == 0) {
    warning("No paths remained after significance filtering; check the threshold.")
  }
  g <- graph_from_data_frame(edges_used %>% select(Predictor, Response), directed = TRUE)

  vars <- unique(c(coefs_df$Predictor, coefs_df$Response))
  res_list <- list()

  for (v in vars) {
    if (v == response) next

    # Calculate direct effects from significant paths only.
    direct <- edges_used %>% filter(Predictor == v, Response == response) %>% pull(Estimate_eff)
    if (length(direct) == 0) direct <- 0

    # Calculate indirect effects along significant paths only.
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
  return(list(effects = res_df, edges = edges_used)) # Return significant edges only.
}





# Run with the default exclusion of paths with p>=0.05.
res_sig <- calc_effects_signif(coefs_all,
                               response = "risk",
                               sig_threshold = 0.05,

                               method = "pvalue")

effects_df <- res_sig$effects



# Reshape effects_df to long format for plotting.
effects_long <- effects_df %>%
  mutate(Variable = factor(Variable, levels = unique(Variable))) %>% # Retain the original order.
  pivot_longer(cols = c("Direct","Indirect","Total"),
               names_to = "EffectType", values_to = "Value")

# Draw the plot with the specified colors and styles.
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


















write.csv(coefs_all, publication_path("SEM_OUTPUT", "sem_coefficients.csv"), row.names=FALSE)
