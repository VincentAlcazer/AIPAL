#' Global parameters

#  library(BiocManager)
# options(repos = BiocManager::repositories())

#' @import ggplot2
#' @import dplyr
#' @import tibble
#' @import ggrepel
#' @import tidyr
#' @import MetBrewer
#' @import rmarkdown
#' @import caret
#' @import xgboost
#' @import shinyalert
#' @importFrom uwot umap umap_transform


########## ========== Parameters
pal <- MetBrewer::met.brewer("Hiroshige", 3, direction = -1)


cutoffs <- data.frame(
  category = c("ALL","AML","APL"),
  PPV = c(0.9593636,0.9448341,0.7487954),
  NPV = c(0.04462764,0.02712490,0.04154142),
  ACC = c(0.4528432, 0.5001304, 0.3770665	)
)

########## ========== Data

### === Model & predictions

res_list <- readRDS("data/221003_Final_model_res_list.rds")

model <- res_list$final_model

overall_pred <- bind_rows(res_list$model_pred, .id = "data") %>%
  filter(!data %in% c("lyon","clermont"))

max_ALL = max(overall_pred$ALL)
max_AML = max(overall_pred$AML)
max_APL = max(overall_pred$APL)

# overall_pred <- overall_pred %>%
#   ### Corrected predictions
#   mutate(ALL_mod = (ALL - AML)*100/max_ALL,
#          AML_mod = (AML - ALL)*100/max_AML,
#          APL_mod = (APL - AML)*100/max_APL,
#          .keep = "unused")

#colnames(overall_pred) <- gsub("_mod","",colnames(overall_pred))

### === Params df
params_df <- readRDS("data/221003_params_df.rds") %>%
  mutate_if(is.numeric, function(x){round(x,2)})

### === Datasets

data_list <- readRDS("data/221003_AI_PAL_processed_data_list.rds")

### === Base PCA
training_data <- bind_rows(data_list) %>%
  # Subset variables from final model
  select(Subtype = Response, all_of(model$finalModel$xNames)) %>%
  # Input NA with median value to keep all samples for PCA
  mutate_all(function(x){ifelse(is.na(x),
                                median(x, na.rm = T),
                                x)})

### === Training data pCA
## Get components
pca <- prcomp(as.matrix(training_data[,-1]), scale=T, center = T)

## Base plot
pca_plot <- pca$x[,1:2] %>% data.frame(check.names = F) %>%
  ggplot(aes(x = PC1, y = PC2)) +
  geom_density_2d(aes(color = ..level..),size=1.5, bins = 25, alpha = 0.75) +
  MetBrewer::scale_color_met_c("Hiroshige", direction = -1) +
  labs(title = "Density plot - training data")

### === Training data UMAP
## Fit UMAP model on the training data for later projections
umap_training <- umap(as.matrix(training_data[,-1]), ret_model = TRUE)
umap_model <- umap_training$model

## Base UMAP plot of training data
umap_plot <- umap_training$embedding %>%
  data.frame(check.names = FALSE) %>%
  setNames(c("UMAP1", "UMAP2")) %>%
  ggplot(aes(x = UMAP1, y = UMAP2)) +
  geom_point(alpha = 0.3) +
  labs(title = "UMAP projection - training data")


########## ========== Parameters

sidebar_split <- c("80%","20%")
checkbox_align <- c("margin-top: 20px; margin-left: -15px")

### Ggplot 2 default theme

default_theme <- theme_bw() + theme(
  plot.title = element_text(size = 20, face = "bold"),
  axis.text = element_text(size = 14, color = "black"),
  axis.title = element_text(size = 16, face = "bold"),
  legend.title = element_text(size = 16, face = "bold"),
  legend.text = element_text(size = 14),
  strip.text.x = element_text(size = 16, face = "bold"),
  legend.position = "none"
)
