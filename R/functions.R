
fast_pca <- function(data_acp, annot_vector, top_genes = 0.25, title = "",
                     scale = T, center = T, legend = "Group",
                     ellipse = "norm", gravity = T,
                     lgd.pos = "right"){

  ##  2. Top 25% variables regions
  if(is.null(top_genes)){
    data_filtered = data_acp
  } else {
    vars <- apply(data_acp, 2, var)
    top_var <- names(vars[order(vars, decreasing = T)][1:round(length(vars)*top_genes)])
    data_filtered <- data_acp[,top_var]
  }


  ##Run PCA
  pca <- prcomp(data_filtered, scale=scale, center = center)

  if(is.null(top_genes)){
    pca_title = paste0("PCA - ", title," (all features)")
  } else {
    pca_title = paste0("PCA - ", title," (top ",top_genes*100,"% variable features)")
  }

  ##### ===== Individuals
  factoextra::fviz_pca_ind(pca,
                           title = pca_title,
                           legend.title = legend,
                           mean.point = gravity, #pt du centre de gravitĂ©
                           addEllipses = TRUE,
                           ellipse.type = ellipse,
                           ellipse.level = 0.95,
                           pointsize = 3,
                           pointshape = 21,
                           col.ind = "black",
                           fill.ind = annot_vector, # Colorer par groupes
                           repel = T,
                           geom.ind=c("point")
  ) +
    # scale_shape_manual(values=c(15,15,15,16,16)) +
    # scale_color_manual(values=c("#06A4FF","#00BFC4", "#53B400","#E38900", "#F8766D")) +
    # scale_fill_manual(values=c("#06A4FF","#00BFC4", "#53B400","#E38900", "#F8766D")) +
    theme_bw() +
    theme(
      plot.title = element_text(size=18, face="bold"),
      axis.text = element_text(size=14, color="black"),
      axis.title = element_text(size=16,face="bold"),
      legend.title = element_text(size=16,face="bold"),
      legend.text = element_text(size = 14),
      legend.position = lgd.pos)





}
