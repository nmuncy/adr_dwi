# Load Libraries ---- 
library("ggplot2")
library("scatterplot3d")
library("psych")
library("devtools")
library("ggbiplot")
library("factoextra")

source(paste0(getwd(), "/resources/pull_data.R"))


# Get Data ---- 
df <- get_user_comp()


# Visualize ---- 

hist(df_comp_fu1$mem_verb, breaks = 20)
hist(df_comp_fu1$mem_vis, breaks = 20)
hist(df_comp_fu1$vis_mot, breaks = 20)
hist(df_comp_fu1$rx_time, breaks = 20)
hist(df_comp_fu1$imp_ctrl, breaks = 20)

#
ggplot(df_comp_fu1, aes(x=mem_verb, y=mem_vis)) +
  geom_point()

#
with(
  df_comp_fu1,
  {scatterplot3d(
    x=mem_verb,
    y=mem_vis,
    z=vis_mot,
  )}
 )

#
pairs.panels(
  df_comp_fu1[,4:8],
  gap = 0,
  pch=21
)


# PCA ---- 
pc <- prcomp(df_comp_fu1[,4:8], center=T, scale. = T)
pc$center
print(pc)
summary(pc)


#
# fviz_eig(pc, addlabels=T)
# fviz_pca_biplot(pc, label="var")

pairs.panels(
  pc$x,
  pch=21
)

g <- ggbiplot(
  pc,
  obs.scale = 1,
  var.scale = 1,
  ellipses=T,
  circle=T,
  ellipse.prob=0.68
)
g <- g + scale_color_discrete(name="")
g <- g + theme(legend.direction = "horizontal")
g
