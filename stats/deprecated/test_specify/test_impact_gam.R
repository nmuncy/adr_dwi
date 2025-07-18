# PCA: Post Impact ----
library("factoextra")
library("ggbiplot")

df_post <- df_scan_imp[which(df_scan_imp$scan_name == "post"), ]
df_post <- df_post[complete.cases(df_post[, 7:12]), ] # 7:10

# hist(df_post$mem_ver, breaks = 20)
# hist(df_post$mem_vis, breaks = 20)
# hist(df_post$vis_mot, breaks = 20)
# hist(df_post$rx_time, breaks = 20)
# hist(df_post$imp_ctl, breaks = 20)
# hist(df_post$tot_symp, breaks = 20)

pc_post <- prcomp(df_post[, 7:12], center = T, scale. = T) # 7:10
pc_post$center
pc_post$scale
print(pc_post)
summary(pc_post)

fviz_eig(pc_post, addlabels = T)
fviz_pca_biplot(pc_post, label = "var")

# ellipse.prob=0.68
# ellipse=T,
ggbiplot(
  pc_post,
  obs.scale = 1,
  var.scale = 1,
  circle = T
) +
  scale_color_discrete(name = "") +
  theme(legend.direction = "horizontal")

# # Invert rx_time
# df_post$rx_time_inv <-
#   (max(df_post$rx_time) - df_post$rx_time) + min(df_post$rx_time)
# pc_post <- prcomp(df_post[,c(7:9,15)], center=T, scale. = T)
# pc_post$center
# print(pc_post)
# summary(pc_post) # no impact of inversion
# fviz_eig(pc_post, addlabels=T)
# fviz_pca_biplot(pc_post, label="var", repel=T)


# K-means: Post Impact ----
#
# 3 centers: ss = 62.8%
# 4 centers: ss = 71.3%
library("psych")
library("ggplot2")
library("ggpubr")

df_post <- df_scan_imp[which(df_scan_imp$scan_name == "post"), ]
df_post <- df_post[complete.cases(df_post[, 7:10]), ] # 7:12

# Normalize and check distance
data_norm <- scale(df_post[, 7:10]) # 7:12
data_dist <- dist(data_norm)
fviz_nbclust(data_norm, kmeans, method = "wss")

# K-means, determine cluster membership
km_post <- kmeans(data_norm, centers = 3, nstart = 100) # 5
print(km_post)

km_clust <- km_post$cluster
rownames(data_norm) <- df_post$subj_id
fviz_cluster(list(data = data_norm, cluster = km_clust))
table(km_clust)

# Plot
km_clust <- as.data.frame(km_clust)
rownames(km_clust) <- df_post$subj_id
km_clust <- cbind(subj_id = rownames(km_clust), km_clust)
rownames(km_clust) <- NULL

# Organize group labels for consistency, by know members of groups
grp_c_lab <- km_clust[which(km_clust$subj_id == 216), ]$km_clust
grp_b_lab <- km_clust[which(km_clust$subj_id == 110), ]$km_clust
grp_a_lab <- km_clust[which(km_clust$subj_id == 267), ]$km_clust

km_clust$km_grp <- 1
km_clust[which(km_clust$km_clust == grp_b_lab), ]$km_grp <- 2
km_clust[which(km_clust$km_clust == grp_c_lab), ]$km_grp <- 3


df_post_grp <- merge(
  x = df_post,
  y = km_clust,
  by = "subj_id",
  all = T
)

plot(df_post_grp$mem_ver, col = factor(df_post_grp$km_grp))
plot(df_post_grp$mem_vis, col = factor(df_post_grp$km_grp))
plot(df_post_grp$vis_mot, col = factor(df_post_grp$km_grp))
plot(df_post_grp$rx_time, col = factor(df_post_grp$km_grp))
plot(df_post_grp$tot_symp, col = factor(df_post_grp$km_grp))
plot(df_post_grp$imp_ctl, col = factor(df_post_grp$km_grp))

pairs.panels(
  df_post_grp[, 7:10], # 7:12
  bg = c("blue", "red", "green")[df_post_grp$km_grp],
  # bg=c("blue", "red", "green", "orange", "purple")[df_post_grp$km_grp],
  gap = 0,
  pch = 21
)

# Check Simpson's Paradox
ggplot(
  data = df_post_grp,
  aes(x = vis_mot, y = mem_vis, color = factor(km_grp))
) +
  geom_point() +
  stat_smooth(method = lm, se = F) +
  scale_color_manual(
    breaks = c("1", "2", "3"), # , "4", "5"
    values = c("blue", "red", "green") # , "orange", "purple"
  ) +
  stat_cor(method = "pearson")

# Add global line
ggplot(
  data = df_post_grp,
  aes(x = vis_mot, y = mem_vis)
) +
  geom_point(aes(color = factor(km_grp))) +
  stat_smooth(method = lm, aes(color = factor(km_grp)), se = F) +
  scale_color_manual(
    breaks = c("1", "2", "3", "4", "5"),
    values = c("blue", "red", "green", "orange", "purple")
  ) +
  stat_cor(method = "pearson", aes(color = factor(km_grp))) +
  stat_cor(method = "pearson", label.x = 40, label.y = 38) +
  geom_smooth(method = lm, color = "black", se = F)


# RF: Base vs Post ----
#
# OOB err rate with 6 param = 39.25%
#
library("randomForest")


df <- df_scan_imp[
  which(df_scan_imp$scan_name != "rtp"),
  c(
    "subj_id", "scan_name", "mem_ver", "mem_vis",
    "vis_mot", "rx_time", "imp_ctl", "tot_symp"
  )
]
df$scan_name <- as.character(df$scan_name)
rf <- randomForest(
  factor(scan_name) ~ mem_ver +
    mem_vis + vis_mot + rx_time + imp_ctl + tot_symp,
  data = df,
  ntree = 700,
  mtry = 2,
  importance = T
)
rf
importance(rf)

plot(rf)
varImpPlot(rf, sort = F, main = "base vs post RF importance")


# MERF: Base vs Post vs RTP ----
library("LongituRF")

df <- df_scan_imp[
  ,
  c(
    "subj_id", "scan_name", "mem_ver", "mem_vis",
    "vis_mot", "rx_time", "imp_ctl", "tot_symp"
  )
]
df$time <- 1
df[which(df$scan_name == "post"), ]$time <- 2
df[which(df$scan_name == "rtp"), ]$time <- 3
df$z <- 1 # TODO specify

merf <- MERF(
  x = df[, 3:8],
  y = df$scan_name,
  z = df$z,
  id = df$subj_id,
  time = df$time,
  mtry = 2,
  ntree = 500,
  sto = "BM"
)


# GAM: Impact values ----
#
# Model as HGAM, using only group smooths and wiggliness, i.e.
# no global and similar to mod_I.
library(mgcv)
library(mgcViz)
library(viridis)
library(rgl)
library(visreg)
library("reshape2")

# Get relevant columns
df_wide <- subset(
  df_scan_imp,
  select = c(
    "subj_id", "scan_name", "mem_ver", "mem_vis",
    "vis_mot", "rx_time", "imp_ctl", "tot_symp"
  )
)

# Convert to long
df <- melt(
  df_wide,
  id.vars = c("subj_id", "scan_name"),
  variable.name = "impact",
  value.name = "value"
)
df$impact <- factor(df$impact)

# Make continuous x
df$scan_time <- 1
df[which(df$scan_name == "post"), ]$scan_time <- 2
df[which(df$scan_name == "rtp"), ]$scan_time <- 3

# Specify HGAM with only group smooths and wiggliness, not ideal because
# impact values have different distributions
fit_I <- bam(
  value ~ s(subj_id, bs = "re") +
    s(scan_time, by = impact, k = 3, bs = "tp") +
    s(impact, bs = "re", k = 6),
  data = df,
  family = nb(),
  method = "fREML",
  discrete = T
)
gam.check(fit_I)
plot(fit_I)
visreg(fit_I, xvar = "scan_time", by = "impact", data = df, method = "fREML")

# Deal with diff distributions
impact_list <- unique(df$impact)
for(imp in impact_list){
  hist(df[which(df$impact == imp), ]$value, main = imp)
}
# right skewed: imp_ctrl, rx_time, tot_symp 
# left skewed: mem_ver, mem_vis, vis_mot

ggplot(data = df, aes(x = scan_time, y = value)) +
  facet_wrap(vars(impact), ncol = 3, scales = "free") +
  # geom_smooth(method = "gam", formula = y ~ s(x, bs = "tp", k = 3)) +
  geom_smooth() +
  # geom_point() +
  scale_x_continuous(breaks = 1:3)


# GAM: WB Post-Base, RTP-Base differences ----
#
# https://stackoverflow.com/questions/68956080/how-to-specify-a-hierarchical-gam-hgam-model-with-two-categorical-a-continuo
# https://stackoverflow.com/questions/47934100/how-to-specify-the-non-linear-interaction-of-two-factor-variables-in-generalised?rq=3
# https://stackoverflow.com/questions/63023080/interactions-between-categorical-terms-in-gam-mgcv?rq=3

# Convert wide for delta calc
df <- subset(
  df_afq,
  select = c("subj_id", "scan_name", "tract_name", "node_id", "dti_fa")
)
df_wide <- reshape(
  df,
  idvar = c("subj_id", "node_id", "tract_name"), timevar = "scan_name",
  direction = "wide"
)
df_wide$delta.post_base <- df_wide$dti_fa.post - df_wide$dti_fa.base
df_wide$delta.rtp_base <- df_wide$dti_fa.rtp - df_wide$dti_fa.base
# hist(df_wide$delta.rtp_base)
# hist(df_wide$delta.post_base)

df_wide <- subset(
  df_wide,
  select = c(
    "subj_id", "tract_name", "node_id", "delta.post_base", "delta.rtp_base"
  )
)
df_long <- reshape(
  df_wide,
  direction = "long",
  varying = c("delta.post_base", "delta.rtp_base"),
  times = c("post_base", "rtp_base"),
  idvar = c("subj_id", "tract_name", "node_id")
)
colnames(df_long)[4] <- c("comp_scan")
rownames(df_long) <- NULL
df_long$comp_scan <- factor(df_long$comp_scan)
rm(df_wide, df)

# Manually calc interactions
df_long$tract_scan <- interaction(df_long$tract_name, df_long$comp_scan)

fit_LDI <- bam(
  delta ~ s(subj_id, by = tract_scan, bs = "re") +
    s(node_id, by = tract_scan, bs = "tp", k = 40) +
    tract_name + comp_scan + tract_scan,
  data = df_long,
  family = gaussian(),
  method = "fREML",
  discrete = T
)
rds_ldi <- paste0(getwd(), "/rda_objects/fit_LDI_fa.Rda")
saveRDS(fit_LDI, file = rds_ldi)

fit_LDI <- readRDS(rds_ldi)
gam.check(fit_LDI)
summary(fit_LDI)
plot(fit_LDI)

# Plot smooths, individually
p <- getViz(fit_LDI)
plot(p)
plot(sm(p, 57))
plot(sm(p, 112))

for(num in c(57:112)){
  print(plot(sm(p, num)))
}

# Build plot obj
library("gratia")
draw(fit_LDI, select = c(57:64), scales = "fixed") # Post CC
draw(fit_LDI, select = c(65:74), scales = "fixed") # Post LH
draw(fit_LDI, select = c(75:84), scales = "fixed") # Post RH

# Plot post and rtp together
num_smooths <- length(fit_LDI$smooth)
node_smooths <- c()
name_smooths <- c()
for(num in 1:num_smooths){
  if(fit_LDI[["smooth"]][[num]][["term"]] == "node_id"){
    node_smooths <- c(node_smooths, num)
    label <- fit_LDI[["smooth"]][[num]][["label"]]
    h <- strsplit(label, "tract_scan")[[1]][2]
    name_smooths <- c(name_smooths, strsplit(h, "\\.")[[1]][1])
  }
}
half <- length(node_smooths)/2
post_smooths <- head(node_smooths, half)
rtp_smooths <- tail(node_smooths, half)
name_smooths <- head(name_smooths, half) # Names appear twice

p <- getViz(fit_LDI)
c <- 1
pp <- plot(sm(p, post_smooths[c]))
pr <- plot(sm(p, rtp_smooths[c]))

# Identify max diff nodes
p_info <- plot(sm(p, 58))
p_data <- as.data.frame(p_info$data$fit)
max_y <- max(abs(p_data$y))
node <- p_data[which(abs(p_data$y) == max_y), ]$x


# GAM: WB Longitudinal ----
#
# As GAM: WB diff is working, use long model to avoid
# dropping data in diff calc.
#
# Runs in ~30 hours, seems like need more data for this model.
df <- subset(
  df_afq,
  select = c("subj_id", "scan_name", "tract_name", "node_id", "dti_fa")
)
df$tract_scan <- interaction(df$tract_name, df$scan_name)

fit_LGI <- bam(
  dti_fa ~ s(subj_id, by = tract_scan, bs = "re") +
    s(node_id, by = tract_name, bs = "tp", k = 40) +
    s(node_id, by = tract_scan, bs = "tp", k = 40) +
    tract_name + scan_name + tract_scan,
  data = df,
  family = betar(link = "logit"),
  method = "fREML",
  discrete = T,
  nthreads = 12
)
rds_lgi <- paste0(getwd(), "/rda_objects/fit_LGI_fa.Rda")
saveRDS(fit_LGI, file = rds_lgi)

fit_LGI <- readRDS(rds_lgi)
gam.check(fit_LGI)
summary(fit_LGI)
plot(fit_LGI)

p <- getViz(fit_LGI)
plot(p)



# GAM: Post tract by K-group 1 vs 2, 3 ----
tract <- "Callosum Temporal"
df <- df_afq[which(df_afq$tract_name == tract & df_afq$scan_name == "post"), ]

subj_exp <- df_post_grp[which(df_post_grp$km_grp != 1), ]$subj_id
df$grp <- "con"
df[which(df$subj_id %in% subj_exp), ]$grp <- "exp"
df$grp <- factor(df$grp)

plot(df$node_id, df$dti_fa)
hist(df$dti_fa)

fit_GS <- bam(
  dti_fa ~ s(subj_id, bs = "re") +
    s(node_id, bs = "tp", k = 40) +
    s(node_id, grp, bs = "tp", k = 40),
  data = df,
  family = betar(link = "logit"),
  method = "fREML",
  discrete = T
)
gam.check(fit_GS)
summary(fit_GS)
plot(fit_GS)

p <- getViz(fit_GS)
plot(p)


# Global fit, ordered group smooth
df$grpOF <- factor(df$grp, ordered = T)
fit_GSO <- bam(
  dti_fa ~ s(subj_id, bs = "re") +
    s(node_id, bs = "tp", k = 40) +
    s(node_id, by = grpOF, bs = "fs", k = 40),
  data = df,
  family = betar(link = "logit"),
  method = "fREML",
  discrete = T
)
gam.check(fit_GSO)
summary(fit_GSO)
plot(fit_GSO)

p <- getViz(fit_GSO)
plot(p)


# Global fit, group smooth, memory
impact_meas <- "mem_vis"
df <- merge(
  x = df,
  y = df_scan_imp[, c("subj_id", "scan_name", impact_meas)],
  by = c("subj_id", "scan_name"),
  all.x = T
)

# s(node_id, scan_name, bs = "fs", k = 40) +
# s(mem_vis, by = scan_name, bs = "tp", k = 5) +
fit_GS_intx <- bam(
  dti_fa ~ s(subj_id, bs = "re") +
    s(node_id, bs = "tp", k = 40) +
    ti(
      node_id, mem_vis,
      by = grp,
      bs = c("tp", "tp"), k = c(50, 5)
    ),
  data = df,
  family = betar(link = "logit"),
  method = "fREML",
  discrete = T,
  nthreads = 4
)
gam.check(fit_GS_intx)
summary(fit_GS_intx)
plot(fit_GS_intx)

p <- getViz(fit_GS_intx)
plot(p)

#
open3d()
mfrow3d(1, 2)
plotRGL(sm(p, 3), xlab = "mem_vis", ylab = "node_id", main = "base", residuals = T)
next3d()
plotRGL(sm(p, 4), xlab = "mem_vis", ylab = "node_id", main = "post", residuals = T)


# Global fit, ordered group smooth, memory
fit_GSO_intx <- bam(
  dti_fa ~ s(subj_id, bs = "re") +
    s(node_id, bs = "tp", k = 40) +
    ti(
      node_id, mem_vis,
      by = grpOF,
      bs = c("tp", "tp"), k = c(50, 5)
    ),
  data = df,
  family = betar(link = "logit"),
  method = "fREML",
  discrete = T,
  nthreads = 4
)
gam.check(fit_GSO_intx)
summary(fit_GSO_intx)
plot(fit_GSO_intx)

p <- getViz(fit_GSO_intx)
plot(p)

#
open3d()
mfrow3d(1, 1)
plotRGL(sm(p, 3), xlab = "mem_vis", ylab = "node_id", main = "base", residuals = T)





# GAM: Single Subj WB delta ----
subj <- "142" # From K-means cluster
df <- df_afq[which(df_afq$subj_id == subj), ]
df_sub <- df[which(df$node_id == 10),]
table(df_sub$tract_name)

#!pArc, vOcc
df <- subset(
  df,
  select = c("subj_id", "scan_name", "tract_name", "node_id", "dti_fa")
)
df_wide <- reshape(
  df,
  idvar = c("subj_id", "node_id", "tract_name"), timevar = "scan_name",
  direction = "wide"
)
df_wide$delta.post_base <- df_wide$dti_fa.post - df_wide$dti_fa.base
df_wide$delta.rtp_base <- df_wide$dti_fa.rtp - df_wide$dti_fa.base

df_wide <- subset(
  df_wide,
  select = c(
    "subj_id", "tract_name", "node_id", "delta.post_base", "delta.rtp_base"
  )
)
df_long <- reshape(
  df_wide,
  direction = "long",
  varying = c("delta.post_base", "delta.rtp_base"),
  times = c("post_base", "rtp_base"),
  idvar = c("subj_id", "tract_name", "node_id")
)
colnames(df_long)[4] <- c("comp_scan")
rownames(df_long) <- NULL
df_long$comp_scan <- factor(df_long$comp_scan)
rm(df_wide, df)

# Manually calc interactions
df_long$tract_scan <- interaction(df_long$tract_name, df_long$comp_scan)

# LDI model
fit_LDI <- bam(
  delta ~ s(node_id, by = tract_scan, bs = "tp", k = 40) +
    tract_name + comp_scan + tract_scan,
  data = df_long,
  family = gaussian(),
  method = "fREML",
  discrete = T, 
  nthreads = 12
)
gam.check(fit_LDI)

# Plot smooths, individually
p <- getViz(fit_LDI)
plot(p)
# plot(sm(p, 57))
# plot(sm(p, 112))
# 
# for(num in c(57:112)){
#   print(plot(sm(p, num)))
# }

draw(fit_LDI, select = c(1:8), scales = "fixed") # Post CC
draw(fit_LDI, select = c(9:18), scales = "fixed") # Post LH
draw(fit_LDI, select = c(19:28), scales = "fixed") # Post RH


# GAM: Tract by Scan with Behavior ----
# s(mem_vis, by=scan_name, bs = "tp") +
tract <- "Left Inferior Fronto-occipital"
df <- df_afq[which(df_afq$tract_name == tract), ]

impact_meas <- "mem_vis"
df <- merge(
  x = df,
  y = df_scan_imp[, c("subj_id", "scan_name", impact_meas)],
  by = c("subj_id", "scan_name"),
  all.x = T
)

# Global tract smooth, intx of group smooth and main effect of mem_vis

# s(node_id, scan_name, bs = "fs", k = 40) +
# s(mem_vis, by = scan_name, bs = "tp", k = 5) +
fit_LGI_intx <- bam(
  dti_fa ~ s(subj_id, scan_name, bs = "re") +
    s(node_id, bs = "tp", k = 40, m = 2) +
    s(mem_vis, by = scan_name, bs = "tp", k = 5) +
    ti(
      node_id, mem_vis, by = scan_name,
      bs = c("tp", "tp"), k = c(50, 5), m = 1
    ),
  data = df,
  family = betar(link = "logit"),
  method = "fREML",
  discrete = T,
  nthreads = 12
)
gam.check(fit_LGI_intx)
summary(fit_LGI_intx)
plot(fit_LGI_intx, pages=1)

p <- getViz(fit_LGI_intx)
plot(p)

#
open3d()
mfrow3d(1, 3)
plotRGL(sm(p, 3), xlab = "mem_vis", ylab = "node_id", main = "base", residuals = T)
next3d()
plotRGL(sm(p, 4), xlab = "mem_vis", ylab = "node_id", main = "post", residuals = T)
next3d()
plotRGL(sm(p, 5), xlab = "mem_vis", ylab = "node_id", main = "rtp", residuals = T)

length(which(df$scan_name == "post" & df$node_id == 10))
length(which(df$scan_name == "rtp" & df$node_id == 10))

# Organize Plots
plot_obj <- getViz(fit_LGI_intx)
p <- plot(sm(plot_obj, 6))
p_data <- as.data.frame(p$data$fit)
colnames(p_data) <- c("fit", "tfit", "mem_vis", "node_id", "se")

ggplot(p_data, aes(x = node_id, y = mem_vis, z = fit)) +
  geom_tile(aes(fill = fit)) +
  geom_contour(colour = "black") +
  scale_x_continuous(breaks = c(seq(10, 89, by = 10), 89)) +
  scale_fill_viridis(
    option = "D", 
    name = "Est. FA Fit"
  ) +
  labs(
    title = paste("Base: IMPACT Comp by", tract),
    x = "Tract Node"
  ) +
  theme(
    text = element_text(family = "Times New Roman"),
    plot.title = element_text(size = 12),
    legend.text=element_text(size = 10),
  )


p <- plot(sm(plot_obj, 7))
p_data <- as.data.frame(p$data$fit)
colnames(p_data) <- c("fit", "tfit", "mem_vis", "node_id", "se")

ggplot(p_data, aes(x = node_id, y = mem_vis, z = fit)) +
  geom_tile(aes(fill = fit)) +
  geom_contour(colour = "black") +
  scale_x_continuous(breaks = c(seq(10, 89, by = 10), 89)) +
  scale_fill_viridis(
    option = "D", 
    name = "Est. FA Fit"
  ) +
  labs(
    title = paste("Post: IMPACT Comp by", tract),
    x = "Tract Node"
  ) +
  theme(
    text = element_text(family = "Times New Roman"),
    plot.title = element_text(size = 12),
    legend.text=element_text(size = 10),
  )


p <- plot(sm(plot_obj, 8))
p_data <- as.data.frame(p$data$fit)
colnames(p_data) <- c("fit", "tfit", "mem_vis", "node_id", "se")

ggplot(p_data, aes(x = node_id, y = mem_vis, z = fit)) +
  geom_tile(aes(fill = fit)) +
  geom_contour(colour = "black") +
  scale_x_continuous(breaks = c(seq(10, 89, by = 10), 89)) +
  scale_fill_viridis(
    option = "D", 
    name = "Est. FA Fit"
  ) +
  labs(
    title = paste("RTP: IMPACT Comp by", tract),
    x = "Tract Node"
  ) +
  theme(
    text = element_text(family = "Times New Roman"),
    plot.title = element_text(size = 12),
    legend.text=element_text(size = 10),
  )


# GAM: Tract by Ordered Scan with Behavior ----
df$scanOF <- factor(df$scan_name, ordered = T)
fit_LGIO_intx <- bam(
  dti_fa ~ s(subj_id, scan_name, bs = "re") +
    s(node_id, bs = "tp", k = 40, m = 2) +
    s(mem_vis, by = scan_name, bs = "tp", k = 5) +
    ti(node_id, mem_vis, bs = c("tp", "tp"), k = c(50, 5), m = 1) +
    ti(
      node_id, mem_vis, by = scanOF, 
      bs = c("tp", "tp"), k = c(50, 5), m = 1
    ),
  data = df,
  family = betar(link = "logit"),
  method = "fREML",
  discrete = T,
  nthreads = 12
)
gam.check(fit_LGIO_intx)
summary(fit_LGIO_intx)
plot(fit_LGIO_intx, pages=1)
plot(fit_LGIO_intx, scheme=2)

plot_obj <- getViz(fit_LGIO_intx)
plot(plot_obj)


#
open3d()
mfrow3d(1, 2)
plotRGL(
  sm(plot_obj, 7), xlab = "mem_vis", ylab = "node_id", 
  main = "post-base", residuals = T, se = F
)
next3d()
plotRGL(
  sm(plot_obj, 8), xlab = "mem_vis", ylab = "node_id", 
  main = "rtp-base", residuals = T, se = F
)
aspect3d(1, 2, 1)


# Organize Plots
plot_obj <- getViz(fit_LGIO_intx)

p <- plot(sm(plot_obj, 7))
p_data <- as.data.frame(p$data$fit)
colnames(p_data) <- c("fit", "tfit", "mem_vis", "node_id", "se")
ggplot(p_data, aes(x = node_id, y = mem_vis, z = fit)) +
  geom_tile(aes(fill = fit)) +
  geom_contour(colour = "black") +
  scale_x_continuous(breaks = c(seq(10, 89, by = 10), 89)) +
  scale_fill_viridis(
    option = "D", 
    name = "Est. FA Fit"
  ) +
  labs(
    title = paste("Post-Base: IMPACT Comp by", tract),
    x = "Tract Node"
  ) +
  theme(
    text = element_text(family = "Times New Roman"),
    plot.title = element_text(size = 12),
    legend.text=element_text(size = 10),
  )


p <- plot(sm(plot_obj, 8))
p_data <- as.data.frame(p$data$fit)
colnames(p_data) <- c("fit", "tfit", "mem_vis", "node_id", "se")
ggplot(p_data, aes(x = node_id, y = mem_vis, z = fit)) +
  geom_tile(aes(fill = fit)) +
  geom_contour(colour = "black") +
  scale_x_continuous(breaks = c(seq(10, 89, by = 10), 89)) +
  scale_fill_viridis(
    option = "D", 
    name = "Est. FA Fit"
  ) +
  labs(
    title = paste("RTP-Base: IMPACT Comp by", tract),
    x = "Tract Node"
  ) +
  theme(
    text = element_text(family = "Times New Roman"),
    plot.title = element_text(size = 12),
    legend.text=element_text(size = 10),
  )


# Check data density
df_imp_post <- df_scan_imp[which(df_scan_imp$scan_name == "post"), ]
plot(df_imp_post$mem_vis)
df_imp <- df[which(df$node_id == 53), ]
ggplot(
  data = df_imp,
  aes(y = mem_vis, x = dti_fa, color = scan_name)
) +
  geom_point()


# GAM: Tract by Scan for K-Groups 2, 3 ----
k_keep <- df_post_grp[which(df_post_grp$km_grp != 1), ]$subj_id
df_k <- df[which(df$subj_id %in% k_keep), ]

# Global fit + group smooths
fit_GS <- bam(
  dti_fa ~ s(subj_id, scan_name, bs = "re") +
    s(node_id, bs = "tp", k = 40) +
    s(node_id, scan_name, bs = "fs", k = 40),
  data = df_k,
  family = betar(link = "logit"),
  method = "fREML",
  discrete = T
)
gam.check(fit_GS)
summary(fit_GS)
plot(fit_GS)

p <- getViz(fit_GS)
plot(p)


# Global fit + ordered group smooths (ref = base)
df_k$scanOF <- factor(df_k$scan_name, ordered = T)

fit_GSO <- bam(
  dti_fa ~ s(subj_id, scan_name, bs = "re") +
    s(node_id, bs = "tp", k = 40) +
    s(node_id, by = scanOF, bs = "fs", k = 40),
  data = df_k,
  family = betar(link = "logit"),
  method = "fREML",
  discrete = T
)
gam.check(fit_GSO)
summary(fit_GSO)
plot(fit_GSO)

p <- getViz(fit_GSO)
plot(p)


