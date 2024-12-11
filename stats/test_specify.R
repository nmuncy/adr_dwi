# Load Env ----
library(mgcv)
library(mgcViz)
library(viridis)
library(rgl)
library("factoextra")
library("ggbiplot")
library("psych")
library("ggplot2")
library("ggpubr")


# Get Data ----
data_dir <- "/Users/nmuncy2/Projects/data/"
data_afq <- paste0(data_dir, "df_afq.csv")
data_imp <- paste0(data_dir, "df_scan_imp.csv")

# out_afq <- write.csv(df_afq, file = paste0(data_dir, "df_afq.csv"), row.names=F)
# out_imp <- write.csv(df_scan_imp, file = paste0(data_dir, "df_scan_imp.csv"), row.names=F)

df_afq <- read.csv(data_afq)
df_afq$subj_id <- factor(df_afq$subj_id)
df_afq$scan_name <- factor(df_afq$scan_name)
df_afq$tract_name <- factor(df_afq$tract_name)

df_scan_imp <- read.csv(data_imp)
df_scan_imp$subj_id <- factor(df_scan_imp$subj_id)
df_scan_imp$scan_name <- factor(df_scan_imp$scan_name)
df_scan_imp$impact_name <- factor(df_scan_imp$impact_name)


# PCA: Post Impact ----
df_post <- df_scan_imp[which(df_scan_imp$scan_name == "post"), ]
df_post <- df_post[complete.cases(df_post[, 7:10]), ]

hist(df_post$mem_ver, breaks = 20)
hist(df_post$mem_vis, breaks = 20)
hist(df_post$vis_mot, breaks = 20)
hist(df_post$rx_time, breaks = 20)
hist(df_post$imp_ctl, breaks = 20)

pc_post <- prcomp(df_post[,7:10], center=T, scale. = T)
pc_post$center
pc_post$scale
print(pc_post)
summary(pc_post)

fviz_eig(pc_post, addlabels=T)
fviz_pca_biplot(pc_post, label="var")

# ellipse.prob=0.68
# ellipse=T,
ggbiplot(
  pc_post,
  obs.scale = 1,
  var.scale = 1,
  circle=T
) + 
  scale_color_discrete(name="") +
  theme(legend.direction = "horizontal")

# Invert rx_time 
df_post$rx_time_inv <- 
  (max(df_post$rx_time) - df_post$rx_time) + min(df_post$rx_time)
pc_post <- prcomp(df_post[,c(7:9,15)], center=T, scale. = T)
pc_post$center
print(pc_post)
summary(pc_post) # no impact of inversion
fviz_eig(pc_post, addlabels=T)
fviz_pca_biplot(pc_post, label="var", repel=T)


# K-means: Post Impact ----
#
# 3 centers: ss = 62.8%
# 4 centers: ss = 71.3%
data_norm <- scale(df_post[, 7:10])
data_dist <- dist(data_norm)
fviz_nbclust(data_norm, kmeans, method = "wss")

km_post <- kmeans(data_norm, centers = 3, nstart=100)
print(km_post)

km_clust <- km_post$cluster
rownames(data_norm) <- df_post$subj_id
fviz_cluster(list(data=data_norm, cluster = km_clust))
table(km_clust)

km_clust <- as.data.frame(km_clust)
rownames(km_clust) <- df_post$subj
km_clust <- cbind(subj_id=rownames(km_clust), km_clust)
rownames(km_clust) <- NULL


# Viz: Post Impact by K-group ----
#
# K-groups show high/med/low/performance on task.
# 4 clusters seems to split "med" group. Strong
# differentiation in mem_vis ~ vis_mot.
#
df_post_grp <- merge(
  x=df_post,
  y=km_clust,
  by="subj_id",
  all = T
)

plot(df_post_grp$mem_ver, col=factor(df_post_grp$km_clust))
plot(df_post_grp$mem_vis, col=factor(df_post_grp$km_clust))
plot(df_post_grp$vis_mot, col=factor(df_post_grp$km_clust))
plot(df_post_grp$rx_time, col=factor(df_post_grp$km_clust))

pairs.panels(
  df_post_grp[, 7:10],
  bg=c("blue", "red", "green")[df_post_grp$km_clust],
  gap = 0,
  pch = 21
)

# Visualize Simpson's Paradox
ggplot(
  data=df_post_grp,
  aes(x=vis_mot, y=mem_vis, color=factor(km_clust))
) +
  geom_point() +
  stat_smooth(method=lm) +
  scale_color_manual(
    breaks=c("1", "2", "3"),
    values=c("blue", "red", "green")
  ) +
  stat_cor(method="pearson")
  
ggplot(
  data=df_post_grp,
  aes(x=vis_mot, y=mem_vis)
) +
  geom_point(aes(color=factor(km_clust))) +
  stat_smooth(method=lm, aes(color=factor(km_clust)), se=F) +
  scale_color_manual(
    breaks=c("1", "2", "3"),
    values=c("blue", "red", "green")
  ) +
  stat_cor(method="pearson", aes(color=factor(km_clust))) +
  stat_cor(method="pearson", label.x=40, label.y=38) +
  geom_smooth(method=lm, color="black", se=F)


# GAM: Impact mem_vis ----
df <- subset(
  df_scan_imp, 
  select = c(
    "subj_id", "scan_name", "mem_ver", "mem_vis", 
    "vis_mot", "rx_time", "imp_ctl", "tot_symp"
  )
)

df$scan_time <-1
df[which(df$scan_name == "post"),]$scan_time <- 2
df[which(df$scan_name == "rtp"),]$scan_time <- 3


# descdist(df$mem_vis)
# hist(df$mem_vis)
fit_mem <- gam(
  mem_vis ~ s(scan_time, bs="tp", k=3) + s(subj_id, bs = "re"),
  data = df,
  family = gaussian(),
  method = "REML"
)
plot(fit_mem)



# GAM: Tract by Scan ----
tract <- "Callosum Temporal"
df <- df_afq[which(df_afq$tract_name == tract), ]

# Global fit + group smooths
fit_GS <- bam(
  dti_fa ~ s(subj_id, scan_name, bs = "re") +
    s(node_id, bs = "tp", k = 40) +
    s(node_id, scan_name, bs = "fs", k = 40),
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

# Global fit + ordered group smooths (ref = base)
df$scanOF <- factor(df$scan_name, ordered = T)

fit_GSO <- bam(
  dti_fa ~ s(subj_id, scan_name, bs = "re") +
    s(node_id, bs = "tp", k = 40) +
    s(node_id, by = scanOF, bs = "fs", k = 40),
  data = df,
  family =  betar(link = "logit"),
  method = "fREML",
  discrete = T
)
gam.check(fit_GSO)
summary(fit_GSO)
plot(fit_GSO)

p <- getViz(fit_GSO)
plot(p)


# GAM: Tract by Scan for K-Groups 2, 3 ----
k_keep <- df_post_grp[which(df_post_grp$km_clust != 1), ]$subj_id
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
  family =  betar(link = "logit"),
  method = "fREML",
  discrete = T
)
gam.check(fit_GSO)
summary(fit_GSO)
plot(fit_GSO)

p <- getViz(fit_GSO)
plot(p)


# GAM: Tract by Scan with Behavior ----
# s(mem_vis, by=scan_name, bs = "tp") +
df <- df_k

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
fit_GS_intx <- bam(
  dti_fa ~ s(subj_id, scan_name, bs = "re") +
    s(node_id, bs = "tp", k = 40) +
    ti(
      node_id, mem_vis, by = scan_name, 
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
mfrow3d(1,3)
plotRGL(sm(p, 3), xlab = "mem_vis", ylab="node_id", main="base", residuals=T)
next3d()
plotRGL(sm(p, 4), xlab = "mem_vis", ylab="node_id", main="post", residuals=T)
next3d()
plotRGL(sm(p, 5), xlab = "mem_vis", ylab="node_id", main="rtp", residuals=T)

length(which(df_k$scan_name == "post" & df_k$node_id == 10))
length(which(df_k$scan_name == "rtp" & df_k$node_id == 10))

# Predict
node_list <- unique(df$node_id)
num_node <- length(node_list)

df_base <- df[which(df$scan_name == "base"), ]
df_base <- na.omit(df_base)
df_base_cov <- df_base[which(df_base$node_id == node_list[1]), ]
subj_base <- as.character(df_base_cov$subj_id)
num_base <- length(subj_base)
seq_base_cov <- seq(
  min(df_base_cov[, "mem_vis"]), 
  max(df_base_cov[, "mem_vis"]), 
  length = num_base
)

df_post <- df[which(df$scan_name == "post"), ]
df_post <- na.omit(df_post)
df_post_cov <- df_post[which(df_post$node_id == node_list[1]), ]
subj_post <- as.character(df_post_cov$subj_id)
num_post <- length(subj_post)
seq_post_cov <- seq(
  min(df_post_cov[, "mem_vis"]), 
  max(df_post_cov[, "mem_vis"]), 
  length = num_post
)

df_rtp <- df[which(df$scan_name == "rtp"), ]
df_rtp <- na.omit(df_rtp)
df_rtp_cov <- df_rtp[which(df_rtp$node_id == node_list[1]), ]
subj_rtp <- as.character(df_rtp_cov$subj_id)
num_rtp <- length(subj_rtp)
seq_rtp_cov <- seq(
  min(df_rtp_cov[, "mem_vis"]), 
  max(df_rtp_cov[, "mem_vis"]), 
  length = num_rtp
)

df_pred_base_intx <- data.frame(
  subj_id = rep(subj_base[1], each = num_node, num_base),
  scan_name = df_base$scan_name,
  node_id = df_base$node_id,
  mem_vis = rep(seq_base_cov, each = num_node)
)
pred_base_fit <- predict(fit_GS_intx, df_pred_base_intx)
ind_excl <- exclude.too.far(
  df_pred_base_intx$node_id, df_pred_base_intx$mem_vis,
  df_base$node_id, df_base[, "mem_vis"],
  dist = 0.1
)
pred_base_fit[ind_excl] <- NA
df_pred_base_intx <- cbind(df_pred_base_intx, fit = pred_base_fit)

z_min <- min(c(df_pred_base_intx$fit, df_pred_base_intx$fit), na.rm = T)
z_max <- max(c(df_pred_base_intx$fit, df_pred_base_intx$fit), na.rm = T)

ggplot(
  df_pred_base_intx, 
  aes(x = node_id, y = mem_vis, z = fit)
  ) +
  geom_tile(aes(fill = fit)) +
  geom_contour(colour = "black") +
  scale_x_continuous(breaks = c(seq(10, 89, by = 10), 89)) +
  scale_fill_viridis(
    option = "D", 
    name = "Est. FA Fit",
    limits = c(z_min, z_max)
  ) +
  theme(
    text = element_text(family = "Times New Roman"),
    plot.title = element_text(size = 12),
    legend.text=element_text(size = 10),
    axis.title.y = element_blank(),
    axis.title.x = element_blank()
  )



df_pred_post_intx <- data.frame(
  subj_id = rep(subj_post[1], each = num_node, num_post),
  scan_name = df_post$scan_name,
  node_id = df_post$node_id,
  mem_vis = rep(seq_post_cov, each = num_node)
)
pred_post_fit <- predict(fit_GS_intx, df_pred_post_intx)
ind_excl <- exclude.too.far(
  df_pred_post_intx$node_id, df_pred_post_intx$mem_vis,
  df_post$node_id, df_post[, "mem_vis"],
  dist = 0.1
)
pred_post_fit[ind_excl] <- NA
df_pred_post_intx <- cbind(df_pred_post_intx, fit = pred_post_fit)

ggplot(
  df_pred_post_intx, 
  aes(x = node_id, y = mem_vis, z = fit)
) +
  geom_tile(aes(fill = fit)) +
  geom_contour(colour = "black") +
  scale_x_continuous(breaks = c(seq(10, 89, by = 10), 89)) +
  scale_fill_viridis(
    option = "D", 
    name = "Est. FA Fit",
    limits = c(z_min, z_max)
  ) +
  theme(
    text = element_text(family = "Times New Roman"),
    plot.title = element_text(size = 12),
    legend.text=element_text(size = 10),
    axis.title.y = element_blank(),
    axis.title.x = element_blank()
  )



df_pred_rtp_intx <- data.frame(
  subj_id = rep(subj_rtp[1], each = num_node, num_rtp),
  scan_name = df_rtp$scan_name,
  node_id = df_rtp$node_id,
  mem_vis = rep(seq_rtp_cov, each = num_node)
)
pred_rtp_fit <- predict(fit_GS_intx, df_pred_rtp_intx)
ind_excl <- exclude.too.far(
  df_pred_rtp_intx$node_id, df_pred_rtp_intx$mem_vis,
  df_rtp$node_id, df_rtp[, "mem_vis"],
  dist = 0.1
)
pred_rtp_fit[ind_excl] <- NA
df_pred_rtp_intx <- cbind(df_pred_rtp_intx, fit = pred_rtp_fit)

ggplot(
  df_pred_rtp_intx, 
  aes(x = node_id, y = mem_vis, z = fit)
) +
  geom_tile(aes(fill = fit)) +
  geom_contour(colour = "black") +
  scale_x_continuous(breaks = c(seq(10, 89, by = 10), 89)) +
  scale_fill_viridis(
    option = "D", 
    name = "Est. FA Fit",
    limits = c(z_min, z_max)
  ) +
  theme(
    text = element_text(family = "Times New Roman"),
    plot.title = element_text(size = 12),
    legend.text=element_text(size = 10),
    axis.title.y = element_blank(),
    axis.title.x = element_blank()
  )



# GAM: Tract by Ordered Scan with Behavior ----
fit_GSO_intx <- bam(
  dti_fa ~ s(subj_id, scan_name, bs = "re") +
    s(node_id, bs = "tp", k = 40) +
    ti(
      node_id, mem_vis, by = scanOF, 
      bs = c("tp", "tp"), k = c(50, 5)
    ),
  data = df,
  family = betar(link = "logit"),
  method = "fREML",
  discrete = T
)
gam.check(fit_GSO_intx)
summary(fit_GSO_intx)
plot(fit_GSO_intx)

p <- getViz(fit_GSO_intx)
plot(p)

saveRDS(fit_GSO_intx, file=file_gso_intx)

#
open3d()
mfrow3d(1,2)
plotRGL(sm(p, 3), xlab = "mem_vis", ylab="node_id", main="post-base")
next3d()
plotRGL(sm(p, 4), xlab = "mem_vis", ylab="node_id", main="rtp-base")



node_list <- unique(df$node_id)
num_node <- length(node_list)

df_post <- df[which(df$scan_name == "post"), ]
df_post <- na.omit(df_post)
df_post_cov <- df_post[which(df_post$node_id == node_list[1]), ]
subj_post <- as.character(df_post_cov$subj_id)
num_post <- length(subj_post)
seq_post_cov <- seq(
  min(df_post_cov[, "mem_vis"]), 
  max(df_post_cov[, "mem_vis"]), 
  length = num_post
)

df_pred_post_intx <- data.frame(
  subj_id = rep(subj_post[1], each = num_node, num_post),
  scan_name = df_post$scan_name,
  scanOF = df_post$scanOF,
  node_id = df_post$node_id,
  mem_vis = rep(seq_post_cov, each = num_node)
)
pred_intx_diff <- as.data.frame(predict.gam(
  fit_GSO_intx, df_pred_post_intx, type = "terms"
))
colnames(pred_intx_diff) <- c("subj.scan", "node", "fit.post", "fit.rtp")
pred_intx_diff <- cbind(df_pred_post_intx, fit = pred_intx_diff$fit.post)

ggplot(
  pred_intx_diff, 
  aes(x = node_id, y = mem_vis, z = fit)
) +
  geom_tile(aes(fill = fit)) +
  geom_contour(colour = "black") +
  scale_x_continuous(breaks = c(seq(10, 89, by = 10), 89)) +
  scale_fill_viridis(option = "D", name = "Est. FA Fit") +
  labs(x = "Tract Node") +
  theme(
    text = element_text(family = "Times New Roman"),
    plot.title = element_text(size = 12),
    legend.text=element_text(size = 10),
    axis.title.y = element_blank(),
    axis.title.x = element_blank()
  )


df_rtp <- df[which(df$scan_name == "rtp"), ]
df_rtp <- na.omit(df_rtp)
df_rtp_cov <- df_rtp[which(df_rtp$node_id == node_list[1]), ]
subj_rtp <- as.character(df_rtp_cov$subj_id)
num_rtp <- length(subj_rtp)
seq_rtp_cov <- seq(
  min(df_rtp_cov[, "mem_vis"]), 
  max(df_rtp_cov[, "mem_vis"]), 
  length = num_rtp
)

df_pred_rtp_intx <- data.frame(
  subj_id = rep(subj_rtp[1], each = num_node, num_rtp),
  scan_name = df_rtp$scan_name,
  scanOF = df_rtp$scanOF,
  node_id = df_rtp$node_id,
  mem_vis = rep(seq_rtp_cov, each = num_node)
)
pred_intx_diff <- as.data.frame(predict.gam(
  fit_GSO_intx, df_pred_rtp_intx, type = "terms"
))
colnames(pred_intx_diff) <- c("subj.scan", "node", "fit.post", "fit.rtp")
pred_intx_diff <- cbind(df_pred_rtp_intx, fit = pred_intx_diff$fit.rtp)

ggplot(
  pred_intx_diff, 
  aes(x = node_id, y = mem_vis, z = fit)
) +
  geom_tile(aes(fill = fit)) +
  geom_contour(colour = "black") +
  scale_x_continuous(breaks = c(seq(10, 89, by = 10), 89)) +
  scale_fill_viridis(option = "D", name = "Est. FA Fit") +
  labs(x = "Tract Node") +
  theme(
    text = element_text(family = "Times New Roman"),
    plot.title = element_text(size = 12),
    legend.text=element_text(size = 10),
    axis.title.y = element_blank(),
    axis.title.x = element_blank()
  )
