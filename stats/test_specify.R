# Load Env ----
library(mgcv)
library(mgcViz)
# library(fitdistrplus)


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


# Model Impact mem_vis ----
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

# fit_mem <- gam(
#   mem_vis ~ s(subj_id, bs = "re") + 
#     s(scan_time, k=3) + 
#     s(subj_id, scan_time, m=1),
#   data = df,
#   family = gaussian(),
#   method = "REML"
# )


# GAM: Tract by Group ----
tract <- "Callosum Temporal"
df <- df_afq[which(df_afq$tract_name == tract), ]

# Global fit + group smooths
file_gs <- paste0(data_dir, "fit_GS.Rda")
# fit_GS <- readRDS(file_gs)

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
saveRDS(fit_GS, file=file_gs)

p <- getViz(fit_GS)
plot(p)

# Global fit + ordered group smooths (ref = base)
df$scanOF <- factor(df$scan_name, ordered = T)

file_gso <- paste0(data_dir, "fit_GSO.Rda")
# fit_GSO <- readRDS(file_gso)

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
saveRDS(fit_GSO, file=file_gso)

p <- getViz(fit_GSO)
plot(p)


# GAM: Tract by Group with behavior ----
# s(mem_vis, by=scan_name, bs = "tp") +
impact_meas <- "mem_vis"
df <- merge(
  x = df,
  y = df_scan_imp[, c("subj_id", "scan_name", impact_meas)],
  by = c("subj_id", "scan_name"),
  all.x = T
)

# Global tract smooth, intx of group smooth and main effect of mem_vis
file_gs_intx <- paste0(data_dir, "fit_GS_intx.Rda")
# fit_GS_intx <- readRDS(file_gs_intx)

fit_GS_intx <- bam(
  dti_fa ~ s(subj_id, scan_name, bs = "re") +
    s(node_id, bs = "tp", k = 40) +
    s(mem_vis, by = scan_name, bs = "tp", k = 5) +
    ti(
      node_id, mem_vis, by=scan_name, 
      bs = c("tp", "tp"), k = c(50, 10), m=2
    ),
  data = df,
  family = betar(link = "logit"),
  method = "fREML",
  discrete = T
)
summary(fit_GS_intx)
plot(fit_GS_intx)
p <- getViz(fit_GS_intx)
plot(p)
saveRDS(fit_GS_intx, file=file_gs_intx)


# ti(node_id, imp_meas, bs = c("tp", "tp"), k = c(50, 5)) +
# s(imp_meas, by = scan_name, bs = "tp", k = 5) +
file_gso_intx <- paste0(data_dir, "fit_GSO_intx.Rda")
# fit_GSO_intx <- readRDS(file_gso_intx)

fit_GSO_intx <- bam(
  dti_scalar ~ s(subj_id, scan_name, bs = "re") +
    s(node_id, bs = "tp", k = 40) +
    ti(
      node_id, imp_meas, by = scanOF, 
      bs = c("tp", "tp"), k = c(50, 10)
    ),
  data = df,
  family = betar(link = "logit"),
  method = "fREML",
  discrete = T
)
saveRDS(fit_GSO_intx, file=file_gso_intx)