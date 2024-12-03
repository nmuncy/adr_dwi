# Get Resources ----
library("mgcv")
library("fitdistrplus")
library("itsadug")
library("mgcViz")
library("viridis")

source(paste0(getwd(), "/resources/pull_data.R"))
source(paste0(getwd(), "/resources/draw_plots.R"))


# Pull Data ----
df <- get_afq()


# Get Parietal data---- 
tract <- "Callosum Superior Parietal"
df_par <- df[
  which(
    df$tract_name == tract
    & df$sess_name %in% c("base", "post")
  ),
  c("subj_id", "sess_name", "node_id", "dti_fa")
]
df_par$subj_id <- factor(df_par$subj_id)
df_par$sess_name <- factor(df_par$sess_name)
df_par <- df_par[order(df_par$subj_id, df_par$sess_name, df_par$node_id),]
df_par <- df_par[which(df_par$node_id > 9 & df_par$node_id < 91), ]


# Fit data with global and group smooths ----
#
# Specify a GS model, determine
# which family best fits data.
#   - Gaussian fit better.
hist(df_par$dti_fa)
descdist(df_par$dti_fa, discrete = F)

fit_norm <- bam(
  dti_fa ~ s(subj_id, bs="re") + 
    s(node_id, bs="cr", k=60, m=2) +
    s(node_id, sess_name, bs="fs", k=60, m=2),
  data = df_par,
  family = gaussian(),
  method = "fREML"
)
gam.check(fit_norm, rep=1000)
summary(fit_norm)

fit_beta <- bam(
  dti_fa ~ s(subj_id, bs="re") + 
    s(node_id, bs="cr", k=70, m=2) +
    s(node_id, sess_name, bs="fs", k=70, m=2),
  data = df_par,
  family = betar(link="logit"),
  method = "fREML"
)
gam.check(fit_beta, rep=1000)
summary(fit_beta)

compareML(fit_norm, fit_beta)
rm(fit_beta)
plot(fit_norm)


# Compare GS with GI model ----
#
# Adding group wiggliness did not improve fit.
fit_GI <- bam(
  dti_fa ~ s(subj_id, bs="re") + 
    s(sess_name, bs="re") +
    s(node_id, bs="cr", k=60, m=2) +
    s(node_id, by = sess_name, bs="cr", k=60, m=1),
  data = df_par,
  family = gaussian(),
  method = "fREML"
)
summary(fit_GI)
compareML(fit_norm, fit_GI)
rm(fit_GI)


# Test for group smooth differences ----
df_par$sessOF <- factor(df_par$sess_name, ordered = T)
fit_OF <- bam(
  dti_fa ~ s(subj_id, bs="re") + 
    s(node_id, bs="cr", k=60, m=2) +
    s(node_id, by = sessOF, bs="cr", k=60, m=2),
  data = df_par,
  family = gaussian(),
  method = "fREML"
)
summary(fit_OF)
plot(fit_OF)

plot_norm <- getViz(fit_norm)
pGlobal <- draw_global_smooth(plot_norm, 2, tract)
pGroup <- draw_group_smooth(plot_norm, 3, tract)

plot_tract_GSOF <- getViz(fit_OF)
pDiff <- draw_group_smooth_diff(plot_tract_GSOF, 3, tract)

plot_list <- list(
  "global" = pGlobal,
  "group" = pGroup,
  "diff" = pDiff
)
name_list <- list(
  "col1" = paste(tract, "Smooths"),
  "rowL" = "Est. FA Fit",
  "rowR1" = "Global",
  "rowR2" = "Group",
  "rowR3" = "Difference",
  "bot1" = "Tract Node"
)
draw_one_three(plot_list, name_list, tract)
