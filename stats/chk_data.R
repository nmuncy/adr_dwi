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
#
# TODO fit all tracts in same model
tract <- "Callosum Superior Parietal"

df_par <- df[
  which(df$tract_name == tract & df$sess_name %in% c("base", "post")),
  c("subj_id", "sess_name", "node_id", "dti_fa")
]
df_par$subj_id <- factor(df_par$subj_id)
df_par$sess_name <- factor(df_par$sess_name)
df_par <- df_par[order(df_par$subj_id, df_par$sess_name, df_par$node_id),]
df_par <- df_par[which(df_par$node_id > 9 & df_par$node_id < 91), ]


# Fit base FA data ----
#
# Strong fit, explaining 87.6% of deviance.
# Using method = "REML" did not change fit,
# and bs="cr" did not change fit.
df_base <- df_par[which(df_par$sess_name == "base"),]
hist(df_base$dti_fa)
descdist(df_base$dti_fa, discrete = F) # Will use beta dist

fit_G <- bam(
  dti_fa ~ s(node_id, bs="tp", k=40) + s(subj_id, bs="re"),
  data = df_base,
  family = betar(link="logit"),
  method = "fREML",
  discrete = T
)
gam.check(fit_G, rep=1000)
summary(fit_G)
plot(fit_G)


# Fit FA data by base, post sessions ----
#
# Ordered factor uses sets base to zero,
# checks for flatness of post.
#
# Smooth of node_id:sess_name f=2.079,
# dev expl = 87.3%.
#
# Smooth of node_id:sessOFpost f=15.69,
# dev expl = 87.3%.
fit_S <- bam(
  dti_fa ~ s(node_id, bs="tp", k=40, m=2) + 
    s(subj_id, bs="re") +
    s(node_id, sess_name, bs="fs", k=40, m=2),
  data = df_par,
  family = betar(link="logit"),
  method = "fREML",
  discrete = T
)
summary(fit_S)
plot(fit_S)

plot_S <- getViz(fit_S)
pGlobal <- draw_global_smooth(plot_S, 1, tract)
pGroup <- draw_group_smooth(plot_S, 3, tract)


df_par$sessOF <- factor(df_par$sess_name, ordered = T)
fit_SO <- bam(
  dti_fa ~ s(node_id, bs="tp", k=40, m=2) + 
    s(subj_id, bs="re") +
    s(node_id, by = sessOF, bs="fs", k=40, m=2),
  data = df_par,
  family = betar(link="logit"),
  method = "fREML",
  discrete = T
)
summary(fit_SO)
plot(fit_SO)

plot_SO <- getViz(fit_SO)
pDiff <- draw_group_smooth_diff(plot_SO, 3, tract)

# draw grid
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


# Fit as longitudinal model ----
#
# Works when using three sessions (base, post, rtp),
# but gives smooth for e/ sess*subj which is
# not of interest.
#
# References examples at:
#   - https://stats.stackexchange.com/questions/565460/is-my-gam-modeling-longitudinal-change-appropriately
#   - https://stats.stackexchange.com/questions/462932/how-do-i-correctly-specify-a-gamm-formula-to-model-interactions-of-random-and-fi
#   - https://stats.stackexchange.com/questions/391912/how-to-fit-a-longitudinal-gam-mixed-model-gamm
df_par$sess <- as.numeric(df_par$sess_name)
fit_X <- bam(
  dti_fa ~ s(node_id, bs="tp", k=40, m=2) + 
    s(sess, subj_id, bs="fs", k=2) +
    s(node_id, sess, bs="fs", k=40, m=2),
  data = df_par,
  family = betar(link="logit"),
  method = "fREML",
  discrete = T
)
summary(fit_X)
plot(fit_X)


# Fit Post-Base FA ----
#
# Rather than treating sess as different groups,
# account for intrasubject variance via Post-Base.
#
# Difference smooth is not flat f=29.80, 
# expl dev = 36.6%.
df_par <- df_par[, c("subj_id", "sess_name", "node_id", "dti_fa")]
df_wide <- reshape(
  df_par, 
  timevar = "sess_name", 
  idvar = c("subj_id", "node_id"),
  direction="wide"
)
df_wide <- df_wide[, c("subj_id", "node_id", "dti_fa.base", "dti_fa.post")]
df_wide <- df_wide[complete.cases(df_wide),]
df_wide$diff <- df_wide$dti_fa.post - df_wide$dti_fa.base

hist(df_wide$diff)
descdist(df_wide$diff, discrete = F) # normal

fit_D <- bam(
  diff ~ s(node_id, bs="tp", k=15) + s(subj_id, bs="re"),
  data = df_wide,
  family = gaussian(),
  method = "fREML",
  discrete = T
)
gam.check(fit_D, rep=1000)
summary(fit_D)
plot(fit_D)

plot_D <- getViz(fit_D)
pDiff <- draw_group_smooth_diff(plot_D, 1, tract)



