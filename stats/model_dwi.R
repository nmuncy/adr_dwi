# Get resources ----
library("modules")
library("gridExtra")

workflows <- modules::use("workflows.R")


# Get cleaned data ----
df_afq <- workflows$clean_afq()
df_scan_imp <- workflows$get_scan_impact()


# Check Impact measures ----
workflows$imp_bet_wor(df_scan_imp)


# Check AFQ gam ----
tract <- "Callosum Temporal"
tract_gams <- workflows$scalar_gams(df_afq, tract, "post")

summary(tract_gams$gam_GSO$FA)
grid::grid.newpage(); grid::grid.draw(tract_gams$gam_plots$FA)
  

# Intx smooths
df <- merge(
  x = df,
  y = df_scan_imp[, c("subj_id", "scan_name", "mem_vis")],
  by = c("subj_id", "scan_name"),
  all.x = T
)
fit_intx <- workflows$gam_intx(df)
plot(fit_intx$gamIntx)
plot(fit_intx$gamIntxOF)


# Check GAM for over sensitivity ----
#
# Replace post of group A with base of group B, subsample
# for group A.
#
# Replace groups
# - seed=1: scanOFpost f=14.19, dev expl=87.9%.
# - seed=2: scanOFpost f=6.015, dev expl=88.3%.
fit_rand <- workflows$gam_rand(df, 2)
workflows$draw_grid(fit_rand$gamGS, fit_rep$gamGSO, 2, 3, 3, tract)
summary(fit_rand$gamGSO)

# TODO identify impact metrics for visit
# TODO fit GAM for global, group, covariate intx
# TODO identify impact subgroups
# TODO relate impact subgroups to tract changes


# TODO support other scalars
df <- df_afq[which(
  df_afq$tract_name == tract &
    df_afq$scan_name %in% c("base", "post")
), ]
descdist(df$dti_ad, discrete = F)
hist(df$dti_ad)
plot(df$dti_ad)
plot(df$node_id, df$dti_ad)
plot(df$node_id, df$dti_ad, col = factor(df$scan_name))


fit_S <- bam(
  dti_ad ~ s(subj_id, scan_name, bs = "re") +
    s(node_id, bs = "tp", k = 40) +
    s(node_id, scan_name, bs = "fs", k = 40),
  data = df,
  family = gaussian(),
  method = "fREML",
  discrete = T
)
gam.check(fit_S)
summary(fit_S)

plot_S <- getViz(fit_S)
p <- plot(sm(plot_S, 3))

