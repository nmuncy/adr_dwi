# Get resources ----
library("modules")
library("gridExtra")

workflows <- modules::use("workflows.R")
draw_plots <- modules::use("resources/draw_plots.R")


# Get cleaned data ----
df_afq <- workflows$clean_afq()
df_scan_imp <- workflows$get_scan_impact()


# Check Impact measures ----
workflows$impact_better_worse(df_scan_imp)

# scan_name <- "post"
imp_clust <- workflows$impact_cluster(df_scan_imp, "post")

# PCA stats and plots
print(imp_clust$stats_pc)
print(imp_clust$plot_pc$plot_eig)
print(imp_clust$plot_pc$plot_biplot)

# K-means stats and plots
print(imp_clust$stats_km)
print(imp_clust$plot_km)
draw_plots$draw_impact_pairs(imp_clust$df_sik, c(7:10), 3)


# Check AFQ gam ----
#
# TODO longitudinal GAMs of ordered factors (base, post, rtp) for scalars
# TODO GAMs of scalars for post by k-means group
# TODO above, with impact interactions
tract_list <- unique(df_afq$tract_name)
for(tract in tract_list){
  print(tract)
  tract_gams <- workflows$gams_long(df_afq, tract)
}
# summary(tract_gams$gam_LGSIO$FA)
# grid::grid.newpage(); grid::grid.draw(tract_gams$gam_plots$FA)



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




