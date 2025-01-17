# Get resources ----
library("modules")
# library("gridExtra")

workflows <- modules::use("workflows.R")
draw_plots <- modules::use("resources/draw_plots.R")


# Get cleaned data ----
df_afq <- workflows$clean_afq()
df_scan_imp <- workflows$get_scan_impact()


# Check Impact measures ----
#
# Show composite distributions, subject-level responses, and clustering.
imp_smooth <- draw_plots$draw_impact_smooths(df_scan_imp)
print(imp_smooth)

# Subject-level responses, cluster for post visit.
workflows$impact_better_worse(df_scan_imp)
imp_clust <- workflows$impact_cluster(df_scan_imp, "post")

# PCA stats and plots
print(imp_clust$stats_pc)
print(imp_clust$plot_pc$plot_eig)
print(imp_clust$plot_pc$plot_biplot)

# K-means stats and plots
print(imp_clust$stats_km)
print(imp_clust$plot_km)
draw_plots$draw_impact_pairs(imp_clust$df_sik, c(7:10), 3)


# Model AFQ metrics via HGAMs ----
#
# Conduct longitudinal HGAM of tract FA differences (LDI), longitudinal
# HGAM of scalars for each tract (LGI, LGIO), and then interactions
# with impact measures.
#
# TODO GAMs of scalars for post by k-means group?

# Fit FA differences between scan times for all tracts
fit_LDI <- workflows$gam_delta_long_all(df_afq)


# Fit LGAM (LGI, LGIO) of individual tracts for FA, MD, RD, AD scalars
tract_list <- unique(df_afq$tract_name)
for (tract in tract_list) {
  tract_gams <- workflows$gams_long_tract(df_afq, tract)
}
# summary(tract_gams$gam_LGIO$FA)
# grid::grid.newpage(); grid::grid.draw(tract_gams$gam_plots$FA)


# Fit interaction smooths between tract and Impact measures
tract_roi <- c(
  tract_list[20], tract_list[1], tract_list[7], tract_list[2],
  tract_list[4], tract_list[12], tract_list[16]
)
tract <- tract_roi[3]
fit_intx <- workflows$gams_long_tract_intx(df_afq, df_scan_imp, tract)


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
