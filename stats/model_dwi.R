library("modules")
library("sessioninfo")
workflows <- modules::use("workflows.R")


# Intro ----
# Simulate hypotheses
workflows$hyp_figure()


# Methods 2.1: Demographics ----
# Pull data
df_afq <- workflows$get_data_afq("tbl_afq")
df_scan_imp <- workflows$get_data_scan_impact(df_afq)
demos_count <- workflows$get_demo_counts(df_afq, df_scan_imp)


# Results 3.1: ImPACT smooths ----
imp_desc <- workflows$beh_desc_impact(df_scan_imp)
imp_gams <- workflows$beh_gam_impact(df_scan_imp)


# Results 3.2: Model AFQ metrics via delta HGAMs ----
#
# Conduct whole-brain longitudinal HGAM of tract FA differences (LDI),
# and then model run-rerun tract FA differences (DI) to identify tracts
# that have stable run-rerun metrics.
fit_LDI <- workflows$dwi_gam_delta_all(df_afq, make_plots = F)

df_afq_rr <- workflows$get_data_afq("tbl_afq_rerun")
fit_DI_rr <- workflows$dwi_gam_delta_rerun(df_afq, df_afq_rr)
workflows$plot_dwi_gam_all_rerun(fit_LDI, fit_DI_rr)


# Results 3.3: Tract scalars ----
#
# Model AFQ tract scalars via longitudinal HGAMs to determine source
# of FA change (AD vs RD).
#
# Model CCsf, CCsp, CCmot, CCorb, lArc, lCS, raThal, rCCing, rIFO, rUNC
tract_all <- unique(df_afq$tract_name)
tract_select <- c(
  tract_all[22], tract_all[23], tract_all[18], tract_all[20],
  tract_all[13], tract_all[5], tract_all[2],
  tract_all[4], tract_all[8], tract_all[16]
)
for (tract in tract_select) {
  workflows$dwi_gam_long_tract(df_afq, tract)
}


# Results 3.4: Select tracts and ImPACT interactions ----
imp_list <- names(df_scan_imp[7:12])
for (tract in tract_select) {
  for (imp in imp_list) {
    workflows$dwi_gam_long_impact(
      df_afq, df_scan_imp, tract,
      impact_meas = imp
    )
  }
}


# Results 3.5: FA changes and time -----
for (tract in tract_select) {
  workflows$dwi_gam_delta_time(df_afq, tract)
}
