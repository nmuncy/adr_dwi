# Get resources and data ----
library("modules")
workflows <- modules::use("workflows.R")

df_afq <- workflows$get_data_afq("tbl_afq")
df_scan_imp <- workflows$get_data_scan_impact(df_afq)


# Determine demographics and assess Impact measures ----
#
# Derive general demographics and determine number of participants
# with scan and ImPACT data for each session.
#
# Then model ImPACT composite and total symptoms measures.

demos_count <- workflows$get_demo_counts(df_afq, df_scan_imp)
imp_gams <- workflows$beh_gam_impact(df_scan_imp)


# Model AFQ metrics via delta HGAMs ----
#
# Conduct whole-brain longitudinal HGAM of tract FA differences (LDI),
# and then model run-rerun tract FA differences (DI) to identify tracts
# that have stable run-reun metrics.

fit_LDI <- workflows$dwi_gam_delta_all(df_afq, make_plots = F)

df_afq_rr <- workflows$get_data_afq("tbl_afq_rerun")
fit_DI_rr <- workflows$dwi_gam_delta_rerun(df_afq, df_afq_rr)
workflows$plot_dwi_gam_all_rerun(fit_LDI, fit_DI_rr)


# Model AFQ tract scalars via longitudinal HGAMs. ----
#
# Conduct longitudinal HGAM of tract scalars (LGI, LGIO). Helps determine
# if FA is changed due to AD or RD.
#
# Tracts with no run-reun difference: CCaf, CCmot, CCocc, CCsp, lArc, lCS, lSL
# (Other tracts of interest: laThal, lIFO, raThal, rCCing, riFO, rUnc, CCpp)
# Select tracts: lArc, lCS, raThal, rCCing, rIFO, rUNC

# Use selected and callosal tracts
tract_all <- unique(df_afq$tract_name)
tract_select <- c(
  tract_all[13], tract_all[5], tract_all[2], tract_all[4],
  tract_all[8], tract_all[16]
)
tract_cc <- tract_all[17:24]

# Plot tracts changes in scalars
for (tract in c(tract_cc, tract_select)) {
  workflows$dwi_gam_long_tract(df_afq, tract)
}


# Model interactions between AFQ metrics and ImPACT, time. ----
#
# Select tracts that had stable metrics between run and rerun, and select
# other tracts of interest. Test their interactions with planned ImPACT
# metrics.
#
# Also model FA changes between Post and RTP as a function of number of days
# as an attempt to quantify recovery (or worsening of injury).

# Exploratory analyses - callosal tracts and ImPACT
imp_list <- names(df_scan_imp[7:12])
for (tract in tract_cc) {
  for (imp in imp_list){
    workflows$dwi_gam_long_impact(
      df_afq, df_scan_imp, tract, impact_meas = imp
    )
  }
}

# Exploratory analyses - selected tracts and ImPACT
for (tract in tract_select) {
  for (imp in imp_list){
    workflows$dwi_gam_long_impact(
      df_afq, df_scan_imp, tract, impact_meas = imp
    )
  }
}

# Test whether longer Post-RTP intervals associate with larger scalar changes
for (tract in c(tract_cc, tract_select)){
  workflows$dwi_gam_delta_time(df_afq, tract)
}
