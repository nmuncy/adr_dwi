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
imp_gams <- workflows$impact_gams(df_scan_imp)


# Model AFQ metrics via HGAMs ----
#
# Conduct longitudinal HGAM of tract FA differences (LDI), longitudinal
# HGAM of scalars for each tract (LGI, LGIO), and then interactions
# with time and ImPACT measures.

fit_LDI <- workflows$dwi_gam_delta_all(df_afq, make_plots = F)

# Changes in FA resulting from rerunning pyAFQ on ADR base data
df_afq_rr <- workflows$get_data_afq("tbl_afq_rerun")
fit_DI_rr <- workflows$dwi_gam_delta_rerun(df_afq, df_afq_rr)


#
tract_list <- unique(df_afq$tract_name)
for (tract in tract_list) {
  tract_gams <- workflows$dwi_gam_long_tract(df_afq, tract)
}

for(tract in tract_list){
  workflows$dwi_gam_delta_time(df_afq, tract)
}

# Select specific tracts: CCorb, laThal, lCCs, 
#  liFO, lArc, raThal, rCCing, riFO, rUnc.
tract_roi <- c(
  tract_list[20], tract_list[1], tract_list[5], tract_list[7], tract_list[13],
  tract_list[3], tract_list[4], tract_list[8], tract_list[16]
)
for(tract in tract_roi){
  fit_intx <- workflows$dwi_gam_long_impact(df_afq, df_scan_imp, tract)
}

# Only test for CC and vis_mem interactions
tract_roi <- tract_list[17:24]
for(tract in tract_roi){
  fit_intx <- workflows$dwi_gam_long_impact(
    df_afq, df_scan_imp, tract, impact_meas="mem_vis"
  )
}
