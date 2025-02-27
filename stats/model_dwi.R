# Get resources and data ----
library("modules")
workflows <- modules::use("workflows.R")
draw_plots <- modules::use("resources/draw_plots.R")

df_afq <- workflows$clean_afq("tbl_afq")
df_scan_imp <- workflows$get_scan_impact()


# Determine demographics and assess Impact measures ----
#
# Derive general demographics and determine number of participants
# with scan and ImPACT data for each session.
#
# Then model ImPACT composite and total symptoms measures.

demos <- workflows$basic_demographics()
sess_count <- workflows$prisma_values(df_afq, df_scan_imp)
imp_gams <- workflows$impact_gams(df_scan_imp)



# Model AFQ metrics via HGAMs ----
#
# Conduct longitudinal HGAM of tract FA differences (LDI), longitudinal
# HGAM of scalars for each tract (LGI, LGIO), and then interactions
# with impact measures.
#
# TODO GAMs of scalars for post by k-means group?

# Fit FA differences between scan times for all tracts
fit_LDI <- workflows$gam_delta_long_all(df_afq, make_plots = F)


# Fit LGAM (LGI, LGIO) of individual tracts for FA, MD, RD, AD scalars
tract_list <- unique(df_afq$tract_name)
for (tract in tract_list) {
  tract_gams <- workflows$gams_long_tract(df_afq, tract)
}



# Account for number of days between Post and RTP ----
for(tract in tract_list){
  workflows$gam_delta_tract_time(df_afq, tract)
}



# Model AFQ metrics interactions with IMPACT ----

# Fit interaction smooths between tract and Impact measures
# CCorb, laThal, lCCs, liFO, lArc, raThal, rCCing, riFO, rUnc.
tract_roi <- c(
  tract_list[20], tract_list[1], tract_list[5], tract_list[7], tract_list[13],
  tract_list[3], tract_list[4], tract_list[8], tract_list[16]
)
for(tract in tract_roi){
  fit_intx <- workflows$gams_long_tract_intx(df_afq, df_scan_imp, tract)
}

