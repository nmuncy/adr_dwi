# Get Resources ----
library("modules")

workflows <- modules::use("workflows.R")


# Get cleaned data ----
df_afq <- workflows$clean_afq()
df_scan_imp <- workflows$get_scan_impact()


# TODO Get afq, impact data
# TODO identify impact metrics for visit
# TODO fit GAM for global, group, covariate intx
# TODO identify impact subgroups
# TODO relate impact subgroups to tract changes
