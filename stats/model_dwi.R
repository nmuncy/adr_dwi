# Get Resources ----
library("modules")

workflows <- modules::use("workflows.R")

df_afq_imp <- workflows$get_afq_impact()

# 
# TODO Get afq, impact data
# TODO identify impact metrics for visit
# TODO fit GAM for global, group, covariate intx
# TODO identify impact subgroups
# TODO relate impact subgroups to tract changes
