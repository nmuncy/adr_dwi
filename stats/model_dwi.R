# Get resources ----
library("modules")

workflows <- modules::use("workflows.R")


# Get cleaned data ----
df_afq <- workflows$clean_afq()
df_scan_imp <- workflows$get_scan_impact()


# Check Impact measures ----
workflows$imp_bet_wor(df_scan_imp)




# Check AFQ gam ----
#
# scanOFpost f=15.63, dev expl=87.2%.
tract <- "Callosum Superior Parietal"

# Group smooths
df <- df_afq[which(
  df_afq$tract_name == tract & 
    df_afq$scan_name %in% c("base", "post")),
]
fit_spar <- workflows$gam_spar(df)
workflows$draw_grid(fit_spar$gamGS, fit_spar$gamGSO, tract)
summary(fit_spar$gamGSO)

# Intx smooths


# Check GAM for over sensitivity ----
#
# Switch groups
# - seed=1: scanOFpost f=5.604, dev expl=87.2%.
# - seed=2: scanofPost f=4.135, dev expl=87.1%.
#     Effects are tempered, could still be driven
#     by unswapped group.
#
# Replace groups
# - seed=1: scanOFpost f=12.4, dev expl=85.3%.
# - seed=2: scanOFpost f=14.04, dev expl=85.8%.

# Switch groups
df <- df_afq[which(
  df_afq$tract_name == tract & 
    df_afq$scan_name %in% c("base", "post")),
]
subj_list <- unique(df$subj_id)
set.seed(2)
subj_list <- sample(subj_list)
grp_swap <- subj_list[1:33]
idx_base <- which(df$scan_name == "base" & df$subj_id %in% grp_swap)
idx_post <- which(df$scan_name == "post" & df$subj_id %in% grp_swap)
df[idx_base, ]$scan_name <- "post"
df[idx_post, ]$scan_name <- "base"

fit_swt <- workflows$gam_spar(df)
workflows$draw_grid(fit_swt$gamGS, fit_swt$gamGSO, tract)
summary(fit_swt$gamGS)
summary(fit_swt$gamGSO)

# Replace post of group A with base of group B, subsample
# for group A.
df <- df_afq[which(
  df_afq$tract_name == tract & 
    df_afq$scan_name %in% c("base", "post")),
]
subj_list <- unique(df[which(df$scan_name == "post"), ]$subj_id)
df <- df[which(df$subj_id %in% subj_list), ]
set.seed(2)
subj_list <- sample(subj_list)
grp_a <- subj_list[1:32]
grp_b <- subj_list[33:65]

idx_a_post <- which(df$scan_name == "post" & df$subj_id %in% grp_a)
idx_b_base <- which(df$scan_name == "base" & df$subj_id %in% grp_b)
df[idx_a_post,]$dti_fa <- df[idx_b_base, ]$dti_fa

fit_rep <- workflows$gam_spar(df)
workflows$draw_grid(fit_rep$gamGS, fit_rep$gamGSO, tract)
summary(fit_rep$gamGS)
summary(fit_rep$gamGSO)

# TODO identify impact metrics for visit
# TODO fit GAM for global, group, covariate intx
# TODO identify impact subgroups
# TODO relate impact subgroups to tract changes
