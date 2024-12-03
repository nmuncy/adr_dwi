# Set Env ----
#
# TODO Simpson's paradox for FU1
# TODO identify groups of subjs in data
# TODO follow groups across different metrics

library("survival")
library("ranger")
library("ggplot2")
library("dplyr")
library("ggfortify")
library("tidyverse")
library("lubridate")

source(paste0(getwd(), "/visualize.R"))
source(paste0(getwd(), "/transform.R"))
# source(paste0(getwd(), "/stats.R"))


# Load Data ----
data_dir <- "/Users/nmuncy2/Projects/data"
df <- read.csv(paste0(data_dir, "/df_impact_user.csv"))

df$subj_id <- as.factor(df$subj_id)
df$visit_name <- as.factor(df$visit_name)
df$num_tbi <- as.factor(df$num_tbi)
df$visit_date <- as.POSIXct(df$visit_date, format="%Y-%m-%d", tz="UTC")


# Total Symptom Score ----
# 
# Test plotting, stats functions.
df_tsymp <- fu1_better("userTotalSymptomScore", df, low=T)
bet_wor <- better_worse("userTotalSymptomScore", df_tsymp, "base")
visit_track("userTotalSymptomScore", df_tsymp)
# visit_box("userTotalSymptomScore", df_tsymp)
# stats.tsymp <- wc_ranksum("userTotalSymptomScore", df_tsymp, "base")
# stats.tsymp


# Many scores ----
#
# Check data for remaining composites.
col_list <- c(
  "userMemoryCompositeScoreVerbal",
  "userMemoryCompositeScoreVisual",
  "userVisualMotorCompositeScore",
  "userReactionTimeCompositeScore",
  "userImpulseControlCompositeScore"
)
for(col_name in col_list){
  low <- if (col_name == "userReactionTimeCompositeScore") T else F
  df_sub <- fu1_better(col_name, df, low=low)
  print(visit_track(col_name, df_sub))
  # print(visit_box(col_name, df_sub))
}
rm(df_sub,  df_tsymp)


# Corr: Ver Mem ~ Tot Symp ----
df_wide <- comp_group(df)

# Baseline lm
model <- lm(mem_ver.base ~ tot_symp.base, data=df_wide)
summary(model)
# plot(model)

ggplot(
  data=df_wide,
  aes(tot_symp.base, mem_ver.base)
) +
  geom_point() +
  stat_smooth(method=lm) +
  stat_cor(method="pearson", label.y = 50, label.x = 30) +
  labs(title="Baseline: Verbal Memory ~ Total Symptom")

  

# FU1 lm
model <- lm(mem_ver.fu1 ~ tot_symp.fu1, data=df_wide)
summary(model)
ggplot(
  data=df_wide,
  aes(tot_symp.fu1, mem_ver.fu1)
) +
  geom_point() +
  stat_smooth(method=lm) +
  stat_cor(method="pearson", label.x = 50) +
  labs(title="FU1: Verbal Memory ~ Total Symptom")
  

# Change
model <- lm(mem_ver_chg ~ tot_symp_chg, data=df_wide)
model
ggplot(
  data=df_wide,
  aes(tot_symp_chg, mem_ver_chg)
) +
  geom_point() +
  stat_smooth(method=lm) +
  stat_cor(method="pearson", label.x = 35) +
  labs(title="FU1-Baseline: Verbal Memory ~ Total Symptom")

# Fu1 lm by group
model <- anova(lm(mem_ver_chg ~ tot_symp_chg * mem_ver_chg_grp, data=df_wide))
model
ggplot(
  data=df_wide,
  aes(tot_symp_chg, mem_ver_chg, color=mem_ver_chg_grp)
) +
  geom_point() +
  stat_smooth(method=lm) +
  stat_cor(method="pearson") +
  labs(title="FU1-Baseline: Verbal Memory ~ Total Symptom by Memory Group")


model <- anova(lm(mem_ver_chg ~ tot_symp_chg * tot_symp_chg_grp, data=df_wide))
model
ggplot(
  data=df_wide,
  aes(tot_symp_chg, mem_ver_chg, color=tot_symp_chg_grp)
) +
  geom_point() +
  stat_smooth(method=lm) +
  stat_cor(method="pearson") +
  labs(title="FU1-Baseline: Verbal Memory ~ Total Symptom by Symptom Group")


# Corr: Vis Mem ~ Tot Symp ----
df_wide <- comp_group(df)

# Baseline lm
model <- lm(mem_vis.base ~ tot_symp.base, data=df_wide)
summary(model)
# plot(model)

ggplot(
  data=df_wide,
  aes(tot_symp.base, mem_vis.base)
) +
  geom_point() +
  stat_smooth(method=lm) +
  stat_cor(method="pearson", label.y = 50, label.x = 30) +
  labs(title="Baseline: Visual Memory ~ Total Symptom")



# FU1 lm
model <- lm(mem_vis.fu1 ~ tot_symp.fu1, data=df_wide)
summary(model)
ggplot(
  data=df_wide,
  aes(tot_symp.fu1, mem_vis.fu1)
) +
  geom_point() +
  stat_smooth(method=lm) +
  stat_cor(method="pearson", label.x = 50) +
  labs(title="FU1: Visual Memory ~ Total Symptom")


# Change
model <- lm(mem_vis_chg ~ tot_symp_chg, data=df_wide)
model
ggplot(
  data=df_wide,
  aes(tot_symp_chg, mem_vis_chg)
) +
  geom_point() +
  stat_smooth(method=lm) +
  stat_cor(method="pearson", label.x = 35) +
  labs(title="FU1-Baseline: Visual Memory ~ Total Symptom")

# Fu1 change lm by group
model <- anova(lm(mem_vis_chg ~ tot_symp_chg * mem_vis_chg_grp, data=df_wide))
model
ggplot(
  data=df_wide,
  aes(tot_symp_chg, mem_vis_chg, color=mem_ver_chg_grp)
) +
  geom_point() +
  stat_smooth(method=lm) +
  stat_cor(method="pearson") +
  labs(title="FU1-Baseline: Visual Memory ~ Total Symptom by Memory Group")


model <- anova(lm(mem_vis_chg ~ tot_symp_chg * tot_symp_chg_grp, data=df_wide))
model
ggplot(
  data=df_wide,
  aes(tot_symp_chg, mem_vis_chg, color=tot_symp_chg_grp)
) +
  geom_point() +
  stat_smooth(method=lm) +
  stat_cor(method="pearson") +
  labs(title="FU1-Baseline: Visual Memory ~ Total Symptom by Symptom Group")

# Fu1 by group
# FU1 lm
model <- anova(lm(mem_vis.fu1 ~ tot_symp.fu1 * mem_vis_chg_grp, data=df_wide))
model
ggplot(
  data=df_wide,
  aes(tot_symp.fu1, mem_vis.fu1, color=mem_vis_chg_grp)
) +
  geom_point() +
  stat_smooth(method=lm) +
  stat_cor(method="pearson", label.x = 50) +
  labs(title="FU1: Visual Memory ~ Total Symptom by ")


# Survival Analysis ----
#
# TODO
df_comp <- df[, -c(11:32)]
# df_comp <- df_comp[which(! df_comp$num_tbi %in% c(2, 3)),]

# Data cleaning
# TODO resolve multiple FU1 (e.g. 3)
# TODO resolve missing TBI nums (e.g. 3)
# TODO resolve TBI order/num (e.g. 89)
# TODO missing dates (e.g. 9021)
df_comp <- df_comp[-which(df_comp$subj_id == 3),]


# Separate base
idx_base <- which(df_comp$visit_name == "base")
df_base <- df_comp[idx_base, ]
df_comp <- df_comp[-c(idx_base), ]

# Days
df_comp <- df_comp %>%
  group_by(subj_id, num_tbi) %>%
  mutate(
    date = ymd(visit_date),
    days = as.numeric(date - min(date))
  )

# Event (final date)
df_comp <- df_comp %>%
  group_by(subj_id, num_tbi) %>%
  mutate(
    last = last(date)
  )
df_comp <- df_comp %>%
  group_by(subj_id, num_tbi) %>%
  mutate(
    ret_play = if_else(date == last, 1, 0)
    )

# Set tstart and tstop
df_comp$tstop <- df_comp$days + 1
df_comp <- df_comp %>%
  group_by(subj_id, num_tbi) %>%
  mutate(
    tstart = if_else(tstop == 1, 0, lag(tstop))
  )
# df_comp$tstart <- df_comp$tstop - 1
# df_comp$enum <- as.numeric(df_comp$num_tbi) - 1
# df_comp$tstart <- df_comp$days
# df_comp <- df_comp %>%
#   group_by(subj_id, num_tbi) %>%
#   mutate(
#     tstop = lead(df_comp$tstart)
#   )


# Total assessments
# df_comp <- df_comp %>%
#   group_by(subj_id, num_tbi) %>%
#   mutate(
#     tot_impact = last(visit_name)
#   )
# df_comp$tot_impact <- as.numeric(df_comp$tot_impact) - 1 # Not counting baseline



# Add base
imp_names <- c("mem_ver", "mem_vis", "vis_mot", "rx_time", "imp_ctl", "tot_symp")
colnames(df_comp)[5:10] <- imp_names
colnames(df_base)[5:10] <- paste0("base.", imp_names)

df_surv <- merge(
  x=df_comp,
  y=df_base[, c("subj_id", paste0("base.", imp_names))],
  by="subj_id",
  all.x = T
)
  
#
df_surv$ret_play[is.na(df_surv$ret_play)] <- 1
rm_list <- c("visit_name", "num_tbi", "visit_date",  "date", "last")
df_surv <- df_surv[, -which(names(df_surv) %in% rm_list)]
# rm(df_base, df_comp)

#
# df_surv <- df_surv[which(df_surv$enum == 1), ]

# 
stats.surv <- coxph(
  formula = Surv(tstart, tstop, ret_play) ~ tot_symp + base.tot_symp,
  data = df_surv
)
stats.surv
cox_fit <- survfit(stats.surv)
autoplot(cox_fit)

stats.surv <- coxph(
  formula = Surv(tstart, tstop, ret_play) ~ tot_symp + mem_ver + mem_vis +
    vis_mot + rx_time + imp_ctl +
    base.tot_symp + base.mem_ver + base.mem_vis + base.vis_mot + 
    base.rx_time + base.imp_ctl,
  data = df_surv
)
stats.surv
cox_fit <- survfit(stats.surv)
autoplot(cox_fit)

# stats.surv <- coxph(
#   formula = Surv(tstart, tstop, ret_play) ~ mem_ver + base.mem_ver,
#   data = df_surv
# )
# stats.surv
# cox_fit <- survfit(stats.surv)
# autoplot(cox_fit)
# 
# stats.surv <- coxph(
#   formula = Surv(tstart, tstop, ret_play) ~ mem_vis + mem_vis,
#   data = df_surv
# )
# stats.surv
# cox_fit <- survfit(stats.surv)
# autoplot(cox_fit)
# 
# stats.surv <- coxph(
#   formula = Surv(tstart, tstop, ret_play) ~ vis_mot + base.vis_mot,
#   data = df_surv
# )
# stats.surv
# cox_fit <- survfit(stats.surv)
# autoplot(cox_fit)
# 
# stats.surv <- coxph(
#   formula = Surv(tstart, tstop, ret_play) ~ rx_time + base.rx_time,
#   data = df_surv
# )
# stats.surv
# cox_fit <- survfit(stats.surv)
# autoplot(cox_fit)
# 
# stats.surv <- coxph(
#   formula = Surv(tstart, tstop, ret_play) ~ imp_ctl + base.imp_ctl,
#   data = df_surv
# )
# stats.surv
# cox_fit <- survfit(stats.surv)
# autoplot(cox_fit)


autoplot(stats.surv)
