# Set Up Env ----
library("modules")
library("mgcv")
library("gratia")
library("mgcViz")
library("visreg")
library("gridExtra")
library("ggpubr")

workflows <- modules::use("workflows.R")
draw_plots <- modules::use("resources/draw_plots.R")


#' Return project analysis directory.
#'
#' @returns Path to NRDStor project analysis directory for
#' Gimli (linux) and Frodo (mac) workstations.
.analysis_dir <- function() {
  if (Sys.info()["sysname"] == "Linux") {
    an_dir <- "/run/user/1001/gvfs/smb-share:server=nrdstor.unl.edu,share=nrdstor/muncylab/nmuncy2/ADR/analyses/stats_gams/slides/"
  } else if (Sys.info()["sysname"] == "Darwin") {
    an_dir <- "/Volumes/nrdstor/muncylab/nmuncy2/ADR/analyses/stats_gams/slides/"
  }
  return(an_dir)
}


# Get Data ----
df_afq <- workflows$clean_afq()
tract <- "Callosum Orbital"
df <- df_afq[which(df_afq$tract_name == tract), ]
colnames(df)[2] <- "visit"


# Non-linear data ----
p_dist <- ggplot(data = df, aes(x = node_id, y = dti_fa)) +
  geom_point(alpha = 0.05) +
  scale_x_continuous(breaks = c(seq(10, 89, by = 10), 89)) +
  ggtitle(paste(tract, "Profile")) +
  ylab("FA Value") +
  xlab("Node ID") +
  theme(
    text = element_text(family = "Times New Roman", size = 14),
    plot.title = element_text(hjust = 0.5)
  )
ggsave(
  paste0(.analysis_dir(), "plot_dist.png"), plot = p_dist, dpi = 600
)


p_dist_lin <- ggplot(
  data = df, 
  aes(x = node_id, y = dti_fa)
) +
  geom_point(alpha = 0.05) +
  geom_smooth(method = "lm") +
  scale_x_continuous(breaks = c(seq(10, 89, by = 10), 89)) +
  ggtitle(paste(tract, "Profile: Linear Fit")) +
  ylab("FA Value") +
  xlab("Node ID") +
  theme(
    text = element_text(family = "Times New Roman", size = 14),
    plot.title = element_text(hjust = 0.5)
  )
ggsave(
  paste0(.analysis_dir(), "plot_dist_lin.png"), plot = p_dist_lin, dpi = 600
)


# Gams ----
p_dist_gam <- ggplot(
  data = df, 
  aes(x = node_id, y = dti_fa)
) +
  geom_point(alpha = 0.05) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "tp", k = 15)) +
  scale_x_continuous(breaks = c(seq(10, 89, by = 10), 89)) +
  ggtitle(paste(tract, "Profile: GAM Smooth")) +
  ylab("FA Value") +
  xlab("Node ID") +
  theme(
    text = element_text(family = "Times New Roman", size = 14),
    plot.title = element_text(hjust = 0.5)
  )
ggsave(
  paste0(.analysis_dir(), "plot_dist_gam.png"), plot = p_dist_gam, dpi = 600
)


fit_G <- gam(
  dti_fa ~ s(node_id, bs = "tp", k = 15),
  data = df,
  family = betar(link = "logit"),
  method = "fREML"
)
p_gam_basis <- draw(basis(fit_G))
p_gam_basis +
  ggtitle(paste(tract, "Profile: Basis Functions")) +
  scale_x_continuous(breaks = c(seq(10, 89, by = 10), 89)) +
  ylab("Weight") +
  xlab("Node ID") +
  theme(
    text = element_text(family = "Times New Roman", size = 14),
    plot.title = element_text(hjust = 0.5)
  )
ggsave(
  paste0(.analysis_dir(), "plot_dist_gam_basis.png"), plot = p_gam_basis, dpi = 600
)


# Multiple groups in data ----
p_dist_group <- ggplot(
  data = df, 
  aes(x = node_id, y = dti_fa, color = visit)
) +
  geom_point(alpha = 0.15) +
  scale_x_continuous(breaks = c(seq(10, 89, by = 10), 89)) +
  ggtitle(paste(tract, "Profile: Visit")) +
  ylab("FA Value") +
  xlab("Node ID") +
  theme(text = element_text(
    family = "Times New Roman", size = 14
  )) + 
  theme(
    text = element_text(family = "Times New Roman", size = 14),
    plot.title = element_text(hjust = 0.5),
    legend.position = c(0.85, 0.85),
    legend.text = element_text(size = 10)
  ) + 
  guides(colour = guide_legend(override.aes = list(alpha = 1)))
ggsave(
  paste0(.analysis_dir(), "plot_dist_group.png"), plot = p_dist_group, dpi = 600
)

p_dist_group_smooth <- ggplot(
  data = df, 
  aes(x = node_id, y = dti_fa, color = visit)
) +
  geom_point(alpha = 0.05) +
  scale_x_continuous(breaks = c(seq(10, 89, by = 10), 89)) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "tp", k = 40),se=F) +
  ggtitle(paste(tract, "Profile: Visit Smooths")) +
  ylab("FA Value") +
  xlab("Node ID") +
  theme(text = element_text(
    family = "Times New Roman", size = 14
  )) + 
  theme(
    text = element_text(family = "Times New Roman", size = 14),
    plot.title = element_text(hjust = 0.5),
    legend.position = c(0.85, 0.85),
    legend.text = element_text(size = 10)
  ) 
ggsave(
  paste0(.analysis_dir(), "plot_dist_group_smooth.png"), plot = p_dist_group_smooth, dpi = 600
)


# Smooth by group ----
fit_GI <- gam(
  dti_fa ~ s(subj_id, bs = "re") + 
    s(node_id, by = visit, bs = "tp", k = 40),
  data = df,
  family = betar(link = "logit"),
  method = "fREML"
)
# gam.check(fit_GI)
# summary(fit_GI)
# plot(fit_GI)

df_pred <- predict.bam(
  fit_GI,
  exclude_terms = c("subj_id"),
  se.fit = T,
  type = "response"
)
df_pred <- data.frame(
  visit = df$visit,
  subj_id = df$subj_id,
  node_id = df$node_id,
  fit = df_pred$fit,
  se.fit = df_pred$se.fit
)
p_group_gam <- ggplot(data = df_pred) +
  geom_smooth(mapping = aes(x = node_id, y = fit, color = visit)) +
  scale_x_continuous(breaks = c(seq(10, 89, by = 10), 89)) +
  ggtitle(paste(tract, "Profile: Visit Smooths")) +
  ylab("Est. FA Fit") +
  xlab("Tract Node") +
  theme(
    text = element_text(family = "Times New Roman", size = 14),
    plot.title = element_text(hjust = 0.5),
    legend.position = c(0.85, 0.85),
    legend.text = element_text(size = 10)
  ) 
ggsave(
  paste0(.analysis_dir(), "plot_group_gam.png"), plot = p_group_gam, dpi = 600
)


# Global, group smooths ----
fit_GS <- bam(
  dti_fa ~ s(subj_id, visit, bs = "re") + 
    s(node_id, bs = "tp", k = 40) +
    s(node_id, by = visit, bs = "tp", k = 40),
  data = df,
  family = betar(link = "logit"),
  method = "fREML"
)
plot_GS <- getViz(fit_GS)

# 
p <- plot(sm(plot_GS, 2))
p_data <- p$data$fit
colnames(p_data) <- c("nodeID", "est", "ty", "se")
p_data$lb <- as.numeric(p_data$est - (1.96 * p_data$se))
p_data$ub <- as.numeric(p_data$est + (1.96 * p_data$se))
p_G <- ggplot(data = p_data, aes(x = .data$nodeID, y = .data$est)) +
  geom_line() +
  geom_ribbon(aes(ymin = .data$lb, ymax = .data$ub), alpha = 0.2) +
  scale_x_continuous(breaks = c(seq(10, 89, by = 10), 89)) +
  theme(
    text = element_text(family = "Times New Roman"),
    plot.title = element_text(hjust = 0.5),
    axis.title.y = element_blank(),
    axis.title.x = element_blank()
  )

#
p <- plot(sm(plot_GS, 3))
p_data <- p$data$fit
p_data$group <- "base"

p <- plot(sm(plot_GS, 4))
h_data <- p$data$fit
h_data$group <- "post"
p_data <- rbind(p_data, h_data)

p <- plot(sm(plot_GS, 5))
h_data <- p$data$fit
h_data$group <- "rtp"
p_data <- rbind(p_data, h_data)
colnames(p_data) <- c("nodeID", "est", "ty", "se", "group")
p_data$CI <- as.numeric(1.96 * p_data$se)

p_S <- ggplot(data = p_data, aes(.data$nodeID)) +
  geom_line(aes(y = .data$est, color = .data$group)) +
  geom_ribbon(
    aes(
      ymin = .data$est - .data$CI, 
      ymax = .data$est + .data$CI, 
      fill = group), 
    alpha = 0.2,
    show.legend = F) +
  scale_y_continuous() +
  scale_x_continuous(breaks = c(seq(10, 89, by = 10), 89)) +
  scale_color_discrete(name = "") + 
  theme(
    text = element_text(family = "Times New Roman"),
    plot.title = element_text(hjust = 0.5),
    legend.position = c(0.88, 0.85),
    legend.text = element_text(size = 10),
    axis.title.y = element_blank(),
    axis.title.x = element_blank()
  )


top <- text_grob(paste(tract, "Profile: HGAM Smooths"), size = 12, family = "Times New Roman")
l1 <- text_grob("Est. Global Fit", size = 12, family = "Times New Roman", rot = 90)
l2 <- text_grob("Est. Visit Fit", size = 12, family = "Times New Roman", rot = 90)
bot <- text_grob("Node ID", size = 10, family = "Times New Roman")

grDevices::png(
  filename = paste0(.analysis_dir(), "plot_global_group_smooths.png"),
  units = "in",
  height = 8,
  width = 6,
  res = 600
)
grid.arrange(
  arrangeGrob(p_G, top = top, left = l1),
  arrangeGrob(p_S, left = l2, bottom = bot),
  nrow = 2,
  ncol = 1
)
grDevices::dev.off()



# Global, group difference smooths ----
df$visitOF <- factor(df$visit, ordered = T)
fit_GSO <- bam(
  dti_fa ~ s(subj_id, visit, bs = "re") + 
    s(node_id, bs = "tp", k = 40) +
    s(node_id, by = visitOF, bs = "tp", k = 40),
  data = df,
  family = betar(link = "logit"),
  method = "fREML"
)
plot_GSO <- getViz(fit_GSO)
p_Da <- draw_plots$draw_gios_diff_sig(plot_GSO, 2, 3)
p_Db <- draw_plots$draw_gios_diff_sig(plot_GSO, 2, 4)
top <- text_grob(paste(tract, "Profile: HGAM Smooths"), size = 12, family = "Times New Roman")
l1 <- text_grob("Est. Global Fit", size = 12, family = "Times New Roman", rot = 90)
l2 <- text_grob("Visit Diff: Post-Base", size = 12, family = "Times New Roman", rot = 90)
l3 <- text_grob("Visit Diff: RTP-Base", size = 12, family = "Times New Roman", rot = 90)
bot <- text_grob("Node ID", size = 10, family = "Times New Roman")

grDevices::png(
  filename = paste0(.analysis_dir(), "plot_global_groupOF_smooths.png"),
  units = "in",
  height = 8,
  width = 6,
  res = 600
)
grid.arrange(
  arrangeGrob(p_G, top = top, left = l1),
  arrangeGrob(p_Da$diff, left = l2),
  arrangeGrob(p_Db$diff, left = l3, bottom = bot),
  nrow = 3,
  ncol = 1
)
grDevices::dev.off()
