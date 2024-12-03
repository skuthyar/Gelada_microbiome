# NMDS of microbial compositional differences by age and season
# Age
full_gel_clean_age <- prune_samples(sample_data(full_gelada_clean)$Age_Cat != "Early_Adult", full_gelada_clean)
brayPS <- ordinate(full_gel_clean_age, method = "NMDS")
BC_age <- plot_ordination(full_gel_clean_age, brayPS, justDF = TRUE)
BCPlot_age <- ggplot(data = BC_age, aes(x = NMDS1, y = NMDS2, color = Age_Cat)) +
  geom_point(size = 4) + scale_color_manual(values=c("#D02424","#D0CD24"))+ theme(axis.text = element_text(size=25))
BCPlot_age <- BCPlot_age + theme_bw() + theme(legend.title=element_blank()) + theme(legend.position="bottom") 
BCPlot_age

# Season
brayPS_season <- ordinate(full_gelada_clean, method = "NMDS")
BC_season <- plot_ordination(full_gelada_clean, brayPS_season, justDF = TRUE)
BCPlot_season <- ggplot(data = BC_season, aes(x = NMDS1, y = NMDS2, color = Season)) +
  geom_point(size = 4) + scale_color_manual(values=c("tan","forest green")) + theme(axis.text = element_text(size=25))
BCPlot_season <- BCPlot_season + theme_bw() + theme(legend.title=element_blank()) + theme(legend.position="bottom") 
BCPlot_season

NMDS_supplementary <- ggarrange(BCPlot_age, BCPlot_season)
