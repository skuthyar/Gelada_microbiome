# Create Figure 6: PCoA of microbial compositional differences by age and season
brayPS <- ordinate(full_gelada_clean, method = "PCoA")

# Age
full_gel_clean_age <- prune_samples(sample_data(full_gelada_clean)$Age_Cat != "Early_Adult", full_gelada_clean)
brayPS_age <- ordinate(full_gel_clean_age, method = "PCoA")
BC_age <- plot_ordination(full_gel_clean_age, brayPS_age, color="Age_Cat") + geom_point(size = 4) + scale_color_manual(values=c("#D02424","#D0CD24"))  
BC_age <- BC_age + theme_bw() + theme(legend.title=element_blank()) + theme(legend.position="bottom") +theme(text = element_text(size = 25))  
BC_age

# Season 
BC_season <- plot_ordination(full_gelada_clean, brayPS, color="Season") + geom_point(size = 4) + scale_color_manual(values=c("tan","forest green"))  
BC_season <- BC_season + theme_bw() + theme(legend.title=element_blank()) + theme(legend.position="bottom") +theme(text = element_text(size = 25))  
BC_season

Figure6 <- ggarrange(BC_age, BC_season)
