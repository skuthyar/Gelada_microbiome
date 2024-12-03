# Create Figure 8 - ANCOM-BC2 plot
season = ggplot(data = tab_lfc1_strict, 
                aes(x = Family, y = Dry.Wet, fill = direct, color = direct)) + 
  geom_bar(stat = "identity", colour=NA, width = 0.7, 
           position = position_dodge(width = 0.4)) +
  geom_errorbar(aes(ymin = Dry.Wet - SE, ymax = Dry.Wet + SE), width = 0.2,
                position = position_dodge(0.05), color = "black") + 
  labs(x = NULL, y = "Log fold change",
       title = "Log-fold change of microbial families in wet season compared to dry") + 
  scale_fill_manual(values=c("#167027","#729679")) + 
  theme_minimal() + theme(legend.title=element_blank()) + 
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(angle = 60, hjust = 1)) + theme(text = element_text(size = 18))
season
