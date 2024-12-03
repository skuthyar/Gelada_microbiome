# Create Figure 2
age_cat <- ggplot(inf_m_filtered, aes(x=AgeCatMonth, y=shannon_infant, color = AgeCatMonth)) +
  scale_color_manual(values=c("#D02424", "#D05D24", "#d09524", "#D0CD24")) +
  geom_violin() + 
  geom_jitter(width = 0.2, alpha = 0.5, size = 3) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none") +
  ylab("Shannon diversity index") +
  xlab("Age category in months") +
  theme(axis.line = element_line(color = "black")) +
   theme(axis.title.x = element_text(face = "bold"),
        axis.text.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold")) +
  geom_signif(comparisons = list(c("0-3", "3-6")), color = "gray40", size = 0.7, textsize = 7, test = wilcox.test, y_position = 5.5, map_signif_level=c("***"=0.001)) +
  geom_signif(comparisons = list(c("0-3", "6-9")), color = "gray40", size = 0.7, textsize = 7, test = wilcox.test, y_position = 5.8, map_signif_level=c("***"=0.001)) +
  geom_signif(comparisons = list(c("0-3", "9-12")), color = "gray40", size = 0.7, textsize = 7, test = wilcox.test, y_position = 6.1, map_signif_level=c("***"=0.001)) +
  geom_signif(comparisons = list(c("3-6", "9-12")), color = "gray40", size = 0.7, textsize = 7, test = wilcox.test, y_position = 6.4, map_signif_level=c("*"=0.05)) 
age_cat
