age <- ggplot(inf_m, aes(x=AgeYear, y=shannon_infant)) +
  geom_point(alpha = .25, color = "#D0CD24", size = 3) +
  geom_smooth(method = "loess", color = "#D02424") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  xlab("Age in years") + 
  ylab("Shannon diversity index") +
  theme(axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold")) +
  theme(axis.line = element_line(color = "black")) 
age
