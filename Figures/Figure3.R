# Create Figure 3: Box plots using diversity estimates from phyloseq
df1_age <- subset(df1, df1$Age_Cat != "Early_Adult")
pd = position_jitterdodge(dodge.width = 0.75, jitter.width = 0.3)
Shannon_age <- ggplot(df1_age, aes(Age_Cat, Shannon, fill = Age_Cat)) + 
  geom_boxplot(outlier.shape=NA) + geom_point(aes(color=Age_Cat), position=pd, alpha=0.4, size=3) + scale_fill_manual(values=c("#D02424","#D0CD24")) + scale_color_manual(values=c("#D02424","#D0CD24")) + theme_bw() + theme(axis.title.x=element_blank()) + theme(legend.title=element_blank()) + theme(legend.position ="none") + ylab("Shannon Diversity")+theme(text = element_text(size = 25)) + theme(
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank()
)
Shannon_age

df2_age <- subset(df2, df2$Age_Cat != "Early_Adult")
Richness_age <- ggplot(df2_age, aes(Age_Cat, Richness, fill = Age_Cat)) + 
  geom_boxplot(outlier.shape=NA) + geom_point(aes(color=Age_Cat), position=pd, alpha=0.4, size=3) + scale_fill_manual(values=c("#D02424","#D0CD24")) + scale_color_manual(values=c("#D02424","#D0CD24")) + theme_bw() + theme(axis.title.x=element_blank()) + theme(legend.title=element_blank()) + theme(legend.position ="none") + ylab("Richness")+theme(text = element_text(size = 25)) + theme(
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank()
)
Richness_age

Figure3 <- ggarrange(Shannon_age, Richness_age)
