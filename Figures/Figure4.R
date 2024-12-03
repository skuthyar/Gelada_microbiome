# Create Figure 4: Cloud plots for visualizing both Shannon diversity and richness using phyloseq estimates
median_age1 <- dabestr::dabest(df1, Age_Cat ,Shannon,
                        idx = c("Adult", "Infant"),
                        paired = FALSE) %>%
  median_diff()

plot_age1<-plot(median_age1, palette = c("#D02424","#D0CD24"), color.column = Age_Cat)
plot_age1

median_age2 <- dabest(df2, Age_Cat ,Richness,
                     idx = c("Adult", "Infant"),
                     paired = FALSE) %>%
  median_diff()

plot_age2<-plot(median_age, palette = c("#D02424","#D0CD24"), color.column = Age_Cat)
plot_age2

Figure4 <- ggarrange(plot_age1, plot_age2)

