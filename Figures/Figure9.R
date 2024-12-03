# Create Figure 9 - faceted boxplot of relative abundances of the taxa that were significant in ANCOM-BC2
psq_rel <- transform_sample_counts(full_gelada_clean_family, function(x) x*100/sum(x)) # relativize the data

psq_rel_ancomfams<- subset_taxa(psq_rel, Family == "Prevotellaceae" |
                                  Family == "Spirochaetaceae" | Family == "Eggerthellaceae" | Family == "Microbacteriaceae" |
                                  Family == "Puniceicoccaceae" | Family == "Clostridium sp. CAG:306" | Family == "vadinBE97" |
                                  Family == "Pirellulaceae" | Family == "Methanobacteriaceae" | Family == "M2PB4-65 termite group"| 
                                  Family == "Bacteroidales UCG-001" |
                                  Family == "Succinivibrionaceae") # select the sig. taxa

plot_abundance <- function(x = psq_rel_ancomfams, # phyloseq data
                           title = "",
                           Facet = "Family", # taxa rank for facets
                           Category = "Season", # categorical features for x axis
                           Color = "Season",
                           legend = "Season",
                           LBLs = c("Label 1", "Label 2", 
                                    "Label 3", "Label 4", "5", "6", "7", "8", "9", "10", "11", "12")
) {
  
  mphyseq <- psmelt(x)
  mphyseq <- subset(mphyseq, Abundance > 0)
  
  ggplot(data = mphyseq, 
         mapping = aes_string(x = Category,
                              y = "Abundance",
                              color = Color, fill = Color)
  ) +
    scale_color_manual(name='Season', values=c("tan", "forestgreen"), labels=c('')) +
    geom_boxplot(fill = NA, outlier.shape=NA) + # avoid double plotting of outliers)
    geom_point(size = 1, alpha = 0.3, 
               position = position_jitter(width = 0.2)) +
    facet_wrap(facets = Facet, ncol = 6) +
    scale_y_log10(labels = scales::number_format(accuracy = 0.1, decimal.mark = '.'), limits = c(0.001, 100)) +
    labs(title = title) +
    theme(legend.position = legend) +
    theme_bw() +
    xlab(" ") + ylab("Relative abundance")
}

p.box <- plot_abundance(psq_rel_ancomfams, Facet="Family")
