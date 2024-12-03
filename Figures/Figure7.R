# Set the prevalence to be 20%
prev <- 0.2 

# Agglomerate to family level and remove taxa that are less than prevalence value
full_gelada_clean_family <- full_gelada_clean%>%
  tax_glom("Family") %>%
  filter_taxa(., function(x) sum(x > 0) > (prev*length(x)), TRUE)

# Exclude uncultured or unidentified taxa and mitochondria
psq = subset_taxa(full_gelada_clean_family, Family !="uncultured rumen bacterium"
                  & Family !="Unassigned Order" & Family !="uncultured Mollicutes bacterium"
                  & Family !="uncultured Clostridiaceae bacterium"
                  & Family !="uncultured bacterium"
                  & Family !="uncultured" & Family !="Unassigned"
                  & Family !="gut metagenome" & Family !="metagenome" & Family != "uncultured Firmicutes bacterium"
                  & Family != "uncultured prokaryote" & Family != "uncultured organism" & Family != "unidentified rumen bacterium RF39"
                  & Family !="Mitochondria")

# Create Figure 7: hierarchical clustering abundance heat map by season
cols <-c("forestgreen", "tan") 
names(cols) <- unique(samdat_tbl(psq)$Season)

set.seed(3)
heat.season <- psq %>%
  tax_transform("clr", rank = "Family") %>%
  comp_heatmap(colors = heat_palette(sym = TRUE), name = "CLR",tax_anno = taxAnnotation(
    Prev. = anno_tax_prev(bar_width = 0.3, size = grid::unit(1, "cm"))
  ),
  sample_anno = sampleAnnotation(
    Season = anno_sample("Season"),
    col = list(Season = cols), border = FALSE
  ))

heat.season 
