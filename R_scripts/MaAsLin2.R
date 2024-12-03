# Clean unknown ASV names from taxonomy 
tax_table(full_gelada_clean)<-apply(tax_table(full_gelada_clean),2, function(x) (gsub("Unassigned*",NA,x)))
tax_table(full_gelada_clean)<-apply(tax_table(full_gelada_clean),2, function(x) (gsub("uncultured*",NA, x)))
tax_table(full_gelada_clean)<-apply(tax_table(full_gelada_clean),2, function(x) (gsub("unidentified*",NA, x)))
tax_table(full_gelada_clean)<-apply(tax_table(full_gelada_clean),2, function(x) (gsub("gut metagenome",NA, x)))
tax_table(full_gelada_clean)<-apply(tax_table(full_gelada_clean),2, function(x) (gsub("metagenome",NA, x)))
tax_table(full_gelada_clean)[,"Family"] [tax_table(full_gelada_clean)[,"Order"] %in% "WCHB1-41" ]<- "RFP12"

# Agglomerate taxonomic levels by family 
season_aggregated <- tax_glom(full_gelada_clean, "Family")

# Pull data out of phyloseq object
season_tax <- as.data.frame(tax_table(season_aggregated))
season_otu <- as.data.frame(otu_table(season_aggregated))
season_m <- as.data.frame(sample_data(season_aggregated))

# Update ASV rows to reflect appropriate taxonomic names
f_names <- season_tax$Family  

maaslin_otu <- season_otu %>%
  mutate(taxa_id = f_names) %>%
  rownames_to_column(var = "OldRowNames") %>%
  select(-OldRowNames) %>%
  column_to_rownames(var = "taxa_id") %>%
  t()  %>% #transpose data
  as.data.frame()

# Make row names sample names
maaslin_m <- season_m
maaslin_m <- data.frame(maaslin_m)

# Run Maaslin for season 
Maaslin_fit = Maaslin2(
    input_data = maaslin_otu, 
    input_metadata = maaslin_m, 
    output = "maaslin2_results", 
    fixed_effects = c("Rain30"),
    random_effects = c("Individual_ID"))
