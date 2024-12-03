# Using a phyloseq object with more timepoints
season_physeq <- readRDS("season_physeq.rds") 
# Clean unknown ASV names from taxonomy **using Baniel et al 2021 code
tax_table(season_physeq)<-apply(tax_table(season_physeq),2, function(x) (gsub("Unassigned*",NA,x)))
tax_table(season_physeq)<-apply(tax_table(season_physeq),2, function(x) (gsub("uncultured*",NA, x)))
tax_table(season_physeq)<-apply(tax_table(season_physeq),2, function(x) (gsub("unidentified*",NA, x)))
tax_table(season_physeq)<-apply(tax_table(season_physeq),2, function(x) (gsub("gut metagenome",NA, x)))
tax_table(season_physeq)<-apply(tax_table(season_physeq),2, function(x) (gsub("metagenome",NA, x)))
tax_table(season_physeq)[,"Family"] [tax_table(season_physeq)[,"Order"] %in% "WCHB1-41" ]<- "RFP12"

# Tax glom by family 
season_aggregated <- tax_glom(season_physeq, "Family")

# Pull data out of physeq object
season_tax <- as.data.frame(tax_table(season_aggregated))
season_otu <- as.data.frame(otu_table(season_aggregated))
season_m <- as.data.frame((sample_data(season_aggregated)))

# Update ASV rows to reflect appropriate taxonomic names
f_names <- season_tax$Family  

# Plot all families for one individual, F22
# Prep ASV data
biom_otu <- season_otu %>%
  mutate(taxa_id = f_names) %>%
  select(last_col(), everything())

# Prep metadata
season_m <- data.frame((sample_data(season_aggregated)))
season_m$sample <- row.names(season_m) 

biom_m <- season_m %>%
  mutate(SampleDate = mdy(SampleDate)) %>%
  rename(subject = ID) %>%
  rename(collection_date = SampleDate) %>%
  as.data.frame()

paramList <- prepanel(otudata = biom_otu, metadata = biom_m, subj = "F22")

F22_horizon <- horizonplot(paramList,
            aesthetics = horizonaes(title = "Microbiome Horizon Plot", 
            xlabel = "Samples from one wild gelada, F22", 
            ylabel = "Taxa Phyla", 
            legendTitle = "Quartiles Relative to Taxon Median"))
ggsave("FigureS6a.png", age, width=30, height=20)

# Plot one ASV (RFP12) across multiple individuals
biom2_m <- biom_m %>%
  filter(subject %in% c("F19","F22")) %>%
  mutate(subject = as.character(subject)) %>%
  filter(collection_date == "2017-06-06" | collection_date == "2017-12-01" | collection_date =="2018-04-03" | collection_date == "2018-09-06" | collection_date == "2018-11-12") %>%
  select(subject, sample, collection_date) %>%
  arrange(subject, collection_date) %>%
  as.data.frame()

biom2_otu <- biom_otu %>%
  select("taxa_id", "LID_102279", "LID_101908", "LID_103279", "LID_102220", "LID_102831", "LID_102185", "LID_101812", "LID_102068", "LID_102031", "LID_102855")

paramList <- prepanel(otudata = biom2_otu, metadata = biom2_m, singleVarOTU = "RFP12")

multi_horizon <- horizonplot(paramList)

ggsave("FigureS6b.png", multi_horizon, width=20, height=10)
