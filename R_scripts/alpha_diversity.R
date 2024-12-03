## Divnet and breakaway
# We exported  and viewed the ASV table in Excel to determine the "base" ASV, or the most common ASV: 5f71804aa6eb0691c68a33d1811fd654 or Veillonella. 

# Aggregate phyloseq object to bacterial family level using tax_glom
full_gelada_clean_family <- tax_glom(full_gelada_clean, taxrank="Family")
# Run DivNet
divnet_full_gelada_clean_family <- full_gelada_clean_family %>%
  divnet(formula= ~ Individual_ID, base="5f71804aa6eb0691c68a33d1811fd654", ncores = 8)

metafam <- full_gelada_clean_family %>%
  sample_data %>%
  as_tibble %>%
  mutate("sample_names" = full_gelada_clean_family %>% sample_names )

shannonfam <- metafam %>%
  dplyr::left_join(divnet_full_gelada_clean_family$shannon %>% summary,
            by = "sample_names")

# Filter out the one sample that is "Early_Adult" 
shannonfam_final <- filter(shannonfam, Age_Cat != "Early_Adult")

# Does age (categorical) predict Shannon diversity of gelada samples?
# Dry season
shannonfam_final_dry <- subset(shannonfam_final, Season=="Dry")
Age_cat_dry <- betta(formula = estimate ~ Age_Cat, ses = error,  data = shannonfam_final_dry)
Age_cat_dry$table
Age_cat_dry$r_squared_wls

# Wet season
shannonfam_final_wet <- subset(shannonfam_final, Season=="Wet")
Age_cat_wet <- betta(formula = estimate ~ Age_Cat, ses = error,  data = shannonfam_final_wet)
Age_cat_wet$table
Age_cat_wet$r_squared_wls

## Estimation of diversity through phyloseq  
# All geladas together at the ASV level
# Estimate Shannon diversity
sample_data(full_gelada_clean)$Shannon <- estimate_richness(full_gelada_clean, measure=c("Shannon"))$Shannon
df1 <- data.frame(sample_data(full_gelada_clean))
# Estimate richness
sample_data(full_gelada_clean)$Richness <- estimate_richness(full_gelada_clean, measure=c("Observed"))$Observed
df2 <- data.frame(sample_data(full_gelada_clean))

# Adults only
# Filter out infants
adult_subset <- prune_samples(sample_data(full_gelada_clean)$Age_Cat != "Infant", full_gelada_clean)
# Estimate Shannon diversity for adults
shannon_adult <- estimate_richness(adult_subset, measures = "Shannon")
# Extract the Shannon diversity values
shannon_adult<- shannon_adult$Shannon
# Add Shannon diversity as a column in the metadata
ad_m <- as.data.frame(as_tibble(sample_data(adult_subset)))
ad_m$shannon_adult <- shannon_adult

# Infants only
# Filter out adult females
inf_subset <- prune_samples(sample_data(full_gelada_clean)$Age_Cat != "Adult", full_gelada_clean)
# Estimate Shannon diversity for infants
shannon_infant <- estimate_richness(inf_subset, measures = "Shannon")
# Extract the Shannon diversity values
shannon_infant<- shannon_infant$Shannon
# Add Shannon diversity as a column in the metadata
inf_m <- as.data.frame(as_tibble(sample_data(inf_subset)))
inf_m$shannon_infant <- shannon_infant

# Filter data for one year and under
inf_m_filtered <- inf_m %>%
  filter(AgeYear <= 1)

# Shapiro-Wilks test to test for normality
shapiro.test(ad_m$shannon_adult)
shapiro.test(inf_m_filtered$shannon_infant)

# Kruskal-Wallis test since p-value<0.001 for both cases
kruskal.test(shannon_adult ~ AgeYear, data = ad_m)
kruskal.test(shannon_infant ~ AgeCatMonth, data = inf_m_filtered)

# Pairwise group comparison
dunn.test(ad_m$shannon_adult, ad_m$AgeYear, method = "bonferroni")
dunn.test(inf_m_filtered$shannon_infant, inf_m_filtered$AgeCatMonth, method = "bonferroni")

