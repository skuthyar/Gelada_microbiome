# PERMANOVA of Bray-Curtis distances - Table S1
full_gelada_clean_family <- tax_glom(full_gelada_clean, taxrank="Family")
full_gelada_clean_rel <- transform_sample_counts(full_gelada_clean_family, function(x) {x*100/sum(x)}) #relativized 
set.seed(9)
Age_Cat <- sample_data(full_gelada_clean_rel)$Age_Cat
AgeYear <- sample_data(full_gelada_clean_rel)$AgeYear
Season <- sample_data(full_gelada_clean_rel)$Season
Rain30 <- sample_data(full_gelada_clean_rel)$Rain30
Rain30 <- as.numeric(Rain30)
Sex <- sample_data(full_gelada_clean_rel)$Sex

# Keep individual as random effect to control for repeated measurements
permanova2 <- adonis2(phyloseq::distance(full_gelada_clean, method="bray") ~ Age_Cat + AgeYear + Season + Rain30 + Sex, strata=sample_data(full_gelada_clean)$Individual_ID, na.action = na.omit)
permanova2
