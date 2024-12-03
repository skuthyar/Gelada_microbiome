## Redundancy analysis of bacterial abundances
# RDA (values for Tables S2 & S3)
full_gelada_clean_family_clr <- microbiome::transform(full_gelada_clean_family, transform = "clr")
summary(sample_sums(full_gelada_clean_family_clr)) # should look clr-transformed

clr_py <- full_gelada_clean_family_clr

main_vars <- c("Age_Cat", "AgeYear", "Sex", "Season", "Rain30") #"Individual_ID"

clr_py_sub <- subset_samples(clr_py, select = main_vars)

# Inputs
# export data for RDAs
d_clr <- t(as(otu_table(clr_py_sub), "matrix"))
sdf <- as(sample_data(clr_py_sub), "data.frame")

# check that sample data matches otu table (order and sample IDs)
identical(rownames(d_clr), rownames(sdf)) # should be TRUE

var_list <- as.list(sdf) 

# run the RDA
set.seed(9999)
rda_tests <- lapply(var_list, 
                    FUN = function(x) {rda(d_clr ~ x, na.action = "na.omit")}) 

# Test each model using PERMANOVA
aov_rda <- lapply(rda_tests, FUN = function(x) {anova(x)}) 

# extract and clean up output
# p-values as data frame
p_vals_df <- aov_rda %>% lapply(., function(x) {x["Pr(>F)"]}) %>% 
  bind_rows(., .id = "Variable") %>% .[!grepl("Residual", row.names(.)),]

# R2 data frame
r2s_df <- rda_tests %>% lapply(., FUN = function(x) {RsquareAdj(x)$r.squared}) %>% bind_rows(., .id = "column_label")
r2s_df2 <- as.data.frame(t(r2s_df))

identical(p_vals_df$Variable, row.names(r2s_df2))

# Merge
p_val_r2 <- data.frame(Variable = p_vals_df$Variable, 
                       r.squared = r2s_df2$V1, 
                       p.value = p_vals_df$`Pr(>F)`)

p_val_r2
aov_rda # The simple way to get pseudo-F values for the table in the ms (Table S2)

# Repeat - a combined model with only the sig. predictors - Table S3
set.seed(9999)
rda_tests <- rda(d_clr ~ sdf$Age_Cat + sdf$AgeYear + sdf$Season + sdf$Rain30, na.action = "na.omit") 

summary(rda_tests)

# R2 and p-value
RsquareAdj(rda_tests)
anova.cca(rda_tests, step = 1000)
anova.cca(rda_tests, step = 1000, by = "term") # shows each predictor on it's own
