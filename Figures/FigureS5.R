# Using larger phyloseq object
# Prep ASV data using ASV table from family-level phyloseq object
metalon_otu <- season_otu %>%
  mutate(taxa_id = f_names) %>%
  select(last_col(), everything()) %>%
  rownames_to_column() %>%
  select(-rowname) %>%
  column_to_rownames(var = "taxa_id") 

# Put columns in descending order 
column_order <- (names(metalon_otu)) 
metalon_otu2 <- metalon_otu %>%
  select(all_of(column_order))

# Select needed columns, create a numeric day column, and arrange row names to match ASV table
metalon_m <- season_m %>%
  select(c(ID, Sex, SampleDate)) %>%
  mutate(SampleDate = mdy(SampleDate)) %>%
  arrange(SampleDate) %>%
  mutate(day = as.factor(SampleDate)) %>%
  mutate(day = unclass(day)) %>%
  select(-SampleDate) %>%
  arrange(row.names(.)) 

# Check that row names in metadata are identical to column names in ASV table
identical(rownames(metalon_m), colnames(metalon_otu))

# Create vectors
group <- metalon_m$Sex
subject <- metalon_m$ID 
time <- as.numeric(metalon_m$day)

# Define the prediction timepoints 
points = seq(1, 300, length.out = 300)

# Run MetalonDA on one family (Helicobacteraceae)
# Identify significant time intervals between males and females 
output.metalonda.f1 = metalonda(Count = data.matrix(metalon_otu2)[1,], Time = time, Group = group,
                                ID = subject, n.perm = 100, points = points,
                                text = rownames(data.matrix(metalon_otu2))[1],
                                parall = FALSE, pvalue.threshold = 0.05,
                                adjust.method = "BH", time.unit = "days", ylabel = "Normalized Count",
                                col = c("black", "green"), prefix = "metalon_F1")
