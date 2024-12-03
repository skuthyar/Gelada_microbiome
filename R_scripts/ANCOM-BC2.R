# Set prevalence to be 20%
prev <- 0.2
full_gelada_clean_family <- full_gelada_clean%>%
  tax_glom("Family") %>%
  filter_taxa(., function(x) sum(x > 0) > (prev*length(x)), TRUE) # this took it down to 94 taxa
psq = subset_taxa(full_gelada_clean_family, Family !="uncultured rumen bacterium"
                  & Family !="Unassigned Order" & Family !="uncultured Mollicutes bacterium"
                  & Family !="uncultured Clostridiaceae bacterium"
                  & Family !="uncultured bacterium"
                  & Family !="uncultured" & Family !="Unassigned"
                  & Family !="gut metagenome" & Family !="metagenome" & Family != "uncultured Firmicutes bacterium"
                  & Family != "uncultured prokaryote" & Family != "uncultured organism" & Family != "unidentified rumen bacterium RF39") # this took it down to 57 taxa
out_season = ancombc(
  phyloseq = psq, 
  formula = "Season", 
  p_adj_method = "fdr", 
  zero_cut = 0.90, # by default prevalence filter of 10% is applied
  lib_cut = 0, 
  group = "Season", 
  struc_zero = TRUE, 
  neg_lb = TRUE, 
  tol = 1e-5, 
  max_iter = 100, 
  conserve = TRUE, 
  alpha = 0.05, 
  global = TRUE
)

res = out_season$res
qval = res$q_val
tab_lfc = res$beta
se = res$se
tab_lfc$ASV <- rownames(tab_lfc)
col_name = c("Dry.Wet","ASV")
colnames(tab_lfc) = col_name
tax <- tax_table(psq)
tax <- as.data.frame(tax)
tab_lfc <- merge(tab_lfc, tax, by='row.names')
tab_lfc <- column_to_rownames(tab_lfc, var = "Row.names")
tab_lfc <- merge(tab_lfc, se, by="row.names")
colnames(tab_lfc)[11] = "SE"
tab_lfc1 <- tab_lfc %>%
  arrange(desc(Dry.Wet)) %>%
  dplyr::mutate(direct = ifelse(Dry.Wet > 0, "Positive LFC", "Negative LFC"))

tab_lfc1$direct = factor(tab_lfc1$direct, 
                           levels = c("Positive LFC", "Negative LFC"))
tab_lfc1_strict <- subset(tab_lfc1, tab_lfc1$Dry.Wet > 0.5 | tab_lfc1$Dry.Wet < -0.5)
