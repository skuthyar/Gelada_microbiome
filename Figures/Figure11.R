# Agglomeration **using Baniel et al 2021 data and code
ps1 = tax_glom(season_physeq,"Phylum",NArm=FALSE)
ps2<-psmelt(ps1)
ps2$Taxa<-ps2$Phylum

ps1 = tax_glom(season_physeq,"Class",NArm=FALSE)
ps2<-psmelt(ps1)
ps2$Taxa<-ps2$Class
  
ps1 = tax_glom(season_physeq,"Order",NArm=FALSE)
ps2<-psmelt(ps1)
ps2$Taxa<-ps2$Order
  
ps1 = tax_glom(season_physeq,"Family",NArm=FALSE)
ps2<-psmelt(ps1)
ps2$Taxa<-ps2$Family #CHOOSE THIS LEVEL
  
ps1 = tax_glom(season_physeq,"Genus",NArm=FALSE)
ps2<-psmelt(ps1)
ps2$Taxa<-ps2$Genus

# Write function **using Baniel et al 2021 data and code
'%!in%'<-function(x,y)!('%in%'(x,y))

# Calculate relative abundance of each taxa across all samples **using Baniel et al 2021 data and code
ps2$RelativeAbundance<-(as.numeric(as.character(ps2$Abundance))*100)/as.numeric(as.character(ps2$NbReads))
a1<-sapply(unique(ps2$Taxa),function(x) mean(ps2$RelativeAbundance[ps2$Taxa==x],na.rm=TRUE))
a2<-data.frame(cbind(Taxa=as.character(unique(ps2$Taxa)),RA=a1))
a2$RA<-as.numeric(as.character(a2$RA))
a2[with(a2, order(-RA)), ] 

# Filter taxa>0.01% relative abundance for the models **using Baniel et al 2021 data and code
ps3<-droplevels(subset(ps2,ps2$Taxa %!in% NA))
ps3<-droplevels(subset(ps3,ps3$Taxa %in% unique(a2$Taxa[a2$RA>=0.01])))
length(unique(ps3$Taxa)) # 142 
length(unique(ps2$Taxa)) # 218

# Read in csv that details which taxa change in abundance during which season - this csv is based on the analyses done in Baniel et al 2021.
season_microbes <- read.csv("season_microbes.csv")

# Create wet/dry variable for each taxa  
ps4 <- left_join(ps3, season_microbes, by = "Taxa")

wet_microbes <- season_microbes[season_microbes$Type == 'increase in wet periods',]
dry_microbes <- season_microbes[season_microbes$Type == 'increase in dry periods',]

# Filter for dates
ps4 <- ps4 %>%
  mutate(SampleDate = mdy(SampleDate)) %>%
  filter(between(SampleDate, as.Date('2017-10-01'), as.Date('2019-01-31'))) 

# dtw dry plot
dtwd <-droplevels(subset(ps4,ps4$Taxa %in% c("M2PB4-65 termite group", "Bacteroidales UCG-001"))) #0.30 - 0.03

dtw_d <- ggplot(dtwd, aes(x=SampleDate, y=RelativeAbundance+0.001, color = Taxa)) +
  scale_color_manual(values=c("#5c4424", "#a57a41")) +
  scale_y_log10() +
  scale_x_date(limits = as.Date(c("2017-9-01", "2019-01-31"))) +
  coord_cartesian(ylim = c(0.005, 5.0))+
    annotate("rect", xmin = as.Date("2017-10-01"), xmax = as.Date("2018-02-01"), ymin = 0.001, ymax = 5.0, 
             alpha = .15, fill = "#D2B48C") +
    annotate("rect", xmin = as.Date("2018-02-01"), xmax = as.Date("2018-06-01"), ymin = 0.001, ymax = 5.0, 
           alpha = .15, fill = "#D2B48C") +
    annotate("rect", xmin = as.Date("2018-10-01"), xmax = as.Date("2019-01-31"), ymin = 0.001, ymax = 5.0, 
             alpha = .15, fill = "#D2B48C") +
  geom_smooth(method="gam",se=F) +
    theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),
        legend.position ="none") +
  annotate(geom = "text", y= 0.65, x = as.Date("2018-02-08"), label = "M2PB4-65 termite group", fontface = "bold", size = 2.5, color = "#a57a41") +
  annotate(geom = "text", y= 0.07, x = as.Date("2018-02-01"), label = "Bacteroidales UCG-001", fontface = "bold", size = 2.5, color = "#5c4424")

dtw_d

# gc dry plot
gcd <-droplevels(subset(ps4,ps4$Taxa %in% c("Pirellulaceae", "Methanobacteriaceae"))) #1.0 - 0.03

gc_d <- ggplot(gcd, aes(x=SampleDate, y=RelativeAbundance+0.001, color = Taxa,  linetype = Taxa)) +
  scale_color_manual(values=c("#5c4424", "#a57a41")) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  scale_y_log10() +
  scale_x_date(limits = as.Date(c("2017-9-01", "2019-01-31"))) +
  coord_cartesian(ylim = c(0.005, 5.0))+
    annotate("rect", xmin = as.Date("2017-10-01"), xmax = as.Date("2018-02-01"), ymin = 0.001, ymax = 5.0, 
             alpha = .15, fill = "#D2B48C") +
    annotate("rect", xmin = as.Date("2018-02-01"), xmax = as.Date("2018-06-01"), ymin = 0.001, ymax = 5.0, 
           alpha = .15, fill = "#D2B48C") +
    annotate("rect", xmin = as.Date("2018-10-01"), xmax = as.Date("2019-01-31"), ymin = 0.001, ymax = 5.0, 
             alpha = .15, fill = "#D2B48C") +
  geom_smooth(method="gam",se=F) +
    theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        legend.position ="none") +
  annotate(geom = "text", y= 0.07, x = as.Date("2018-11-30"), label = "Pirellulaceae", fontface = "bold", size = 2.5, color = "#a57a41") +
  annotate(geom = "text", y= 0.94, x = as.Date("2018-09-15"), label = "Methanobacteriaceae", fontface = "bold", size = 2.5, color =
             "#5c4424") +
  annotate(geom = "text", y= 0.75, x = as.Date("2017-11-15"), label = "Predictor", fontface = "bold", size = 2.5, color =
             "#5c4424") +
  annotate(geom = "text", y= 0.07, x = as.Date("2017-11-15"), label = "Predicted", fontface = "bold", size = 2.5, color =
             "#a57a41")
gc_d

# dtw wet plot
dtww <-droplevels(subset(ps4,ps4$Taxa %in% c("Prevotellaceae", "Muribaculaceae"))) #0.50 - 0.05

dtw_w <- ggplot(dtww, aes(x=SampleDate, y=RelativeAbundance+0.001, color = Taxa)) +
  scale_color_manual(values=c("#0f3d0f", "#2db82d")) +
  scale_y_log10() +
  scale_x_date(breaks = as.Date(c("2018-01-15", "2018-08-01", "2018-12-23")),
               limits = as.Date(c("2017-9-01", "2019-01-31")), 
               labels = c("Dry","Wet", "Dry")) +
  coord_cartesian(ylim = c(0.005, 5.0))+
    annotate("rect", xmin = as.Date("2018-06-01"), xmax = as.Date("2018-10-01"), ymin = 0.001, ymax = 5.0, 
           alpha = .15, fill = "#228B22") +
  geom_smooth(method="gam",se=F) +
    theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position ="none") +
  annotate(geom = "text", y= 0.60, x = as.Date("2018-02-15"), label = "Muribaculaceae", fontface = "bold", size = 2.5, color = "#0f3d0f") +
  annotate(geom = "text", y= 0.14, x = as.Date("2018-02-15"), label = "Prevotellaceae", fontface = "bold", size = 2.5, color = "#2db82d")

dtw_w

# gc wet
gcw <-droplevels(subset(ps4,ps4$Taxa %in% c("Prevotellaceae", "Acidaminococcaceae"))) #3.0 - 0.1 this scale is a problem

gc_w <- ggplot(gcw, aes(x=SampleDate, y=RelativeAbundance+0.001, color = Taxa, linetype = Taxa)) +
  scale_color_manual(values=c("#0f3d0f", "#228B22")) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  scale_y_log10() +
  scale_x_date(breaks = as.Date(c("2018-01-15", "2018-08-01", "2018-12-23")),
               limits = as.Date(c("2017-9-01", "2019-01-31")), 
               labels = c("Dry","Wet", "Dry")) +
    annotate("rect", xmin = as.Date("2018-06-01"), xmax = as.Date("2018-10-01"), ymin = 0.001, ymax = 5.0, 
           alpha = .15, fill = "#228B22") +
  geom_smooth(method="gam",se=F) +
    theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        legend.position ="none") +
  annotate(geom = "text", y= 1.1, x = as.Date("2018-10-05"), label = "Acidaminococcaceae", fontface = "bold", size = 2.5, color = "#0f3d0f") +
  annotate(geom = "text", y= 0.05, x = as.Date("2018-10-10"), label = "Prevotellaceae", fontface = "bold", size = 2.5, 
           color = "#228B22") +
  annotate(geom = "text", y= 1.1, x = as.Date("2017-11-15"), label = "Predictor", fontface = "bold", size = 2.5, color =
             "#0f3d0f") +
  annotate(geom = "text", y= 0.05, x = as.Date("2017-11-15"), label = "Predicted", fontface = "bold", size = 2.5, color =
             "#228B22")
gc_w

# To create Figure 11 
# Combine first panel
panel_1 <- plot_grid(dtw_d, gc_d, dtw_w, gc_w,
          labels = c("A", "B", "C", "D"), 
          label_x = .20,
          label_y = .94,
          align = "v",
          nrow = 2, 
          ncol = 2)

# Create labels
y.grob <- textGrob("Log relative abundance (%)", 
                   gp=gpar(fontface="bold", fontsize=15), rot=90)

x.grob <- textGrob("Seasons Sept 2017 - Jan 2019", 
                   gp=gpar(fontface="bold", fontsize=15))


tiff(file="season fig panel 1",
width=20, height=14, units="cm", res=700)

grid.arrange(arrangeGrob(panel_1, left = y.grob, bottom = x.grob))


dev.off()

# Create two plots for second panel
# granger causality dry -> wet
gcdw <-droplevels(subset(ps4,ps4$Taxa %in% c("Lachnospiraceae", "M2PB4-65 termite group"))) #0.03 - 0.01

gc_dw <- ggplot(gcdw, aes(x=SampleDate, y=RelativeAbundance+0.001, color = Taxa,  linetype = Taxa)) +
  scale_color_manual(values=c("#228B22", "#5c4424")) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  scale_y_log10() +
  scale_x_date(breaks = as.Date(c("2018-01-15", "2018-08-01", "2018-12-23")),
               limits = as.Date(c("2017-9-01", "2019-01-31")), 
               labels = c("Dry","Wet", "Dry")) +
  coord_cartesian(ylim = c(0.005, 5.0))+
     annotate("rect", xmin = as.Date("2017-10-01"), xmax = as.Date("2018-02-01"), ymin = 0.001, ymax = 5.0, 
             alpha = .15, fill = "#D2B48C") +
    annotate("rect", xmin = as.Date("2018-02-01"), xmax = as.Date("2018-06-01"), ymin = 0.001, ymax = 5.0, 
           alpha = .15, fill = "#D2B48C") +
    annotate("rect", xmin = as.Date("2018-10-01"), xmax = as.Date("2019-01-31"), ymin = 0.001, ymax = 5.0, 
             alpha = .15, fill = "#D2B48C") +
  annotate("rect", xmin = as.Date("2018-06-01"), xmax = as.Date("2018-10-01"), ymin = 0.001, ymax = 5.0, 
           alpha = .15, fill = "#228B22") +
  geom_smooth(method="gam",se=F) +
    theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(), 
        legend.position ="none") +
  annotate(geom = "text", y= 0.65, x = as.Date("2018-10-05"), label = "M2PB4-65 termite group", fontface = "bold", size = 2.5, 
           color = "#5c4424") +
  annotate(geom = "text", y= 0.015, x = as.Date("2018-11-01"), label = "Lachnospiraceae", fontface = "bold", size = 2.5, 
           color = "#228B22") +
    annotate(geom = "text", y= 0.70, x = as.Date("2018-01-20"), label = "Predictor", fontface = "bold", size = 2.5, color =
             "#5c4424") +
  annotate(geom = "text", y= 0.015, x = as.Date("2018-02-05"), label = "Predicted", fontface = "bold", size = 2.5, color =
             "#228B22")
  
gc_dw

# granger causality wet -> dry
gcwd <-droplevels(subset(ps4,ps4$Taxa %in% c("Bacteroidales UCG-001", "Muribaculaceae"))) #0.03 - 0.03

gc_wd <- ggplot(gcwd, aes(x=SampleDate, y=RelativeAbundance+0.001, color = Taxa, linetype = Taxa)) +
  scale_color_manual(values=c("#a57a41", "#0f3d0f")) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  scale_y_log10() +
  scale_x_date(breaks = as.Date(c("2018-01-15", "2018-08-01", "2018-12-23")),
               limits = as.Date(c("2017-9-01", "2019-01-31")), 
               labels = c("Dry","Wet", "Dry")) +
     annotate("rect", xmin = as.Date("2017-10-01"), xmax = as.Date("2018-02-01"), ymin = 0.001, ymax = 5.0, 
             alpha = .15, fill = "#D2B48C") +
    annotate("rect", xmin = as.Date("2018-02-01"), xmax = as.Date("2018-06-01"), ymin = 0.001, ymax = 5.0, 
           alpha = .15, fill = "#D2B48C") +
    annotate("rect", xmin = as.Date("2018-10-01"), xmax = as.Date("2019-01-31"), ymin = 0.001, ymax = 5.0, 
             alpha = .15, fill = "#D2B48C") +
  annotate("rect", xmin = as.Date("2018-06-01"), xmax = as.Date("2018-10-01"), ymin = 0.001, ymax = 5.0, 
           alpha = .15, fill = "#228B22") +
  geom_smooth(method="gam",se=F) +
    theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(), 
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        legend.position ="none") +
  annotate(geom = "text", y= 0.80, x = as.Date("2018-11-10"), label = "Muribaculaceae", fontface = "bold", size = 2.5, 
           color = "#0f3d0f") +
  annotate(geom = "text", y= 0.009, x = as.Date("2018-10-25"), label = "Bacteroidales UCG-001", fontface = "bold", size = 2.5, 
           color = "#a57a41") +
  annotate(geom = "text", y= 0.70, x = as.Date("2018-01-20"), label = "Predictor", fontface = "bold", size = 2.5, color =
             "#0f3d0f") +
  annotate(geom = "text", y= 0.06, x = as.Date("2018-02-05"), label = "Predicted", fontface = "bold", size = 2.5, color =
             "#a57a41")
gc_wd

# Combine second panel
panel_2 <- plot_grid(gc_dw, gc_wd,
          labels = c("E", "F"), 
          label_x = .20,
          label_y = .94,
          align = "H",
          nrow = 1, 
          ncol = 2)


tiff(file="season fig panel 2",
width=20, height=7, units="cm", res=700)

grid.arrange(arrangeGrob(panel_2, left = y.grob, bottom = x.grob))


dev.off()
