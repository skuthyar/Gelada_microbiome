# Create Figure 5: forest plot of breakaway regression results
infant<-c("Infant vs Adult in Dry Season", "Infant vs Adult in Wet Season")
estimate<-c(-0.2337576, -0.1477579) 
season<-c("Dry", "Wet")
se<-c(0.04325141, 0.03574336)
# values taken from from breakaway regression results
A<-data.frame(infant, season, estimate, se)

colors_geladas <- c("tan", "forestgreen")
g<-ggplot(data=A, aes(x=infant, y=estimate, ymin= (estimate - (0.96* se)), ymax=(estimate + (0.96* se)))) + geom_pointrange()+ # Makes range for ggplot values based on the data and AES specified in first line
  geom_hline(yintercept=0, lty=2, size =1) +  # Add a dotted line at x = 0 after flip
  geom_errorbar(aes(ymin=(estimate - (0.96* se)), ymax=(estimate + (0.96* se))), width=0.5, cex=1)+ # Makes whiskers on the range
  scale_y_continuous(breaks = seq(-0.4, 0.4, by = 0.05))+
  coord_flip() + # Flip coordinates (puts labels on y axis)
  geom_point(shape = 15, size = 10, color = colors_geladas) + xlab("") + ylab("") +
  theme(axis.text=element_text(size=15), axis.title=element_text(size=15))
print(g)
