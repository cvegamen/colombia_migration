#########################
# Plot immigration
#########################

library(ggplot2)

# Load data
m = read.csv("~/Dropbox/Migration Colombia/01_data/raw_data/total_migrants.csv", stringsAsFactors=FALSE, sep=";")

# Prepare plot
Year=m$Year
total=m$Migrants.Total/1000

cairo_pdf(file="~/Dropbox/Migration Colombia/03_manuscript/figures/fig2.pdf", 
          width=10, 
          height=10)
ggplot(m, aes(x = Year, y = total)) + geom_col(position = "dodge",  width=0.8 ) +  
                                      scale_x_continuous(breaks=c(2010,2011,2012,2013,2014,2015,2016, 2017,2018)) + 
                                      theme_minimal() + 
                                      theme(legend.key = element_rect(colour = NA)) + 
                                      theme(text = element_text(size=20)) +
                                      theme(legend.position="bottom") + 
                                      ylab("Number of immigrants(in thousands)")
dev.off()

cairo_ps(file="~/Dropbox/Migration Colombia/03_manuscript/figures/fig2.eps", 
          width=10, 
          height=10)
ggplot(m, aes(x = Year, y = total)) + geom_col(position = "dodge",  width=0.8 ) +  
  scale_x_continuous(breaks=c(2010,2011,2012,2013,2014,2015,2016, 2017,2018)) + 
  theme_minimal() + 
  theme(legend.key = element_rect(colour = NA)) + 
  theme(text = element_text(size=20)) +
  theme(legend.position="bottom") + 
  ylab("Number of immigrants(in thousands)")
dev.off()


