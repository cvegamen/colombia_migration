m = read.csv("total_migrants.csv", stringsAsFactors=FALSE, sep=";")



#########################

# Plot

#########################

library(ggplot2)


year=m$Year
total=m$Migrants.Total/1000


ggplot(m, aes(x = year, y = total)) + geom_col(position = "dodge",  width=0.8 ) + 
  scale_x_continuous(breaks=c(2010,2011,2012,2013,2014,2015,2016, 2017,2018)) + 
  
  theme_minimal() + 
  theme(legend.key = element_rect(colour = NA)) + theme(text = element_text(size=10)) + 
  theme(legend.position="bottom") + ylab("Number of immigrants(in thousands)")

##
install.packages("Cairo")
install.packages("cairoDevice")


cairo_pdf(file="C:/Users/catis/Dropbox/Migration Colombia/01_data/raw_data/Rplot02.pdf",
          
          width=10,
          
          height=10)

Year=m$Year
total=m$Migrants.Total/1000


ggplot(m, aes(x = Year, y = total))+
  geom_col(position = "dodge") +
  scale_x_continuous(breaks=c(2010,2011,2012,2013,2014,2015,2016, 2017,2018)) +
  scale_y_continuous(breaks=c(100,200,300,400,500,600,700,800,900,1000))+
  theme_bw()+ 
  theme(legend.key = element_rect(colour = NA))+
  theme(text = element_text(size=30)) + theme(legend.position="bottom")+
  ylab("Number of refugees (in thousands)")



dev.off()
