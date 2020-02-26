# let's do the emissions plot 
library(data.table)
library(ggplot2)
library(ggthemes)
library(viridis)

ghg <- fread("data/nyc_ghg.csv")

names(ghg) <- as.character(ghg[3, ])
ghg <- ghg[-c(1:3), ]
names(ghg)[5:17] <- c(2005:2017)

long_ghg <- melt(ghg, id.vars = c("(Sectors, Sector)", "(Category, Label)", "(Source, Label)", "(Source, Units)"), variable.name = "Year")
setnames(long_ghg, "(Sectors, Sector)", "Sector")
long_ghg[, value := as.numeric(value)]
ghg_sub <- long_ghg[, .(value, Year, Sector)]
ghg_sub[, value := sum(value), by = c("Year", "Sector")]
ghg_sub[, value := value/1000000]
ghg_sub[, value := value/10]

cols <- c("#D05D4E","#F59F00","#228AE6","#12B886","#23417D","#A07952","#82C91E","#CACACA","#2F56A6","#BE4BDB")
p <- ggplot(ghg_sub, aes(x=Year, y=value, fill=Sector)) + geom_col() 
  
  
p + theme_bw() + theme(legend.position="right",
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            text = element_text(family = "Open Sans"),
            plot.title = element_text(family = "Georgia",size = 14),
            axis.text.x = element_text(angle = 90),
            axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
            axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) + 
  scale_fill_manual(values=c("#D05D4E","#F59F00","#228AE6")) + 
   ylab("Million Metric Tons of CO2 Equivalent (MtCO2e)") + 
   ggtitle("Greenhouse Gas Emissions by Sector") +
  labs(caption = "Source: NYC Mayor's Office of Sustainability") 

