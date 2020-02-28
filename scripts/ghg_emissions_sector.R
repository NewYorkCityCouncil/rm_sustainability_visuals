# let's do the emissions plot 
library(data.table)
library(ggplot2)
library(ggthemes)
library(viridis)
library(hrbrthemes)
library(dplyr)

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

ghg_sub=unique(ghg_sub)

#double check with origral dataset
round(sum(as.numeric(as.character(ghg$`2005`))))
 round(sum(as.numeric(as.character(ghg$`2017`))))

totals <- ghg_sub %>%
  group_by(Year) %>%
  summarize(total = round(sum(value),2))

#remove middle values
totals$total[2:12]<-NA


cols <- c("#D05D4E","#F59F00","#228AE6","#12B886","#23417D","#A07952","#82C91E","#CACACA","#2F56A6","#BE4BDB")

p <- ggplot(ghg_sub, aes(x=Year, y=value, fill=Sector)) + geom_col() +
 theme_ipsum(axis_title_just = "mc", 
                base_size = 8,
                axis_title_size = 11,
                axis_text_size = 10.5) + 
  theme(legend.position="right", legend.text=element_text(size=9), 
        legend.title=element_text(size=10), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        text = element_text(family = "Open Sans"),
            plot.title = element_text(family = "Georgia",size = 14),
            axis.text.x = element_text(angle = 90),
            axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
            axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) + 
  scale_fill_manual(values=c("#D05D4E","#F59F00","#228AE6")) + 
  #geom_text(# Filter data first
  #  aes(label = labels), vjust = -10) +
    geom_text(aes(Year, total, label = total, fill = NULL), data = totals, vjust = -0.55, size=2.5) +
  #geom_text(aes(Year, total + 1510, label = total, fill = NULL), data = totals) +
  #geom_text(aes(label = stat(y), group = Year), 
  #          stat = 'summary', fun.y = c('round','sum'), vjust = -1)  +
   ylab("Million Metric Tons of CO2 Equivalent (MtCO2e)") + 
   ggtitle("Greenhouse Gas Emissions by Sector", "Emissions declined 17% from 2005 to 2017") +
  labs(caption = "Source: NYC Mayor's Office of Sustainability") 

ggsave("/visuals/ghg_by_sector.png", plot = p, path = getwd(), width = 8.5, height = 5, units = "in", dpi = 300)


  

