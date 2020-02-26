# let's look at the global fossil fuel consumption

library(data.table)
library(ggplot2)

ff_glob <- fread("data/gloabal_fossil_fuel_cons.csv")

ff_glob_long <- melt(ff_glob, id.vars = c("Entity", "Code", "Year"))

ff_glob_long[, Source := gsub("(terawatt-hours)", "", variable)]
ff_glob_long[, Source := gsub("()", "", Source, fixed = TRUE)]
cols <- c("#D05D4E","#F59F00","#228AE6","#12B886","#23417D","#A07952","#82C91E","#CACACA","#2F56A6","#BE4BDB")

ggplot(ff_glob_long, aes(x=Year, y=value, fill=Source)) + 
  geom_area() + theme_bw() + theme_bw() + theme(legend.position="right",
                                                  panel.grid.major.x = element_blank(),
                                                  panel.grid.minor.x = element_blank(),
                                                  text = element_text(family = "Open Sans"),
                                                  plot.title = element_text(family = "Georgia",size = 14),
                                                  axis.text.x = element_text(angle = 90),
                                                  axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
                                                  axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) + 
  scale_fill_manual(values=c("#12B886","#23417D","#A07952")) + 
  
  ylab("Energy Consumption (TWh)") + xlab("Year") + 
  scale_y_continuous(labels = scales::comma) + 
  labs(title = "Global Fossil Fuel Consumption", 
       subtitle = "Global primary energy consumption by source, in terawatt-hours (TWh)", 
       caption = "Source: BP Statistical Review of World Energy, 2017") 

