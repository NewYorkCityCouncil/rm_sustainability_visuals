
library(data.table)
library(ggplot2)
library(ggpubr)

# new york city 

cols <- c("#D05D4E","#F59F00","#228AE6","#12B886","#23417D","#A07952","#82C91E","#CACACA","#2F56A6","#BE4BDB")

amount <- c(3, 2, 5, 25, 65)
Source <- c("Other", "Renewables", "Hydropower", "Nuclear", "Fossil Fuels")
data <- data.table(amount, Source)
data[, label := paste(amount, "%", sep="")]

ggdonutchart(data, x="amount",  label = "label", 
             fill = "Source", color = "white", lab.adjust = 0, 
             lab.pos = "out", lab.font = c(size=3)) + 
                                          theme(legend.position="right",
                                          panel.grid.major.y = element_blank(), 
                                          panel.grid.major.x = element_blank(),
                                          panel.grid.minor.x = element_blank(),
                                          panel.grid.minor.y = element_blank(),
                                          text = element_text(family = "Open Sans"),
                                          plot.title = element_text(family = "Georgia",size = 14)) +
  scale_fill_manual(values=cols) +
  labs(title = "       New York City Energy Consumption, 2017",
       subtitle = "",
       caption = "Source: NYC Mayor's Office of Sustainability")

# new york state
# https://www.eia.gov/state/?sid=NY

nys_ec <- fread("data/nys_ec.csv")
nys <- nys_ec[-c(1:3), ]
names(nys) <- c("type", "amount")
nys[, amount := as.numeric(amount)]
group_1 <- c("Net Interstate Flow of Electricity", "Net Electricity Imports") 
fossils <- c("Natural Gas", "Motor Gasoline excl. Ethanol", "Other Petroleum", "Distillate Fuel Oil", "HGL", "Jet Fuel", "Residual Fuel")
nys <- nys[type %in% group_1, type := "Other"]
nys[type %in% fossils, type := "Fossil Fuels"]
nys[type %in% c("Other Renewables", "Biomass"), type := "Renewables"]
nys[type %in% "Nuclear Electric Power", type := "Nuclear"]
nys[type %in% "Hydroelectric Power", type := "Hydropower"]

nys[, amount := sum(amount), by = "type"]
nys <- unique(nys)
nys[, tot_amt := sum(amount)]
nys[, amount := amount/tot_amt, by = "type"]
nys[, amount := round(amount, 2)*100]
nys[, label := paste(amount, "%", sep = "")]
nys[, Source := type]

ggdonutchart(nys, x="amount",  label = "label", 
             fill = "Source", color = "white", lab.adjust = 0, 
             lab.pos = "out", lab.font = c(size=10), 
             palette = "") + theme(legend.position="right",
                                   panel.grid.major.y = element_blank(), 
                                   panel.grid.major.x = element_blank(),
                                   panel.grid.minor.x = element_blank(),
                                   panel.grid.minor.y = element_blank(),
                                   text = element_text(family = "Open Sans"),
                                   plot.title = element_text(family = "Georgia",size = 14)) +
  scale_fill_manual(values=cols) +
  labs(title = "       New York State Energy Consumption, 2017",
       subtitle = "",
       caption = "Source: Energy Information Administration, \nState Energy Data System")



#### us
cols <- c("#D05D4E","#F59F00","#228AE6","#23417D")

# Us annual 
amount <- c(8.25, 2.75, 8, 81)
Source <- c("Renewables", "Hydropower", "Nuclear", "Fossil Fuels")

data <- data.table(amount, Source)
data[, label := paste(amount, "%", sep="")]

ggdonutchart(data, x="amount",  label = "label", 
             fill = "Source", color = "white", lab.adjust = 0, 
             lab.pos = "out", lab.font = c(size=3), 
             palette = "viridis") + theme(legend.position="right",
                                          panel.grid.major.y = element_blank(), 
                                          panel.grid.major.x = element_blank(),
                                          panel.grid.minor.x = element_blank(),
                                          panel.grid.minor.y = element_blank(),
                                          text = element_text(family = "Open Sans"),
                                          plot.title = element_text(family = "Georgia",size = 14)) +
  scale_fill_manual(values=cols) +
  labs(title = "      US Energy Consumption, 2017",
       subtitle = "",
       caption = "Source: Energy Information Administration, \nEnergy Data System")
