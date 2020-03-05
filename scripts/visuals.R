library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(scales)
library(weathermetrics)
library(RSocrata)
library(networkD3)
library(htmlwidgets)
library(htmltools)
library(webshot)
library(lubridate)
library(plotly)
library(chartjs)
#webshot::install_phantomjs()


options(scipen = 999)
# SOC SUSTAINABILITY - BACKGROUND SECTION: EVIDENCE OF CLIMATE CHANGE --------------

# global temp rise ----------

glotemp=read.delim("https://data.giss.nasa.gov/gistemp/graphs/graph_data/Global_Mean_Estimates_based_on_Land_and_Ocean_Data/graph.txt", sep = "", header = FALSE, stringsAsFactors = FALSE)
glotemp=glotemp[-c(1:5),-4]
names(glotemp)<-c("Year", "No_Smoothing", "Lowess")
glotemp$farenheit_ns <- celsius.to.fahrenheit(as.numeric(as.character(glotemp$No_Smoothing)))-32

row.names(glotemp) <- NULL

glotemp$Year<-as.numeric(glotemp$Year)
glotemp$label<-rep("", nrow(glotemp))
 glotemp[which(glotemp$Year %in% seq(1890,2010, 10)),]$label <- seq(1890,2010, 10)
glotemp[1,]$label<-1881
glotemp[139,]$label<-2019


#plot
p <- glotemp %>%
  ggplot(aes(x=Year, y=farenheit_ns)) + 
  xlab("Year") + ylab("Temperature Anamoly  (F)")+
  #geom_line(color="grey") +
  # geom_text(# Filter data first
  #  aes(label=label), nudge_y = 50) +
  geom_point(shape=21, color="#2F56A6", fill="#2F56A6", size=1) +
  geom_smooth(se=FALSE, fullrange=TRUE,color="#2F56A6") +
  scale_x_continuous(waiver(), 
                     breaks = as.numeric(glotemp$label), 
                     labels = as.numeric(glotemp$label), limits=c(1881,2019)) +
  scale_y_continuous(limit=c(-1,NA),
                     oob=squish,
                     breaks=seq(-1,2,0.5),
                     labels=seq(-1,2,0.5))+
  theme_ipsum(axis_title_just = "mc", 
              base_size = 8,
              axis_title_size = 11,
              axis_text_size = 10) +
  theme(legend.position="none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        text = element_text(family = "Open Sans"),
        plot.title = element_text(family = "Georgia",size = 14),
        axis.text.x = element_text(angle = 90),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
  ggtitle("Global Temperature Rise",
          paste("How much warmer than average the most recent year was globally")) +
  labs(caption = "NASA's Goddard Institute for Space Studies (GISS)")

ggsave("/visuals/Global Temperature Rise.png", plot = p, path = getwd(), width = 8.5, height = 5, units = "in", dpi = 300)


# greenland ice mass ------
#require earth data authentication/registration
#HDR column description
# HDR 1 TIME (year.decimal)
# HDR 2 Antarctic mass (Gigatonnes)
# HDR 2 Antarctic mass 1-sigma uncertainty (Gigatonnes)
aicemass=read.delim("/Users/romartinez/Desktop/sshfs/rm_sustainability_visuals/data/antarctica_mass_200204_201911.txt", sep = "", header = FALSE)


# HDR column description
# HDR 1 TIME (year.decimal)
# HDR 2 Greenland mass (Gigatonnes)
# HDR 3 Greenland mass 1-sigma uncertainty (Gigatonnes)
grlicemass=read.delim(paste0(getwd(),"/data/greenland_mass_200204_201911.txt"), sep = "", header = FALSE)

grlicemass=grlicemass[-c(1:36),-c(4:20)]
row.names(grlicemass) <- NULL
names(grlicemass)<- c("Year", "Mass", "Uncertainty")
grlicemass$Mass <-as.numeric(as.character(grlicemass$Mass))
grlicemass$Year <-as.numeric(as.character(grlicemass$Year))


 #grlicemass$label<-rep("", nrow(grlicemass))
 #grlicemass[c(seq(7,67,12),76,seq(87,122,9),seq(123,133,5)),]$label <-seq(2006,2019, 1)
 grlicemass$label= as.integer(substr(grlicemass$Year, 1, 4))
 grlicemass$label[duplicated(grlicemass$label)] <- ""


#plot
p <- grlicemass %>%
  ggplot(aes(x=Year, y=Mass)) + 
  xlab("Year") + ylab("Mass (Gt)")+
  #geom_line(color="#2F56A6", size=1) +
  #geom_text(# Filter data first
  #  aes(label=label), nudge_y = 50) +
  geom_point(shape=21, color="#2F56A6", fill="#2F56A6", size=1) +
  scale_x_continuous(waiver(), 
                     breaks = as.numeric(grlicemass$label), 
                     labels = as.numeric(grlicemass$label), limits=c(2002,2019)) +
  geom_smooth(se=FALSE, fullrange=TRUE,color="#2F56A6", 
              mapping = aes(weight = as.numeric(as.character(Uncertainty)))) +
  theme_ipsum(axis_title_just = "mc", 
              base_size = 8,
              axis_title_size = 11,
              axis_text_size = 10.5) +
  theme(legend.position="none",
        panel.grid.major.y = element_line(), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        text = element_text(family = "Open Sans"),
        plot.title = element_text(family = "Georgia",size = 14),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.text.x = element_text(angle = 90))+
  ggtitle("Greenland Ice Mass Variaton Since 2002",
          "Ice Mass Loss of 283.0 Gigatonnes per year") +
  labs(caption = "Ice mass measurement by NASA's GRACE satellites.")

ggsave("/visuals/Greenland Ice Mass.png", plot = p, path = getwd(), width = 8.5, height = 5, units = "in", dpi = 300)



# sea level satellite observations --------------

#HDR column description
# HDR 1 altimeter type 0=dual-frequency  999=single frequency (ie Poseidon-1)
# HDR 2 merged file cycle #
# HDR 3 year+fraction of year (mid-cycle)
# HDR 4 number of observations
# HDR 5 number of weighted observations
# HDR 6 GMSL (Global Isostatic Adjustment (GIA) not applied) variation (mm) with respect to 20-year TOPEX/Jason collinear mean reference
# HDR 7 standard deviation of GMSL (GIA not applied) variation estimate (mm)
# HDR* 8 smoothed (60-day Gaussian type filter) GMSL (GIA not applied) variation (mm)  with respect to 20-year mean
# HDR* 9 GMSL (Global Isostatic Adjustment (GIA) applied) variation (mm) )  with respect to 20-year mean
# HDR 10 standard deviation of GMSL (GIA applied) variation estimate (mm)
# HDR* 11 smoothed (60-day Gaussian type filter) GMSL (GIA applied) variation (mm) )  with respect to 20-year mean
# Use this Column below!!!!!!
# HDR* 12 smoothed (60-day Gaussian type filter) GMSL (GIA applied) variation (mm); annual and semi-annual signal removed )  with respect to 20-year mean
satsealevel=read.delim("/Users/romartinez/Desktop/untitled_folder/Data_Products/rm_sustainability_visuals/data/GlobalMeanSeaLevel_TPJAOS_4.2_199209_201911.txt", sep = "", header = FALSE)

satsealevel=satsealevel[-c(1:54), -c(13:20)]
names(satsealevel)<- c("altimeter_type", "filecyclenum", "Year_Decimal", "Observations", "Weights_Obs", "GMSL_nGIA", "SD_GMSL_nGIA", "GMSL_sm_nGIA", "GMSL_wGIA", "SD_GMSL_wGIA", "GMSL_sm_wGIA","add_GMSL_sm_wGIA")

satsealevel$Year= as.integer(substr(satsealevel$Year_Decimal, 1, 4))
satsealevel$Year[duplicated(satsealevel$Year)] <- ""
satsealevel$add_GMSL_sm_wGIA=as.numeric(as.character(satsealevel$add_GMSL_sm_wGIA))
satsealevel$ref1993=satsealevel$add_GMSL_sm_wGIA-satsealevel$add_GMSL_sm_wGIA[1]
#label
satsealevel$label=rep("", nrow(satsealevel))
satsealevel$label[nrow(satsealevel)] <-"94.6 mm (9/13/19)"
#plot

p <- satsealevel %>%
  ggplot(aes(x=Year_Decimal, y=ref1993)) + 
  xlab("Year") + ylab("Sea Height Variation (mm)")+
  #geom_line(color="grey") +
  geom_point(shape=21, color="#2F56A6", fill="#2F56A6", size=0.5) +
  geom_smooth(se=FALSE, fullrange=TRUE,color="#2F56A6") +
  scale_x_discrete(waiver(), 
                   breaks = waiver(), 
                   labels = satsealevel$Year) +
  theme_ipsum(axis_title_just = "mc", 
              base_size = 8,
              axis_title_size = 11) +
  theme(legend.position="none",
        panel.grid.major.y = element_line(), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        text = element_text(family = "Open Sans"),
        plot.title = element_text(family = "Georgia",size = 14),
        axis.text.x = element_text(angle = 90),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))+
  geom_text(aes(label=label), size =3.5, vjust= -0.3, hjust=1.05 ) +
  ggtitle("Sea Level Rise",
          paste("Global mean sea level is rising 3.3 mm per year")) +
  labs(caption = "Satellite sea level observations, NASA Goddard Space Flight Center.")

ggsave("/visuals/Sea Level Satellite Observations.png", plot = p, path = getwd(), width = 8.5, height = 5, units = "in", dpi = 300)






# artic sea ice min ---------------
artic_sea_ice_min=read.delim("https://climate.nasa.gov/system/internal_resources/details/original/1929_Arctic_data_1979-2019.txt")


#plot
p <- artic_sea_ice_min %>%
  ggplot(aes(x=year, y=extent)) + 
  xlab("Year") + ylab("Extent  (Million Square km)")+
  #geom_line(color="grey") +
  # geom_text(# Filter data first
  #  aes(label=label), nudge_y = 50) +
  geom_point(shape=21, color="#2F56A6", fill="#2F56A6", size=1) +
  geom_smooth(se=FALSE, fullrange=TRUE,color="#2F56A6") +
  theme_ipsum(axis_title_just = "mc", 
              base_size = 8,
              axis_title_size = 11,
              axis_text_size = 10.5) +
  theme(legend.position="none",
        panel.grid.major.y = element_line(), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        text = element_text(family = "Open Sans"),
        plot.title = element_text(family = "Georgia",size = 14),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))+
  ggtitle("Arctic Sea Ice Minimum",
          paste("The Average September Arctic Sea Ice Extent is Declining 12.85% every Decade")) +
  labs(caption = "Satellite observations, NSIDC/NASA")

ggsave("/visuals/Artic Sea Ice Min.png", plot = p, path = getwd(), width = 8.5, height = 5, units = "in", dpi = 300)




# relative sea level trend at the battery -------
bat=read.csv(paste0(getwd(),'/data/battery_meantrend.csv'), row.names = NULL)
bat=bat[,-ncol(bat)]
names(bat)<-c("Year", "Month", "Monthly_MSL", "Unverified" , "Linear_Trend", "High_Conf.", "Low_Conf." )
bat$Year=bat$Year
bat$Monthly_MSL_ft=bat$Monthly_MSL*3.281
bat$Monthly_MSL_in=bat$Monthly_MSL*39.37

bat$date <- as.Date(paste(bat$Month, "01", bat$Year), format='%m %d %Y')
bat$dec_date <- decimal_date(bat$date)

summary(lm(Monthly_MSL ~ dec_date, data= bat))
#plot
p <- bat%>% 
  ggplot(aes(x=dec_date, y=Monthly_MSL_in)) +
  geom_point(aes(x=dec_date, y=Monthly_MSL_in), 
             shape=21, color="#2F56A6", fill="#2F56A6", size=0.8, alpha = 0.55) +
  stat_smooth(aes(x=dec_date, y=Monthly_MSL_in), 
              se=FALSE, fullrange=TRUE,color="#2F56A6", method = "lm") +
  ylab("Inches") +
  xlab("Year") +
  scale_y_continuous(limit=c(-22,12),
                     oob=squish,
                     breaks=seq(-22,12, 4),
                     labels=seq(-22,12, 4)) +
  scale_x_continuous(breaks = c(seq(1850,2019,10),2019)) +
  theme_ipsum(axis_title_just = "mc", 
              base_size = 8,
              axis_title_size = 11,
              axis_text_size = 10.5) +
  theme(legend.position="none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        #text = element_text(family = "Open Sans"),
        plot.title = element_text(family = "Georgia",size = 14),
        axis.text.x = element_text(angle = 90),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
  ggtitle("New York Battery Monthly Sea Level from 1856 to 2019", 
          "Sea level has been increasing 1.1 inches per decade.") +
  labs(caption = "tidesandcurrents.noaa.gov")

ggsave("/visuals/battery_msl.png", plot = p, path = getwd(), width = 8.5, height = 5, units = "in", dpi = 300)


# How much waste is NYC producing in 2017?-------

w=read.socrata('https://data.cityofnewyork.us/resource/ebb7-mvp5.json?$limit=99999999')
w$year<-as.integer(substr(w$month, 1, 4))
w$mo<- as.integer(substr(w$month, 8, 9))

w$date <- as.Date(paste(w$mo, "01", w$year), format='%m %d %Y')
w$refusetonscollected=as.numeric(w$refusetonscollected)
w$papertonscollected=as.numeric(w$papertonscollected)
w$mgptonscollected=as.numeric(w$mgptonscollected)
w$xmastreetons=as.numeric(w$xmastreetons)
w$leavesorganictons=as.numeric(w$leavesorganictons)
w$resorganicstons=as.numeric(w$resorganicstons)
w$schoolorganictons=as.numeric(w$schoolorganictons)

w2017<-w[w$year==2017,]


totals <- w2017 %>%
  group_by(year) %>%
  summarize(Landfill = round(sum(refusetonscollected,na.rm = TRUE),2), 
            Paper = round(sum(papertonscollected,na.rm = TRUE),2),
            `Metal, Glass, Plastics, Cartons`= round(sum(mgptonscollected,na.rm = TRUE),2),
            `Xmas Trees`= round(sum(xmastreetons,na.rm = TRUE),2),
            `Res. Organics`= round(sum(resorganicstons,na.rm = TRUE),2),
            `School Organics`= round(sum(schoolorganictons,na.rm = TRUE),2),
            `Leaves Organics`= round(sum(schoolorganictons,na.rm = TRUE),2)
            )

t2 <- data.frame(
  name=colnames(totals) ,  
  value=c(totals$year, totals$Landfill, totals$Paper, totals$`Metal, Glass, Plastics, Cartons`, totals$`Xmas Trees`, totals$`Res. Organics`, totals$`School Organics`, totals$`Leaves Organics`)
)

t2=t2[-1,]
t2=t2[order(t2$value, decreasing = TRUE),]

t2$percent=round(t2$value/sum(t2$value)*100,2)
  

#plot -----
#ggplot(aes(x=reorder(t2$name, t2$percent), y=sort(t2$percent, decreasing = TRUE),
p <- t2 %>% 
  ggplot(aes(x=reorder(t2$name, t2$percent), 
             y=sort(t2$percent, decreasing = TRUE),
             fill=as.factor(name))) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values=cols) +
  xlab("Materials") + ylab("Percent")+
  geom_text(# Filter data first
    aes(label=paste0(percent, '%')), nudge_y = 3.5, size=3.5) +
  theme_ipsum(axis_title_just = "mc", 
              base_size = 8,
              axis_title_size = 11,
              axis_text_size = 10,
              caption_size = 10) +
  theme(legend.position="none",
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        text = element_text(family = "Open Sans"),
        plot.title = element_text(family = "Georgia",size = 14),
        axis.text.y = element_text(margin = margin(t = 0, r = -16, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
  ggtitle("NYC Waste Composition in 2017",
          paste("80% of NYC's waste went to landfill in 2017")) +
  labs(caption = "DSNY Monthly Tonnage Data")

ggsave("/visuals/waste_2017.png", plot = p, path = getwd(), width = 8.5, height = 5, units = "in", dpi = 300)

#Ben's plot
#ran bens code
#
#sankey version ---------

links <- data.frame(
  source=c(rep("NYC Waste", 7),rep(as.vector(t2$name[1]),6)),
  target=c(as.vector(t2$name), "Organics", "*Other",'Paper ',"Metal, Glass, Plastics, Cartons ", "Textiles", "Other Divertable Materials"),
  value=c(t2$percent, 34,23,17,17,6,3)
)


nodes <- data.frame(
  name=c(as.character(links$source), 
         as.character(links$target)) %>% unique()
)

links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1

# cols <- c("#D05D4E","#F59F00","#228AE6","#12B886","#23417D","#A07952","#82C91E","#CACACA","#2F56A6","#BE4BDB")

# Add a 'group' column to each connection:
links$group <- as.factor(c("LF_grey",rep("RD_green",6),rep("LF_grey",6)))

# Add a 'group' column to each node. 
nodes$group <- as.factor(
  c("Total", "LF_dgrey",rep("RD_dgreen",6), "LF_dgreen2", "LF_dgrey2",
    rep("LF_dgreen", 4)))

# Give a color for each group and link
my_color <- 'd3.scaleOrdinal() .domain(["LF_grey","RD_green", "Total", "LF_dgrey2", 
"RD_dgreen","LF_dgreen2","LF_dgrey", "LF_dgreen"]) .range(["#CACACA","#a0e2ce", "#F59F00","#666666", "#12B886", "#095c43","#666666", "#095c43"])'

## Add text to label - dont run for website version
links$per <- paste0(links$value, "%")
nodes$txt<- c(paste(as.vector(nodes$name[1]),'3.1 million tons', sep = ": "),
              paste(as.vector(nodes$name[2:nrow(nodes)]),
                    links$per, sep = ": "))

#move labels to the left
Middle <- as.vector(nodes$txt[1:8])

# Make the Network
p <- sankeyNetwork(Links = links, Nodes = nodes,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "value", NodeID = "txt", 
                   sinksRight=FALSE, fontSize = 14, 
                   fontFamily = "Open Sans",
                   nodeWidth = 15 , colourScale = my_color,
                   LinkGroup = "group", NodeGroup = "group",
                   nodePadding = 20, width=800, height=650,
                   margin = list("right"=200))


p

### remove value from repeating
p <-  onRender(
    p,
    paste0('
        function(el,x){
        
var link = d3.selectAll(".link");
link.select("title").select("body")
        .html(function(d) { return "<pre>" + d.source.name + " \u2192 " + d.target.name +
           "<pre>"; });

        }
        ')
  )
#move label to the left
p <-
  onRender(
    p,
    paste0('
        function(el,x){
        d3.select(el)
        .selectAll(".node text")
        .filter(function(d) { return (["',paste0(Middle,collapse = '","'),'"].indexOf(d.name) > -1);})
        .attr("x", -20 + x.options.nodeWidth)
        .attr("text-anchor", "end");
        }
        ')
  )

p
p <- htmlwidgets::prependContent(p, htmltools::tags$div(HTML(
  paste(tags$span(style="font-family:Georgia;font-size:16px;font-weight: bold;", "NYC's Residential Waste Composition & Potential for Landfill Diversion"), sep = ""))))
p<-htmlwidgets::appendContent(p,htmltools::tags$div(
  HTML(paste(tags$span(style="font-family:Open Sans; font-size:12px; font-style: italic;", "DSNY Monthly Tonnage Data, 2017 NYC Residential, School, and NYCHA Waste Characterization Study"), 
             "<br>", 
             tags$span(style="font-family:Open Sans; font-size:10px;", "*Other: Items that currently have no or limited options for beneficial reuse"), sep = ""))))

p
saveWidget(p,file=paste0(getwd(),"/visuals/waste_flow.html"))
webshot(url=paste0(getwd(),"/visuals/waste_flow.html"), 
        file=paste0(getwd(),"/visuals/waste_flow.png"), 
        delay = 0.5, debug = TRUE, zoom = 2, vheight = 650, vwidth = 800)



## waste over time normalized by pop ---------
w2000<-w[w$year>=2000,]
w2000 <- w2000 %>%
  group_by(year) %>%
  summarize(Landfill = round(sum(refusetonscollected,na.rm = TRUE),2), 
            Paper = round(sum(papertonscollected,na.rm = TRUE),2),
            `Metal, Glass, Plastics`= round(sum(mgptonscollected,na.rm = TRUE),2),
            `Xmas Trees`= round(sum(xmastreetons,na.rm = TRUE),2),
            `Res. Organics`= round(sum(resorganicstons,na.rm = TRUE),2),
            `School Organics`= round(sum(schoolorganictons,na.rm = TRUE),2),
            `Leaves Organics`= round(sum(schoolorganictons,na.rm = TRUE),2)
  )
w2000$total<-round(rowSums(w2000[,-1]))

#add pop numbers
pop <- data.frame(
  Year = c(1984, 1985, 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993,
           1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003,
           2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014,
           2015, 2016, 2017, 2018, 2019),
  NYC_Pop = c(7234514, 7274054, 7319246, 7342476, 7353719, 7344175, 7322564,
              7374501, 7428944, 7506166, 7570458, 7633040, 7697812, 7773443,
              7858259, 7947660, 8017608, 8059813, 8072000, 8068073, 8043366,
              8013368, 7993906, 8013775, 8068195, 8131574, 8175133, 8244910,
              8336697, 8405837, 8491079, 8550405, 8537673, 8622698, 8398748,
              8560072)
)

#leave out 2020 not complete year yet
w2000=w2000[-nrow(w2000),]
w2000$pop <- pop[pop$Year>=2000,]$NYC_Pop
names(w2000)[c(1,9)]<-c('Year','Tonnage')
w2000$Rate <-round((w2000$Tonnage/w2000$pop)*100,2)

#plot
p <- w2000 %>%
  ggplot(aes(x=Year, y=Rate, group = 1, label = Tonnage, text = paste0('Rate: ',Rate, '%'))) + 
  xlab("Year") + ylab("Tonnage/Population (%)") +
  geom_point(shape=21, color="#2F56A6", fill="#2F56A6", size=2) +
  geom_line(color="#2F56A6", size=0.8) +
  #geom_smooth(se=FALSE, fullrange=TRUE,color="#2F56A6", method = 'lm') +
  scale_x_continuous(waiver(), 
                     breaks = as.numeric(w2000$Year), 
                     labels = as.numeric(w2000$Year), 
                     limits=c(2000,2019)) +
  scale_y_continuous(limit=c(30,45),
                     oob=squish,
                     breaks=waiver(),
                     labels=waiver())+
  theme_ipsum(axis_title_just = "mc", 
              base_size = 8,
              axis_title_size = 11,
              axis_text_size = 10) +
  theme(legend.position="none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        text = element_text(family = "Open Sans"),
        plot.title = element_text(family = "Georgia",size = 14),
        axis.text.x = element_text(angle = 90),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
 # annotate(geom="text", x=as.Date("2017-01-01"), y=20089, 
  #         label="Bitcoin price reached 20k $\nat the end of 2017") +
  #annotate(geom="point", x=as.Date("2017-12-17"), y=20089, size=10, shape=21, fill="transparent") 
ggtitle("NYC Residential Waste Rate from 2000 to 2019") +
  labs(caption = "DSNY Monthly Tonnage, ACS 1-Year Estimates & Census")


htmlwidgets::saveWidget(ggplotly(p, tooltip = c('x', 'text', 'label')), paste0(getwd(),"/docs/nycwaste.html"))

#updating curbside oragnics pickup ------
# Curbside Pickup Mapping -------------------------------------------------


# source: https://data.cityofnewyork.us/City-Government/Community-Districts/yfnk-k7r4
cds <- read_sf(paste0(getwd(), '/data/Community_Districts.geojson')) %>% 
  st_simplify(dTolerance = 0.001) %>% 
  select(boro_cd, geometry)

# sources: https://www1.nyc.gov/assets/dsny/site/services/food-scraps-and-yard-waste-page/residents/current-organics-rollout,
# https://www1.nyc.gov/assets/dsny/site/services/food-scraps-and-yard-waste-page/2018-organics-rollout
manhattan <- rep(101:112)
bronx <- rep(201:212)
brooklyn <- rep(301:318)
queens <- rep(401:414)
staten_island <- rep(501:503)


yes <- c(208, 210:212, 301, 302, 306, 307, 310:313, 315, 316, 402, 405, 407:411, 413, 414)

enroll <- c(101:112, 201:207, 209)

partial <- 501

partial_text <- "Partial Availability" #"Available in Castleton Corners, Graniteville, Mariners Harbor, Port Richmond, West Brighton, Westerleigh"


curbside_pickup <- tibble(
  boro_cd = c(manhattan, bronx, brooklyn, queens, staten_island)
) %>% 
  mutate(curbside_pickup_status = ifelse(boro_cd %in% yes,
                                         "Available",
                                         ifelse(boro_cd %in% enroll,
                                                "Available with Enrollment",
                                                ifelse(boro_cd == partial,
                                                       partial_text,
                                                       "Not Available"))))
#matching boros
curbside_pickup$boro_cd=as.character(curbside_pickup$boro_cd)

#adding waste info for each cd
w2019<-w[w$year>=2019,]
#add cd column
w2019$boro_cd<-paste0(w2019$borough_id, w2019$communitydistrict)
w2019 <- w2019 %>%
  group_by(year, boro_cd) %>%
  summarize(Landfill = round(sum(refusetonscollected,na.rm = TRUE)), 
            Paper = round(sum(papertonscollected,na.rm = TRUE)),
            `Metal, Glass, Plastics`= round(sum(mgptonscollected,na.rm = TRUE)),
            `Xmas Trees`= round(sum(xmastreetons,na.rm = TRUE)),
            `Res. Organics`= round(sum(resorganicstons,na.rm = TRUE)),
            `School Organics`= round(sum(schoolorganictons,na.rm = TRUE)),
            `Leaves Organics`= round(sum(schoolorganictons,na.rm = TRUE))
  )

curbside_pickup2<- left_join(curbside_pickup, w2019) 


# join and get rid of non-cd geometry
curbside_pickup_shape <- left_join(cds, curbside_pickup2) %>% 
  filter(!is.na(curbside_pickup_status)) %>% 
  st_transform('+proj=longlat +datum=WGS84')



curbside_pop <- paste0("<h3><font face='Open Sans'><strong>Community Board:</strong> ", 
                       curbside_pickup_shape$boro_cd, "<br>",
              "<strong>Curbside Pickup:</strong> ", curbside_pickup_shape$curbside_pickup_status, 
              "</h3>",
              "<strong>Landfill:</strong> ", curbside_pickup_shape$Landfill, " tons (2019)", "<br>",
              "<strong>Paper:</strong> ", curbside_pickup_shape$Paper, " tons (2019)", "<br>",
              "<strong>Metal, Glass, Plastics:</strong> ", 
              curbside_pickup_shape$`Metal, Glass, Plastics`, " tons (2019)", "<br>",
              "<strong>Xmas Trees:</strong> ", curbside_pickup_shape$`Xmas Trees`, 
              " tons (2019)", "<br>",
              "<strong>Res. Organics:</strong> ", curbside_pickup_shape$`Res. Organics`, 
              " tons (2019)", "<br>",
              "<strong>School Organics:</strong> ", curbside_pickup_shape$`School Organics`, 
              " tons (2019)", "<br>",
              "<strong>Leaves Organics:</strong> ", curbside_pickup_shape$`Leaves Organics`, 
              " tons (2019)</font>")

curbside_pal <- colorFactor(palette = c("#007534", "#1D5FD6","#B63F26", "#846126"),
  levels = unique(curbside_pickup_shape$curbside_pickup_status))


html_legend <- "<small>Source: DSNY Organics Rollout & Monthly Tonnage Data</small>"


lf <-
  leaflet(curbside_pickup_shape) %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(color = ~curbside_pal(curbside_pickup_status), popup = curbside_pop,
              weight = .8, label = curbside_pickup_shape$boro_cd) %>% 
  addLegend(pal = curbside_pal, values = curbside_pickup_shape$curbside_pickup_status,
            position = "topright", title = "Curbside Composting") %>% 
  addControl(html = html_legend, position = "bottomright") %>% 
  setView(lng = -73.912285, lat = 40.694678, zoom = 11)


saveWidget(lf, file=paste0(getwd(),"/docs/curbside_composting.html"))


# for gqis mapping ------

library(raster)
library(rgdal) # for spTransform
library(sf)
library(tidyverse)
library(lubridate)
library(leaflet)
library(ggplot2)
#remotes::install_github("jeffreyevans/spatialEco")
library(spatialEco)
options(scipen = 999)
#below 3% cloud coverage

july_10_18_tif <- '/Users/romartinez/Network Shares/geodata/DATABASE/Landsat/landsat_final_used_values copy/LC08_CU_029007_20180710_20190614_C01_V01_ST.tif'

august_30_19_tif <- '/Users/romartinez/Network Shares/geodata/DATABASE/Landsat/landsat_final_used_values copy/LC08_CU_029007_20190830_20190919_C01_V01_ST.tif'

sept_22_19_tif <- '/Users/romartinez/Network Shares/geodata/DATABASE/Landsat/landsat_final_used_values copy/LC08_CU_029007_20190922_20191001_C01_V01_ST.tif'

nyc <-st_read("/Users/romartinez/Desktop/untitled/Data_Products/snap_map/data/Borough Boundaries.geojson") %>%
  st_transform("+proj=longlat +datum=WGS84")


# Create data
august_30_19_raster <- raster(august_30_19_tif)
july_10_18_raster <- raster(july_10_18_tif)
sept_22_19_raster <- raster(sept_22_19_tif)



#reproject nyc polygon to raster projection
nyc1 <- st_transform(nyc, projection(august_30_19_raster))


#crop & mask the raster files to poylgon extent/boundary
august_30_19_masked <- mask(august_30_19_raster, nyc1)
august_30_19_cropped <- crop(august_30_19_masked, nyc1)


july_10_18_masked <- mask(august_30_19_raster, nyc1)
july_10_18_cropped <- crop(july_10_18_masked, nyc1)

sept_22_19_masked <- mask(august_30_19_raster, nyc1)
sept_22_19_cropped <- crop(sept_22_19_masked, nyc1)

#not coverting to sf
s <- stack(august_30_19_cropped, july_10_18_cropped, sept_22_19_cropped)
#getting the median values into a raser and saving them
med_raster <- calc(s, fun = median, na.rm = T)
writeRaster(med_raster,filename = paste0(getwd(), '/data/med_raster.tif'))
#copy
md<-med_raster
# centering the values, zscore, saving file
md@data@values<- scale(md@data@values)
writeRaster(md,filename = paste0(getwd(), '/data/md_raster.img'))
plot(md)

# theme setting ---------------
# theme_ipsum(base_family = "Arial Narrow", base_size = 11.5,
#             plot_title_family = base_family, plot_title_size = 18,
#             plot_title_face = "bold", plot_title_margin = 10,
#             subtitle_family = base_family, subtitle_size = 12,
#             subtitle_face = "plain", subtitle_margin = 15,
#             strip_text_family = base_family, strip_text_size = 12,
#             strip_text_face = "plain", caption_family = base_family,
#             caption_size = 9, caption_face = "italic", caption_margin = 10,
#             axis_text_size = base_size, axis_title_family = subtitle_family,
#             axis_title_size = 9, axis_title_face = "plain",
#             axis_title_just = "rt", plot_margin = margin(30, 30, 30, 30),
#             grid_col = "#cccccc", grid = TRUE, axis_col = "#cccccc",
#             axis = FALSE, ticks = FALSE)
