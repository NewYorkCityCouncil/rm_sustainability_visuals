library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(scales)
library(weathermetrics)

options(scipen = 999)
# SOC SUSTAINABILITY - BACKGROUND SECTION: EVIDENCE OF CLIMATE CHANGE --------------

# global temp rise ----------

glotemp=read.delim("https://data.giss.nasa.gov/gistemp/graphs/graph_data/Global_Mean_Estimates_based_on_Land_and_Ocean_Data/graph.txt", sep = "", header = FALSE, stringsAsFactors = FALSE)
glotemp=glotemp[-c(1:5),-4]
names(glotemp)<-c("Year", "No_Smoothing", "Lowess")
glotemp$farenheit_ns <- celsius.to.fahrenheit(as.numeric(as.character(glotemp$No_Smoothing)))-32

row.names(glotemp) <- NULL

glotemp$Year<-as.numeric(glotemp$Year)
# glotemp$label<-rep("", nrow(glotemp))
# glotemp[which(glotemp$Year %in% seq(1890,2010, 10)),]$label <- seq(1890,2010, 10)
# glotemp[1,]$label<-1881
# glotemp[139,]$label<-2019


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
  labs(tag = "Figure 2.", caption = "NASA's Goddard Institute for Space Studies (GISS)")

ggsave("Global Temperature Rise.png", plot = p, path = "/Users/romartinez/Desktop/sshfs/rm_sustainability_visuals/visuals/", width = 8.5, height = 5, units = "in", dpi = 300)


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
grlicemass=read.delim("/Users/romartinez/Desktop/sshfs/rm_sustainability_visuals/data/greenland_mass_200204_201911.txt", sep = "", header = FALSE)

grlicemass=grlicemass[-c(1:36),-c(4:20)]
names(grlicemass)<- c("Year", "Mass", "Uncertainty")
grlicemass$Mass <-as.numeric(as.character(grlicemass$Mass))
grlicemass$Year <-as.integer(as.character(grlicemass$Year))
row.names(grl) <- NULL

# grlicemass$label<-rep("", nrow(grlicemass))
# grlicemass[c(seq(7,67,12),76,seq(87,122,9),seq(123,133,5)),]$label <-seq(2006,2019, 1)
# grlicemass[1,]$label<-2005


#plot
p <- grlicemass %>%
  ggplot(aes(x=Year, y=Mass)) + 
  xlab("Year") + ylab("Mass (Gt)")+
  #geom_line(color="grey") +
  #geom_text(# Filter data first
  #  aes(label=label), nudge_y = 50) +
  geom_point(shape=21, color="#2F56A6", fill="#2F56A6", size=1) +
  scale_x_continuous(waiver(), 
                     breaks = as.numeric(grlicemass$label), 
                     labels = as.numeric(grlicemass$label), limits=c(2005,2019)) +
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
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.text.x = element_text(angle = 90))+
  ggtitle("Greenland Ice Mass Variaton Since 2002",
          "Ice Mass Loss of 283.0 Gigatonnes per year") +
  labs(tag = "Figure 3.", caption = "Ice mass measurement by NASA's GRACE satellites.")

ggsave("Greenland Ice Mass.png", plot = p, path = "/Users/romartinez/Desktop/sshfs/rm_sustainability_visuals/visuals/", width = 8.5, height = 5, units = "in", dpi = 300)



## sea level satellite observations --------------

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
# HDR* 12 smoothed (60-day Gaussian type filter) GMSL (GIA applied) variation (mm); annual and semi-annual signal removed )  with respect to 20-year mean
satsealevel=read.delim("/Users/romartinez/Desktop/sshfs/rm_sustainability_visuals/data/GlobalMeanSeaLevel_TPJAOS_4.2_199209_201911.txt", sep = "", header = FALSE)

satsealevel=satsealevel[-c(1:54), -c(13:20)]
names(satsealevel)<- c("altimeter_type", "filecyclenum", "Year_Decimal", "Observations", "Weights_Obs", "GMSL_nGIA", "SD_GMSL_nGIA", "GMSL_sm_nGIA", "GMSL_wGIA", "SD_GMSL_wGIA", "GMSL_sm_wGIA","add_GMSL_sm_wGIA")

satsealevel$Year= substr(satsealevel$Year_Decimal, 1, 4)
satsealevel$GMSL_wGIA=as.numeric(as.character(satsealevel$GMSL_wGIA))

mean(satsealevel[which(satsealevel$Year=="1993"),]$GMSL_wGIA)

for (i in 1:nrow(satsealevel)) {
  satsealevel$firstcycle1993[i]= (as.numeric(as.character(satsealevel$GMSL_wGIA[i+1])) - as.numeric(as.character(satsealevel$GMSL_wGIA[i])))/as.numeric(as.character(satsealevel$GMSL_wGIA[i])) *100
}
# need help with calculation -----------

#plot
p <- satsealevel %>%
  ggplot(aes(x=Year_Decimal, y=Mass)) + 
  xlab("Year") + ylab("Sea Height Variation (mm)")+
  geom_line(color="grey") +
  # geom_text(# Filter data first
  #  aes(label=label), nudge_y = 50) +
  geom_point(shape=21, color="#2F56A6", fill="#2F56A6", size=1) +
  geom_smooth(se=FALSE, fullrange=TRUE,color="#2F56A6") +
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
  ggtitle("Sea Level Rise",
          paste("Global Sea Level Mean Rising 3.3 mm per year")) +
  labs(tag = "Figure 3.", caption = "Satellite sea level observations, NASA Goddard Space Flight Center.")

ggsave("Sea Level Satellite Observations.png", plot = p, path = "/Users/romartinez/Desktop/sshfs/rm_sustainability_visuals/visuals/", width = 8.5, height = 5, units = "in", dpi = 300)






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
  labs(tag = "Figure 5.", caption = "Satellite observations, NSIDC/NASA")

ggsave("Artic Sea Ice Min.png", plot = p, path = "/Users/romartinez/Desktop/sshfs/rm_sustainability_visuals/visuals/", width = 8.5, height = 5, units = "in", dpi = 300)





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
