# let's look at James data and map 
library(readxl)
library(data.table)
library(ggplot2)
library(sf)
library(dplyr)
library(heatmap3)
library(leaflet)

svi <- read_excel("data/Vulnerability_Indicators.xlsx")

bus <- read_excel("Business_Histogram.xlsx", 
                  sheet = "Indicators")
cd <- read_sf("data/cd_shape/")
setDT(svi)
svi[, fp_ind := `Homes (Units) in the 2050 1% Floodplain` * `Social Vulnerability Index`]
svi[, fp := fp_ind/1000]
setnames(svi, "CB Code","boro_cd")
svi[boro_cd %in% cd$boro_cd, ]
svi_sf <- left_join(svi, cd, by = "boro_cd") %>% st_as_sf() %>% st_transform("+proj=longlat +datum=WGS84")

# mapview::mapview(svi_sf, z = "Social Vulnerability Index")
# mapview::mapview(svi_sf, z ="fp")


ggplot(svi, aes(x=fp)) + geom_histogram() + theme_bw()
summary(svi$fp)
plot(ecdf(svi$fp))

# two figures scatterplot & map 
pal = colorNumeric(
  palette = "Reds",
  domain = svi$fp, 
  na.color = "White", 
  reverse = FALSE
)

leaflet(svi_sf) %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(weight = .5, 
              fillColor = ~pal(fp), 
              color = "white", 
              label = svi$boro_cd, 
              stroke = TRUE, 
              fillOpacity = .9) %>% 
  addLegend(position ="topleft", 
            pal = pal, 
            values = svi$fp, 
            title = "") 
svi[, label_num_cd := substr(boro_cd, 2, 3)]
svi[label_boro_cd %in% 1, label_boro_cd:= "MN"]
svi[label_boro_cd %in% 2, label_boro_cd:= "BX"]
svi[label_boro_cd %in% 3, label_boro_cd:= "BK"]
svi[label_boro_cd %in% 4, label_boro_cd:= "QN"]
svi[label_boro_cd %in% 5, label_boro_cd:= "SI"]
svi[, label := paste(label_boro_cd, label_num_cd, sep = "")]

## scatter 
p <- ggplot(svi, aes(x=`Homes (Units) in the 2050 1% Floodplain`, y=`Social Vulnerability Index`)) + geom_point(color = "#2F56A6") + 
  geom_text(data=subset(svi, `Homes (Units) in the 2050 1% Floodplain` > 10000 & `Social Vulnerability Index` >= .5),
            aes(`Homes (Units) in the 2050 1% Floodplain`,`Social Vulnerability Index`,label=label, hjust= 1, vjust = -1),  size=3) + 
  theme_bw() + theme(legend.position="none",
                                      panel.grid.major.x = element_blank(),
                                      panel.grid.minor.x = element_blank(),
                                      text = element_text(family = "Open Sans"),
                                     #  plot.title = element_text(family = "Georgia",size = 14),
                                      axis.text.x = element_text(angle = 90),
                                      axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
                                      axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))
  

ggsave("/Users/bfrye/Desktop/SOC/paper_viz/fp_svi_scatter.png", dpi = 1000)

q <- ggplot(bus, aes(`Office, Retail, and Industrial Space in the Future Floodplain (Sqft)`)) + geom_histogram(fill = "#2F56A6")  + 
  scale_x_continuous(labels = scales::comma) + 
  theme_bw() + theme(legend.position="none",
                     panel.grid.major.x = element_blank(),
                     panel.grid.minor.x = element_blank(),
                     text = element_text(family = "Open Sans"),
                     #  plot.title = element_text(family = "Georgia",size = 14),
                     # axis.text.x = element_text(angle = 90),
                     axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
                     axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))



ggsave("/Users/bfrye/Desktop/SOC/paper_viz/fp_oris_hist.png", dpi = 1000)


pal = colorNumeric(
  palette = "Reds",
  domain = svi$fp, 
  na.color = "White", 
  reverse = FALSE
)

leaflet(svi_sf, options = leafletOptions(zoomControl = FALSE, minZoom = 10, maxZoom = 10)) %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(weight = .5, 
              fillColor = ifelse(svi_sf$boro_cd %in% cds, "#2F56A6", ""), 
              # color = "white", 
              # label = svi$boro_cd, 
              stroke = TRUE, 
              fillOpacity = ifelse(svi_sf$boro_cd %in% cds, .8, 0))



cb <- c(
"BK13—Coney Island", 
"BK15—Sheepshead Bay", 
"BK18—Canarsie",  
"QN10—Howard Beach", 
"QN14—The Rockaways", 
"BK01—Greenpoint/Williamsburg", 
"BK06—Red Hook/Gowanus", 
"BK07—Sunset Park", 
"QN02—Long Island City", 
"MN01—Lower Manhattan", 
"MN02—West Village", 
"MN03—Lower East Side", 
"MN04—East Midtown", 
"MN06—West Midtown"
, "BK01—Mott Haven"
, "BK02—Hunts Point"
, "MN10—Harlem" 
, "MN11—East Harlem"
, "QN01—Astoria"
, "QN07—Flushing" 
, "SI01—North Shore"
, "SI02—West Shore"
)
cb2 <- gsub("Queens", 4, cb)
cb2 <- gsub("Bronx", 2, cb)
cb2 <- gsub("Staten Island", 5, cb)
cb2 <- gsub("Manhattan", 1, cb)
cb2 <- gsub("Brooklyn", 2, cb)
cb2 <- gsub("CB", "", cb)

library(readr)
cds <- parse_number(cb)
