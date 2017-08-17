# Load the necessary libraries
library(ggplot2)
library(tidyverse)
library(maptools)
library(rgeos)
library(rgdal)
library(mapproj)
library(scales)
library(ggrepel)

# Load the shapefiles
ca <- readOGR("bound_p.shp")
ca_map <- fortify(ca, region="STATEABB")

# Merge data. Exports are from the provice (state) to the US (Canada).
# Imports are the reverse. All data are in millions of local currency units.
data<-read.csv("TradeData.csv") %>%
  mutate(share=(Exports+Imports)/GDP)

# Centroids for labels
centroids<-read.csv("centroids.csv",stringsAsFactors=FALSE)

# Format the data
plotdata<-tibble(id=ca@data[,5]) %>%
  left_join(data,by="id") %>%
  left_join(centroids,by="id") %>%
  distinct(id,share,Longitude,Latitude) %>%
  mutate(Rounded=round(share*100)) %>%
  filter(!(id %in% c("US-HI","US-AK")) & !is.na(share))

# Create the map
ggplot() + geom_map(data=plotdata,aes(map_id=id,fill=share),map=ca_map,color="white") +
  expand_limits(x=c(-130,-59),y=c(25,60)) +
  coord_map("albers",lat0=39, lat1=45)+
  scale_fill_continuous(low = "#eff3ff",high = "dodgerblue3", limits = c(0,0.50),
    guide_legend(title="CAN-USA Two-Way Trade as % of GDP : "),labels=percent, breaks=c(0,0.25,0.50)) +
  theme(
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.y=element_blank(),
    axis.ticks.x=element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    legend.position="top",
    legend.text=element_text(size=10),
    plot.title = element_text(size = 16, face = "bold",hjust=0.5),
    plot.subtitle = element_text(size = 8, face = "italic",hjust=0.5),
    plot.margin = unit(c(-2,-2,-2,-1), "cm")
  ) +
  geom_text_repel(data=plotdata,aes(label = paste(Rounded,"%",sep=""), x = Longitude, y = Latitude),
                  point.padding = unit(0,"cm"), box.padding = unit(0,"cm"),fontface="bold",size=3) + 
  labs(x="",y="",title="The Importance of Canada-USA Trade, by Province/State (2016)",
       subtitle="Note: Own calculations using data from Statistics Canada, Industry Canada,\n the U.S. Census Bureau, and the Bureau of Economic Analysis") +
  annotate(geom="text", x=-115, y=25, label="Graph by @trevortombe",size=2,fontface="italic")

# Save the map to a file
ggsave("map.png",width=8,height=8,dpi=300)


