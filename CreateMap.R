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

# Merge data
data<-read.csv("data.csv") %>%
  mutate(share=(Exports+Imports)/GDP)

# Centroids for labels
centroids<-read.csv("centroids.csv",stringsAsFactors=FALSE)

# Format the data
plotdata<-tibble(id=ca@data[,5]) %>%
  distinct(id,Longitude,Latitude) %>%
  left_join(data,by="id") %>%
  left_join(centroids,by="id") %>%
  mutate(Rounded=round(share*100),
         gdpcap_2017=1000*(gdp_2017/cadusd_2017)/pop_2017,
         gdpcap_2017ppp=1000*(gdp_2017/ppp_2016)/pop_2017) %>%
  filter(!(id %in% c("US-HI","US-AK")) & !is.na(share))

#################
# Canada + US48 #
#################
ggplot() + geom_map(data=plotdata,aes(map_id=id,fill=share),map=ca_map,color="white") +
  expand_limits(x=c(-130,-59),y=c(28,60)) +
  coord_map("albers",lat0=40, lat1=60)+
  scale_fill_continuous(low = "#eff3ff",high = "dodgerblue3", limits = c(0,0.50),
    guide_legend(title="CAN-USA Two-Way Trade as % of GDP : "),labels=percent, breaks=c(0,0.25,0.50)) +
  theme(
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.y=element_blank(),
    axis.ticks.x=element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    legend.position="none",
    #legend.text=element_text(size=10),
    plot.title = element_text(size = 16, face = "bold",hjust=0.5),
    plot.subtitle = element_text(size = 7, color="gray50",hjust=0.5),
    plot.margin = unit(c(-2,-2,-2,-1), "cm")
  ) +
  geom_text_repel(data=plotdata %>% filter(!id %in% c("US-DE","US-NH","US-RI","US-MA","US-NJ","US-MD")),
                  aes(label = paste(Rounded,"%",sep=""), x = Longitude, y = Latitude),
                  point.padding = unit(0,"cm"), box.padding = unit(0.1,"cm"),fontface="bold",size=2.5) +
  geom_text_repel(data=plotdata %>% filter(id %in% c("US-DE","US-NH","US-RI","US-MA","US-NJ","US-MD")),
                  xlim=c(0.32,0.32),aes(label = paste(Rounded,"%",sep=""), x = Longitude, y = Latitude),
                  point.padding = unit(0,"cm"), box.padding = unit(0.1,"cm"),fontface="bold",size=2.5,
                  segment.color = "gray80",segment.size = 0.25) +
  labs(x="",y="",title="The Importance of Canada-USA Trade, by Province/State (2016)",
       subtitle="Note: Own calculations using data from Statistics Canada, Industry Canada,
the U.S. Census Bureau, and the Bureau of Economic Analysis. Graph by @trevortombe.")
ggsave("map.png",width=8,height=6.25,dpi=300)

######################
# GDP/Capita in 2017 #
######################
ggplot() + geom_map(data=plotdata,aes(map_id=id,fill=gdpcap_2017ppp),map=ca_map,color="white") +
  expand_limits(x=c(-130,-59),y=c(28,60)) +
  coord_map("albers",lat0=40, lat1=60)+
  scale_fill_continuous(low = "#eff3ff",high = "dodgerblue3") +
  theme(
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.y=element_blank(),
    axis.ticks.x=element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    legend.position="none",
    #legend.text=element_text(size=10),
    plot.title = element_text(size = 16, face = "bold",hjust=0.5),
    plot.subtitle = element_text(size = 7, color="gray50",hjust=0.5),
    plot.margin = unit(c(-2,-2,-2,-1), "cm")
  ) +
  geom_text_repel(data=plotdata %>% filter(!id %in% c("US-DE","US-NH","US-RI","US-MA","US-NJ","US-MD")),
                  aes(label = paste("$",round(gdpcap_2017ppp),sep=""), x = Longitude, y = Latitude),
                  point.padding = unit(0,"cm"), box.padding = unit(0.1,"cm"),fontface="bold",size=2.5) +
  geom_text_repel(data=plotdata %>% filter(id %in% c("US-DE","US-NH","US-RI","US-MA","US-NJ","US-MD")),
                  xlim=c(0.32,0.32),aes(label = paste("$",round(gdpcap_2017ppp),sep=""), x = Longitude, y = Latitude),
                  point.padding = unit(0,"cm"), box.padding = unit(0.1,"cm"),fontface="bold",size=2.5,
                  segment.color = "gray80",segment.size = 0.25) +
  labs(x="",y="",title="GDP per Capita in 2017, by Province/State (000s USD, PPP)",
       subtitle="Note: Own calculations using data from Statistics Canada, the U.S. Census Bureau, and the Bureau
of Economic Analysis. Real dollars, PPP adjusted. Graph by @trevortombe.")
ggsave("map.png",width=8,height=6.25,dpi=300)

###############
# Just Canada #
###############
plotdata_canada<-plotdata %>%
  filter(grepl("CA-",id))
ggplot() + geom_map(data=plotdata_canada,aes(map_id=id,fill=share),map=ca_map,color="white") +
  expand_limits(x=c(-130,-50),y=c(44,60)) +
  coord_map("albers",lat0=40, lat1=60)+
  scale_fill_continuous(low = "#eff3ff",high = "dodgerblue3", limits = c(0,0.50),
                        guide_legend(title="CAN-USA Two-Way Trade as % of GDP : "),labels=percent, breaks=c(0,0.25,0.50))+
  theme(
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.y=element_blank(),
    axis.ticks.x=element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    legend.position="none",
    #legend.text=element_text(size=10),
    plot.title = element_text(size = 16, face = "bold",hjust=0.5),
    plot.subtitle = element_text(size = 7, color="gray50",hjust=0.5),
    plot.margin = unit(c(-2,-2,-2,-1), "cm")
  ) +
  geom_text_repel(data=plotdata_canada,
                  aes(label = paste(Rounded,"%",sep=""), x = Longitude, y = Latitude),
                  point.padding = unit(0,"cm"), box.padding = unit(0.1,"cm"),fontface="bold",size=3) +
  labs(x="",y="",title="The Importance of Canada-USA Trade, by Province/State (2016)",
       subtitle="Note: Own calculations using data from Statistics Canada, Industry Canada,
       the U.S. Census Bureau, and the Bureau of Economic Analysis. Graph by @trevortombe.")
ggsave("map.png",width=8,height=4.5,dpi=300)

