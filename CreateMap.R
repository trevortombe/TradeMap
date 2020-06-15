# Load useful libraries and functions
source("core.R")

# Load the shapefiles
ca <- readOGR("bound_p.shp")
ca_map <- fortify(ca, region="STATEABB")

# Merge data
data<-read.csv("data.csv")

# Centroids for labels
centroids<-read.csv("centroids.csv",stringsAsFactors=FALSE)
new_centroids<-centroids %>%
  rbind(data.frame(id="US-AK",state="Alaska",Longitude=-140,Latitude=45,abbrev="AK")) %>%
  rbind(data.frame(id="US-HI",state="Hawaii",Longitude=-133,Latitude=32,abbrev="HI")) %>%
  mutate(Latitude=ifelse(id %in% c("CA-YT","CA-NU","CA-NT"),62,Latitude))

# Format the data
plotdata<-tibble(id=ca@data[,5]) %>%
  distinct(id) %>%
  left_join(data,by="id") %>%
  left_join(new_centroids,by="id") %>%
  filter(!is.na(Country)) %>%
  group_by(Country) %>%
  mutate(share=(Exports+Imports)/GDP,
         Rounded=round(share*100),
         gdpcap_2017=1000*(gdp_2017/cadusd_2017)/pop_2017,
         country_gdpcap=weighted.mean(gdpcap_2017,pop_2017),
         rel_gdpcap=gdpcap_2017/country_gdpcap,
         gdpcap_2017ppp=1000*(gdp_2017/ppp_2016)/pop_2017,
         gdpcap_2018ppp=1000*(gdp_2018/ppp_2018)/pop_2018,
         med_income_2015=round((medianHH_2016/ppp_2016)/1000),
         revPC=weighted.mean(fed_rev_2017,pop_2017),
         expPC=weighted.mean(fed_exp_2017,pop_2017),
         gap=fed_rev_2017/pop_2017-sum(fed_rev_2017)/sum(pop_2017)-fed_exp_2017/pop_2017+sum(fed_exp_2017)/sum(pop_2017),
         gap2018=fed_rev_2018/pop_2018-sum(fed_rev_2018)/sum(pop_2018)-fed_exp_2018/pop_2018+sum(fed_exp_2018)/sum(pop_2018),
         gapGDP=gap*pop_2017/GDP,
         label=substr(id,4,5)) %>%
  ungroup() %>%
  filter(!(id %in% c("CA-YT","CA-NU","CA-NT"))) %>%
  filter(!is.na(med_income_2015)) 

  filter(!(id %in% c("US-HI","US-AK")))
  
# Plot of revenue/spending differences across states/provinces
ggplot(plotdata,aes(rel_gdpcap-1,gapGDP))+
  geom_smooth(method = "lm",se=F,color="black",linetype="dashed")+
  geom_point(size=4,aes(color=Country))+
  geom_hline(yintercept=0,size=1)+
  geom_text_repel(aes(label=label),segment.colour = "gray50",segment.alpha = 0.5)+
  mytheme+
  theme(legend.position = c(0.9,0.2))+
  scale_color_brewer(name="",palette="Set1")+
  scale_y_continuous(label=percent,breaks=pretty_breaks(n=6),limit=c(NA,.1))+
  scale_x_continuous(label=percent)+
  labs(y="% of GDP",
       x="GDP/Capita Relative to the National Average",
       title="Net Federal Fiscal Outflows in 2017, by State/Province",
       subtitle="Note: Displays the differences between federal revenue and spending (as % of GDP), relative to a 
common per capita benchmark, against each state/provinces's relative GDP per capita.",
       caption="Source: Own calculations from Schultz and Cummings (2019) for the USA and Statistics
Canada data table 36-10-0450 and 36-10-0222 for Canada. Methodology in Tombe (2018). Graph by @trevortombe.")
ggsave('plot.png',width=7,height=5,dpi=200)

# Attempt to extract Alaska, hawaii
alaska <- ca[ca$STATEABB=="US-AK" & !is.na(ca$STATEABB),]
#alaska <- elide(alaska, rotate=-50)
alaska <- elide(alaska, scale=max(apply(bbox(alaska), 1, diff)) / 2.5)
alaska <- elide(alaska, shift=c(-150, 39))
proj4string(alaska) <- proj4string(ca)
hawaii <- ca[ca$STATEABB=="US-HI" & !is.na(ca$STATEABB),]
#hawaii <- elide(hawaii, rotate=-50)
hawaii <- elide(hawaii, scale=1.5*max(apply(bbox(hawaii), 1, diff)))
hawaii <- elide(hawaii, shift=c(-136, 30))
proj4string(hawaii) <- proj4string(ca)
test <- ca[ca$STATEABB!="US-AK" & ca$STATEABB!="US-HI" & !is.na(ca$STATEABB),]
test <- rbind(test,alaska,hawaii)
test2<-fortify(test,region="STATEABB")

ggplot() + geom_map(data=plotdata,aes(map_id=id,fill=med_income_2015),map=test2,color="white") +
  expand_limits(x=-100,y=50) +
  coord_map("albers",lat0=40, lat1=60,xlim=c(-135,-59),ylim=c(25,61))+
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
  )+
  annotate('rect',xmin=bbox(alaska)[1]+2,xmax=-129,ymin=bbox(alaska)[2],ymax=bbox(alaska)[4]+1,
           fill="transparent",color="gray",size=1,linetype="dotted")+
  annotate('rect',xmin=bbox(hawaii)[1],xmax=bbox(hawaii)[3]+1,ymin=bbox(hawaii)[2]-1,ymax=bbox(hawaii)[4]+1,
           fill="transparent",color="gray",size=1,linetype="dotted")+
  geom_text_repel(data=plotdata %>% filter(!id %in% c("US-DE","US-NH","US-RI","US-MA","US-NJ","US-MD")),
                  aes(label = paste("$",round(med_income_2015),sep=""), x = Longitude, y = Latitude),
                  point.padding = unit(0,"cm"), box.padding = unit(0.1,"cm"),fontface="bold",size=3) +
  geom_text_repel(data=plotdata %>% filter(id %in% c("US-DE","US-NH","US-RI","US-MA","US-NJ","US-MD")),
                  xlim=c(0.37,0.37),aes(label = paste("$",round(med_income_2015),sep=""), x = Longitude, y = Latitude),
                  point.padding = unit(0,"cm"), box.padding = unit(0.1,"cm"),fontface="bold",size=3,
                  segment.color = "gray80",segment.size = 0.25) +
  labs(x="",y="",title="Median Household Income in 2015 (000s USD, PPP)",
       subtitle="Note: Own calculations using data from Statistics Canada (Census 2016) and the U.S. Census Bureau. 
       Real dollars, PPP adjusted. Graph by @trevortombe.")
ggsave("map.png",width=8,height=6.25,dpi=300)

# Fiscal Transfers in 2017, by State/Province
ggplot() + geom_map(data=plotdata %>% mutate(status=ifelse(gap>0,"giver","getter")),
                    aes(map_id=id,fill=status),
                    map=test2,color="white") +
  expand_limits(x=-100,y=50) +
  coord_map("albers",lat0=40, lat1=60,xlim=c(-135,-59),ylim=c(25,61))+
  #scale_fill_gradient2(low = "#FF2700",mid="beige",high = "#2b83ba",
  #                     midpoint = 0)+
  scale_fill_manual(name="",values=c("#FF2700","#008FD5"),
                    label=c("Net Recipient","Net Contributor"))+
    theme(
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.y=element_blank(),
    axis.ticks.x=element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    legend.position=c(0.8,0.2),
    #legend.text=element_text(size=10),
    plot.title = element_text(size = 16, face = "bold",hjust=0.5),
    plot.subtitle = element_text(size = 7, color="gray50",hjust=0.5),
    plot.margin = unit(c(-2,-2,-2,-1), "cm")
  )+
  annotate('rect',xmin=bbox(alaska)[1]+2,xmax=-129,ymin=bbox(alaska)[2],ymax=bbox(alaska)[4]+1,
           fill="transparent",color="gray",size=1,linetype="dotted")+
  annotate('rect',xmin=bbox(hawaii)[1],xmax=bbox(hawaii)[3]+1,ymin=bbox(hawaii)[2]-1,ymax=bbox(hawaii)[4]+1,
           fill="transparent",color="gray",size=1,linetype="dotted")+
  geom_text_repel(data=plotdata %>% filter(!id %in% c("US-DE","US-NH","US-RI","US-MA","US-NJ","US-MD")),
                  aes(label = paste("$",round(1000*gap/cadusd_2017,1),"k",sep=""), x = Longitude, y = Latitude),
                  point.padding = unit(0,"cm"), box.padding = unit(0.1,"cm"),fontface="bold",size=2.75) +
  geom_text_repel(data=plotdata %>% filter(id %in% c("US-DE","US-NH","US-RI","US-MA","US-NJ","US-MD")),
                  xlim=c(0.37,0.37),aes(label = paste("$",round(1000*gap/cadusd_2017,1),"k",sep=""), x = Longitude, y = Latitude),
                  point.padding = unit(0,"cm"), box.padding = unit(0.1,"cm"),fontface="bold",size=2.75,
                  segment.color = "gray80",segment.size = 0.25) +
  labs(x="",y="",title="Per Capita Federal Fiscal Gaps in Canada and the USA (2017)",
       subtitle="Note: Displays the difference between federal revenue and spending in each region ($000/capita USD), relative to a common per capita benchmark.
Net \"outflows\" imply federal revenue exceeds federal spending. Source: Own calculations from Schultz and Cummings (2019) for the USA and Statistics
Canada data table 36-10-0450 for Canada. Methodology in Tombe (2018, Canadian Tax Journal).")
ggsave("map.png",width=8,height=6.25,dpi=300)

# GDP per Capita
ggplot() + geom_map(data=plotdata,aes(map_id=id,fill=gdpcap_2018ppp),map=test2,color="white") +
  expand_limits(x=-100,y=50) +
  coord_map("albers",lat0=40, lat1=60,xlim=c(-135,-59),ylim=c(25,61))+
  scale_fill_continuous(low = "#e1f0ff",high = "dodgerblue3") +
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
  )+
  annotate('rect',xmin=bbox(alaska)[1]+2,xmax=-129,ymin=bbox(alaska)[2],ymax=bbox(alaska)[4]+1,
           fill="transparent",color="gray",size=1,linetype="dotted")+
  annotate('rect',xmin=bbox(hawaii)[1],xmax=bbox(hawaii)[3]+1,ymin=bbox(hawaii)[2]-1,ymax=bbox(hawaii)[4]+1,
           fill="transparent",color="gray",size=1,linetype="dotted")+
  geom_text_repel(data=plotdata %>% filter(!id %in% c("US-DE","US-NH","US-RI","US-MA","US-NJ","US-MD")),
                  aes(label = paste("$",round(gdpcap_2018ppp),sep=""), x = Longitude, y = Latitude),
                  point.padding = unit(0,"cm"), box.padding = unit(0.1,"cm"),fontface="bold",size=3) +
  geom_text_repel(data=plotdata %>% filter(id %in% c("US-DE","US-NH","US-RI","US-MA","US-NJ","US-MD")),
                  xlim=c(0.37,0.37),aes(label = paste("$",round(gdpcap_2018ppp),sep=""), x = Longitude, y = Latitude),
                  point.padding = unit(0,"cm"), box.padding = unit(0.1,"cm"),fontface="bold",size=3,
                  segment.color = "gray80",segment.size = 0.25) +
  labs(x="",y="",title="GDP per Capita in 2018 (000s USD, PPP)",
       subtitle="Note: Own calculations using data from Statistics Canada data table 36-10-0222 and the US BEA. 
       All values are in real PPP-adjusted US dollars, based on 36-10-0100. Graph by @trevortombe.")
ggsave("map.png",width=8,height=6.25,dpi=300)

#########################
# Including Territories #
#########################
new_centroids<-centroids %>%
  rbind(data.frame(id="US-AK",state="Alaska",Longitude=-151,Latitude=64.5,abbrev="AK")) %>%
  rbind(data.frame(id="US-HI",state="Hawaii",Longitude=-142,Latitude=36,abbrev="HI"))
plotdata<-tibble(id=ca@data[,5]) %>%
  distinct(id,Longitude,Latitude) %>%
  left_join(data,by="id") %>%
  left_join(new_centroids,by="id") %>%
  mutate(share=(Exports+Imports)/GDP,
         Rounded=round(share*100),
         gdpcap_2017=1000*(gdp_2017/cadusd_2017)/pop_2017,
         gdpcap_2017ppp=1000*(gdp_2017/ppp_2016)/pop_2017,
         gdpcap_2018ppp=1000*(gdp_2018/ppp_2018)/pop_2018,
         med_income_2015=round((medianHH_2016/ppp_2016)/1000)) %>%
  filter(!is.na(med_income_2015)) 
hawaii <- ca[ca$STATEABB=="US-HI" & !is.na(ca$STATEABB),]
hawaii <- elide(hawaii, scale=1.5*max(apply(bbox(hawaii), 1, diff)))
hawaii <- elide(hawaii, shift=c(-145, 35))
proj4string(hawaii) <- proj4string(ca)
test <- ca[ca$STATEABB!="US-HI" & !is.na(ca$STATEABB),]
test <- rbind(test,hawaii)
test2<-fortify(test,region="STATEABB")
ggplot() + geom_map(data=plotdata,aes(map_id=id,fill=med_income_2015),map=test2,color="transparent") +
  expand_limits(x=-100,y=50) +
  coord_map("albers",lat0=40, lat1=70,xlim=c(-140,-40),ylim=c(25,86))+
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
  )+
  annotate('rect',xmin=-146,xmax=-136,ymin=34,ymax=41,fill="transparent",color="gray",size=1,linetype="dotted")+
  #annotate('segment',x=-146,xend=-135,y=33,yend=33,color="white",size=2)+
  geom_text_repel(data=plotdata %>% filter(!id %in% c("US-DE","US-NH","US-RI","US-MA","US-NJ","US-MD")),
                  aes(label = paste("$",round(med_income_2015),sep=""), x = Longitude, y = Latitude),
                  point.padding = unit(0,"cm"), box.padding = unit(0.1,"cm"),fontface="bold",size=2.5) +
  geom_text_repel(data=plotdata %>% filter(id %in% c("US-DE","US-NH","US-RI","US-MA","US-NJ","US-MD")),
                  xlim=c(.35,.35),aes(label = paste("$",round(med_income_2015),sep=""), x = Longitude, y = Latitude),
                  point.padding = unit(0,"cm"), box.padding = unit(0.1,"cm"),fontface="bold",size=2.5,
                  segment.color = "gray80",segment.size = 0.25) +
  labs(x="",y="",title="Median Household Income in 2015 (000s USD, PPP)",
       subtitle="Note: Own calculations using data from Statistics Canada (Census 2016) and the U.S. Census Bureau. 
       Real dollars, PPP adjusted. Graph by @trevortombe.")
ggsave("map_all_NA.png",width=7,height=6.75,dpi=300)

ggplot() + geom_map(data=plotdata,aes(map_id=id,fill=gdpcap_2018ppp),map=test2,color="transparent") +
  expand_limits(x=-100,y=50) +
  coord_map("albers",lat0=40, lat1=70,xlim=c(-140,-40),ylim=c(25,86))+
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
  )+
  annotate('rect',xmin=-146,xmax=-136,ymin=34,ymax=41,fill="transparent",color="gray",size=1,linetype="dotted")+
  #annotate('segment',x=-146,xend=-135,y=33,yend=33,color="white",size=2)+
  geom_text_repel(data=plotdata %>% filter(!id %in% c("US-DE","US-NH","US-RI","US-MA","US-NJ","US-MD")),
                  aes(label = paste("$",round(gdpcap_2018ppp),sep=""), x = Longitude, y = Latitude),
                  point.padding = unit(0,"cm"), box.padding = unit(0.1,"cm"),fontface="bold",size=2.5) +
  geom_text_repel(data=plotdata %>% filter(id %in% c("US-DE","US-NH","US-RI","US-MA","US-NJ","US-MD")),
                  xlim=c(.35,.35),aes(label = paste("$",round(gdpcap_2018ppp),sep=""), x = Longitude, y = Latitude),
                  point.padding = unit(0,"cm"), box.padding = unit(0.1,"cm"),fontface="bold",size=2.5,
                  segment.color = "gray80",segment.size = 0.25) +
  labs(x="",y="",title="GDP per Capita in 2018 (000s USD, PPP)",
       subtitle="Note: Own calculations using data from Statistics Canada data table 36-10-0222 and the US BEA. 
       All values are in real PPP-adjusted US dollars, based on 36-10-0100. Graph by @trevortombe.")
ggsave("map_all_NA.png",width=7,height=6.75,dpi=300)

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

###########################
# Median Household Income #
###########################
ggplot() + geom_map(data=plotdata,aes(map_id=id,fill=med_income_2015),map=ca_map,color="white") +
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
                  aes(label = paste("$",round(med_income_2015),sep=""), x = Longitude, y = Latitude),
                  point.padding = unit(0,"cm"), box.padding = unit(0.1,"cm"),fontface="bold",size=2.5) +
  geom_text_repel(data=plotdata %>% filter(id %in% c("US-DE","US-NH","US-RI","US-MA","US-NJ","US-MD")),
                  xlim=c(0.32,0.32),aes(label = paste("$",round(med_income_2015),sep=""), x = Longitude, y = Latitude),
                  point.padding = unit(0,"cm"), box.padding = unit(0.1,"cm"),fontface="bold",size=2.5,
                  segment.color = "gray80",segment.size = 0.25) +
  labs(x="",y="",title="Median Household Income in 2015 (000s USD, PPP)",
       subtitle="Note: Own calculations using data from Statistics Canada (Census 2016) and the U.S. Census Bureau. 
Real dollars, PPP adjusted. Graph by @trevortombe.")
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

# Fiscal Transfers in 2018
ggplot() + geom_map(data=plotdata_canada %>% mutate(status=ifelse(gap>0,"giver","getter")),
                    aes(map_id=id,fill=status),
                    map=test2,color="white") +
  expand_limits(x=c(-130,-50),y=c(44,60)) +
  coord_map("albers",lat0=40, lat1=60)+
  #scale_fill_gradient2(low = "#FF2700",mid="beige",high = "#2b83ba",
  #                     midpoint = 0)+
  scale_fill_manual(name="",values=c("#FF2700","#008FD5"),
                    label=c("Net Recipient","Net Contributor"))+
  theme(
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.y=element_blank(),
    axis.ticks.x=element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    legend.position=c(0.2,0.2),
    #legend.text=element_text(size=10),
    plot.title = element_text(size = 16, face = "bold",hjust=0.5),
    plot.subtitle = element_text(size = 7, color="gray50",hjust=0.5),
    plot.margin = unit(c(-2,-2,-2,-1), "cm")
  )+
  geom_text_repel(data=plotdata_canada,
                  aes(label = paste0("$",round(1000000*gap,0)), x = Longitude, y = Latitude),
                  point.padding = unit(0,"cm"), box.padding = unit(0.1,"cm"),fontface="bold",size=3) +
  labs(x="",y="",title="Per Capita Federal Fiscal Gaps in Canada (2017)",
       subtitle="Note: Displays the difference between federal revenue and spending in each region ($000/capita USD), relative to a common per capita benchmark.
Net \"outflows\" imply federal revenue exceeds federal spending. Source: Own calculations from Schultz and Cummings (2019) for the USA and Statistics
Canada data table 36-10-0450 for Canada. Methodology in Tombe (2018, Canadian Tax Journal).")
