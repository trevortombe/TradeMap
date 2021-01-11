# Load useful libraries and functions
source("core.R")

# Store some useful objects
today<-Sys.Date()
as_of_label<-gsub(" 0"," ",format(today,"%b %d, %Y"))

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

# Fetch CDC Vaccination Data. Typically updated at 9am ET weekdays.
# Download: https://covid.cdc.gov/covid-data-tracker/#vaccinations
cdc_data<-read.csv("covid19_vaccinations_in_the_united_states.csv",skip=3) %>%
  rename(Name=State.Territory.Federal.Entity) %>%
  mutate(Name=ifelse(Name=="New York State","New York",as.character(Name)))

# Bring in the Canadian Summary Data
canada_data<-as.data.frame(fromJSON('https://api.covid19tracker.ca/summary/split')) %>%
  select(short=data.province,vaccines=data.total_vaccinations) %>%
  left_join(data %>% mutate(short=substr(id,4,5)) %>% select(short,pop_2020),by="short") %>%
  mutate(vaccines=100*vaccines/pop_2020)

# Format the data
plotdata<-tibble(id=ca@data[,5]) %>%
  distinct(id) %>%
  left_join(data,by="id") %>%
  left_join(new_centroids,by="id") %>%
  filter(!is.na(Country)) %>%
  left_join(cdc_data,by="Name") %>%
  mutate(label=substr(id,4,5)) %>%
  filter(!(id %in% c("CA-YT","CA-NU","CA-NT"))) %>%
  select(Name,id,Longitude,Latitude,vaccines=Administered.per.100K) %>%
  mutate(short=substr(id,4,5),
         vaccines=as.numeric(as.character(vaccines)),
         vaccines=vaccines/1000) %>%
  left_join(canada_data,by="short") %>%
  mutate(vaccines=ifelse(is.na(vaccines.x),vaccines.y,vaccines.x))

# Attempt to extract Alaska, hawaii
alaska <- ca[ca$STATEABB=="US-AK" & !is.na(ca$STATEABB),]
alaska <- elide(alaska, scale=max(apply(bbox(alaska), 1, diff)) / 2.5)
alaska <- elide(alaska, shift=c(-150, 39))
proj4string(alaska) <- proj4string(ca)
hawaii <- ca[ca$STATEABB=="US-HI" & !is.na(ca$STATEABB),]
hawaii <- elide(hawaii, scale=1.5*max(apply(bbox(hawaii), 1, diff)))
hawaii <- elide(hawaii, shift=c(-136, 30))
proj4string(hawaii) <- proj4string(ca)
test <- ca[ca$STATEABB!="US-AK" & ca$STATEABB!="US-HI" & !is.na(ca$STATEABB),]
test <- rbind(test,alaska,hawaii)
test2<-fortify(test,region="STATEABB")

ggplot() + geom_map(data=plotdata,aes(map_id=id,fill=vaccines),map=test2,color="white") +
  expand_limits(x=-100,y=50) +
  coord_map("albers",lat0=40, lat1=60,xlim=c(-135,-59),ylim=c(25,61))+
  scale_fill_continuous(low = "#eff3ff",high = "dodgerblue") +
  theme(
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.y=element_blank(),
    axis.ticks.x=element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    legend.position="none",
    plot.title = element_text(size = 16, face = "bold",hjust=0.5),
    plot.subtitle = element_text(size = 7, color="gray50",hjust=0.5),
    plot.margin = unit(c(-2,-1,-2,-1), "cm") # t,r,b,l
  )+
  annotate('rect',xmin=bbox(alaska)[1]+2,xmax=-129,ymin=bbox(alaska)[2],ymax=bbox(alaska)[4]+1,
           fill="transparent",color="gray",size=1,linetype="dotted")+
  annotate('rect',xmin=bbox(hawaii)[1],xmax=bbox(hawaii)[3]+1,ymin=bbox(hawaii)[2]-1,ymax=bbox(hawaii)[4]+1,
           fill="transparent",color="gray",size=1,linetype="dotted")+
  geom_text_repel(data=plotdata %>% filter(!id %in% c("US-DE","US-NH","US-RI","US-MA","US-NJ","US-MD")),
                  aes(label = number(vaccines,.1), x = Longitude, y = Latitude),
                  point.padding = unit(0,"cm"), box.padding = unit(0.1,"cm"),fontface="bold",size=3) +
  geom_text_repel(data=plotdata %>% filter(id %in% c("US-DE","US-NH","US-RI","US-MA","US-NJ","US-MD")),
                  xlim=c(0.37,0.37),aes(label = number(vaccines,.1), x = Longitude, y = Latitude),
                  point.padding = unit(0,"cm"), box.padding = unit(0.1,"cm"),fontface="bold",size=3,
                  segment.color = "gray80",segment.size = 0.25) +
  labs(x="",y="",
       title=paste0("COVID-19 Vaccine Doses Administered per 100 People (",as_of_label,")"),
       subtitle="Note: Own calculations using data from N. Little. COVID-19 Tracker Canada (2020), COVID19Tracker.ca, 
Statistics Canada 17-10-0005, and the CDC COVID Data Tracker. Graph by @trevortombe.")
ggsave("COVID_vaccine_map.png",width=8,height=6.75,dpi=300)

