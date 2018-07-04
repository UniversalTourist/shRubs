# Libraries
library(tidyverse)
library(ALA4R)
library(vegan)
library(RColorBrewer)

# Triodia, Brachychiton, Flindersia, Livistona, Callitris, Daviesia, Ficus, Hakea, Bottlebrush,
#Grevillea. Need more?
# Some look to be limited by rainfall (in wetter areas) but they are not in the north. Why not? What other factors might be limiting distribution?
# Some are limited to north and e coast? Why? Some of these are also in what looks like arid zone. Do some species live in very different conditions from other members of genus or are there small patches of matched climate.
# Rainfall is important in biodiversity. Is there a gradient in biodiversity in line with rainfall gradients?

###NDS Examples

## define some environmental layers of interest [see ala_fields()]
env_layers <- c("Precipitation - annual","Temperature - annual max mean")

# can not select more than one taxon at the same time
triodia <- occurrences(taxon = "triodia", qa = "none",
                        download_reason_id = "testing",extra=env_layers)$data %>% mutate(plant ="Triodia")

bottlebrush <- occurrences(taxon = "bottlebrush", qa = "none",
                       download_reason_id = "testing",extra=env_layers)$data %>% mutate(plant ="Bottlebrush")

grevillea <- occurrences(taxon = "grevillea", qa = "none",
                           download_reason_id = "testing",extra=env_layers)$data %>% mutate(plant ="Grevillea")

brachychiton <- occurrences(taxon = "brachychiton", qa = "none",
                       download_reason_id = "testing",extra=env_layers)$data %>% mutate(plant ="Brachychiton")


flindersia <- occurrences(taxon = "flindersia", qa = "none",
                            download_reason_id = "testing",extra=env_layers)$data %>% mutate(plant ="Flindersia")


livistona <- occurrences(taxon = "livistona", qa = "none",
                          download_reason_id = "testing",extra=env_layers)$data %>% mutate(plant ="Livistona")


callitris <- occurrences(taxon = "callitris", qa = "none",
                         download_reason_id = "testing",extra=env_layers)$data %>% mutate(plant ="Callitris")


daviesia <- occurrences(taxon = "daviesia", qa = "none",
                         download_reason_id = "testing", extra=env_layers)$data %>% mutate(plant ="Daviesia")

ficus <- occurrences(taxon = "ficus", qa = "none",
                        download_reason_id = "testing", extra=env_layers)$data %>% mutate(plant ="Ficus")


hakea <- occurrences(taxon = "hakea", qa = "none",
                     download_reason_id = "testing",extra=env_layers)$data %>% mutate(plant ="Hakea")



datos <- rbind(triodia, brachychiton, flindersia, livistona,
               callitris, daviesia, ficus, hakea, bottlebrush, grevillea) %>%  dplyr::filter(rank %in%
                c("species", "subspecies", "variety", "form", "cultivar")) %>%
  mutate(longitude = round(longitude*2)/2, latitude = round(latitude*2)/2) %>%
  group_by(longitude,latitude)  %>%
  dplyr::select(year, month, eventDate, longitude, latitude, scientificNameOriginal, 
                state, plant, precipitationAnnual, temperatureAnnualMaxMean) 

#write.csv(datos, "datos.csv")

datos %>% drop_na() %>% filter(state!="") %>%group_by( year,state) %>% summarise(total=n()) %>%
  filter(year>1990) %>%
  ggplot(aes(y=total, x=year)) + geom_point() + geom_line() + facet_wrap(~state) 

datos %>% drop_na() %>% filter(state!="") %>% group_by( year, plant) %>% summarise(total=n()) %>%
  filter(year>1990) %>%
  ggplot(aes(y=total, x=year, color=plant)) + geom_point() + geom_line() 

#Is this evolution different by state?
datos %>% drop_na() %>%filter(state!="")%>%group_by( year,  plant, state) %>% summarise(total=n()) %>%
  filter(year>1990) %>%
  ggplot(aes(y=total, x=year, color=plant)) + geom_point() + geom_line() + facet_wrap(~state) +
  theme(legend.position = "bottom")
#


#presipitiation and temperature

datos %>% group_by(state, year) %>% drop_na() %>%filter(state!="") %>% filter(year>1990) %>%
  mutate(precipitationAnnual = mean(precipitationAnnual, na.rm = TRUE),
         temperatureAnnualMaxMean = mean(temperatureAnnualMaxMean, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y=precipitationAnnual)) +geom_line() + geom_point()+
  facet_wrap(~state)


### Australian MAP
load("aus_map.Rda")

library(tidyverse)
library(ochRe)


map <- aus_map %>%
  ggplot() +
  geom_polygon(aes(long, lat, group = group), alpha=1/3) +
  theme_bw() + coord_map() 

#MAP with totals by year
plcant <-  function(y, siz = FALSE, col = TRUE, pl = "all"){
if(pl=="all"){
  dat3 <- datos %>% group_by(state, year) %>% drop_na() %>%filter(state!="") %>% 
    filter(year == y) %>% mutate(total = n())   
} else{
dat3 <- datos %>% group_by(state, year) %>% drop_na() %>%filter(state!="") %>% 
  filter(year == y) %>% mutate(total = n()) %>% filter(plant==pl)
}

xs=quantile(dat3$total)
datall <- dat3 %>% mutate(cattot = cut(total, breaks=c(xs[1], xs[2]), xs[3], xs[4], xs[5]))
 if(col){
 map + geom_point(data = dat3, aes(x = longitude, y =latitude, colour = total), alpha=1/3) +
   labs(colour = "Total", title = paste(pl, y))
 }else{
 map + geom_point(data = dat3, aes(x = longitude, y =latitude, size = total), alpha =1/3) +
     labs(sizw = "Total", title = paste(pl, y))
 }
}

unique(datos$plant)

plcant(2015)
plcant(2015, pl = "Brachychiton" )
plcant(2015, pl = "Triodia" )
plcant(2015, pl = "Flindersia")
plcant(2015, pl = "Livistona")
plcant(2015, pl =  "Callitris")
plcant(2015, pl =  "Daviesia")
plcant(2015, pl =  "Ficus")
plcant(2015, pl =  "Hakea")





### OLD code to check

brachychiton <- specieslist("brachychiton")  
dim(brachychiton)
#Not useful only counts


fabaceae <- occurrences(taxon = "family:Fabaceae", qa = "none",
                        download_reason_id = "testing")



fabaceaedat <- fabaceae$data %>%
  ## discard genus- and higher-level records
  dplyr::filter(rank %in%
                  c("species", "subspecies", "variety", "form", "cultivar")) %>%
  
  ## bin into 0.5-degree bins
  mutate(longitude = round(longitude*2)/2, latitude = round(latitude*2)/2) %>%
  group_by(longitude,latitude)  %>%
  dplyr::select(year, month, eventDate, longitude, latitude, scientificNameOriginal, 
                state) 

#remove NA
#How is the temporal evolution of fabaceae?
#select data from 19900
fabaceaedat %>% group_by( year) %>% summarise(total=n()) %>% drop_na() %>%
  filter(year>19900) %>%
  ggplot(aes(y=total, x=year)) + geom_point() + geom_line() 

#Is this evolution different by state?
fabaceaedat %>% group_by( year, state) %>% summarise(total=n()) %>% drop_na() %>%
  filter(year>19900) %>%
  ggplot(aes(y=total, x=year, color=state)) + geom_point() + geom_line() 