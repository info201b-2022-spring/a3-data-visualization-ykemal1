trends<-read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

install.packages("maps")
install.packages("mapproj")
library(maps)
library(mapproj)


library("tidyverse")
trends<-
  trends %>% 
  drop_na()
 


### Get the black female jail popiulation ever recorded total

bwomen<-trends %>% 
 select(black_female_prison_pop) %>% 
  filter(black_female_prison_pop==max(black_female_prison_pop)) %>% 
  pull(black_female_prison_pop)
  

## get the white female jail population ever recored total
wwomen<-trends %>%
  select(white_female_prison_pop) %>% 
  filter(white_female_prison_pop==max(white_female_prison_pop)) %>% 
  pull(white_female_prison_pop)
  

### get the max latinx female incarceration ever recorded

lwomen<-trends %>% 
  select(latinx_female_prison_pop) %>% 
  filter(latinx_female_prison_pop==max(latinx_female_prison_pop)) %>% 
  pull(latinx_female_prison_pop)




Female_plot <- ggplot(data = trends)+
  geom_line(mapping = aes(x = year, y = black_female_prison_pop, color = "red"), size = 3)+
  labs(x = "Year", y = "Black Female Prison Population", color = "Female  Prison Population")+
  scale_alpha_continuous(10,20,20)
  

### get the aapi female incarceration ever recorded
awomen<-trends %>% 
  select(aapi_female_prison_pop) %>% 
  filter(aapi_female_prison_pop==max(aapi_female_prison_pop)) %>% 
  pull(aapi_female_prison_pop)

### get the female jail population for 2009 do for all races and compare

womenblack2009<-trends %>% 
  select(black_female_prison_pop,year) %>% 
  filter(year==2009) %>% 
  filter(black_female_prison_pop==max(black_female_prison_pop)) %>% 
  pull(black_female_prison_pop)

whitewomen2009<-trends %>% 
  select(white_female_prison_pop,year) %>% 
  filter(year==2009) %>% 
  filter(white_female_prison_pop==max(white_female_prison_pop)) %>% 
  pull(white_female_prison_pop)

latinxwomen2009<-trends %>% 
  select(latinx_female_prison_pop,year) %>% 
  filter(year==2009) %>% 
  filter(latinx_female_prison_pop==max(latinx_female_prison_pop)) %>% 
  pull(latinx_female_prison_pop)


aapiwomen2009<-trends %>% 
  select(aapi_female_prison_pop, year) %>% 
  filter(year==2009) %>% 
filter(aapi_female_prison_pop==max(aapi_female_prison_pop)) %>% 
  pull(aapi_female_prison_pop)

dataplotbw<-trends %>% 
  select(female_prison_pop,black_female_prison_pop,white_female_prison_pop,
         latinx_female_prison_pop,aapi_female_prison_pop,year) %>% 
  group_by(year) %>% 
  summarise(year= unique(year),female_prison_pop=sum(female_prison_pop),
            black_female_prison_pop=sum(black_female_prison_pop),
            white_female_prison_pop =sum(white_female_prison_pop),
            latinx_female_prison_pop=sum(latinx_female_prison_pop),
            aapi_female_prison_pop=sum(aapi_female_prison_pop))
combine_data<-dataplotbw %>% 
  select(female_prison_pop,black_female_prison_pop,white_female_prison_pop,
         latinx_female_prison_pop,aapi_female_prison_pop,year) %>%
  
  gather(key=female_by_race,value=incarceration_rate,-year) %>% 
  group_by(female_by_race,incarceration_rate)

for(i in 1:length(combine_data$female_by_race)){
  
  if(combine_data$female_by_race[i] == "female_prison_pop"){
    
    combine_data$female_by_race[i] = "Female Total Prison Pop"
  }else if(combine_data$female_by_race[i] == "black_female_prison_pop"){
    combine_data$female_by_race[i] = "Black Female Prison Pop"
  }else if(combine_data$female_by_race[i] == "white_female_prison_pop"){
    combine_data$female_by_race[i] = "White Female Prison Pop"
  }else if(combine_data$female_by_race[i] == "latinx_female_prison_pop"){
    combine_data$female_by_race[i] = "Latinx Female Prison Pop"
  }else if(combine_data$female_by_race[i] == "aapi_female_prison_pop"){
    combine_data$female_by_race[i] = "AAPI Female Prison Pop"
  }
  
}
  


make_female_race_analysis<-ggplot(data=combine_data)+
  geom_line(mapping = aes(x=year,y=incarceration_rate,color=female_by_race),size=4)+
  labs(x="Year",y="Incarceration Rate",color="Female By Race")+
  ggtitle("Yousras Analysis")

make_female_race_plot <- trends  %>%
  select(black_jail_pop,year,fips)




bwmaps <- map_data("county") %>% 
  unite(polyname,region,subregion,sep = ",")


joined_data<- left_join(bwmaps,county.fips)


merge_data<- full_join(make_female_race_plot,joined_data)


Map<-ggplot(data=merge_data)+
  geom_polygon(mapping = aes(x=long,y=lat, group=group,color="grey", 
    fill = black_jail_pop
))+
scale_fill_continuous(limits = c(0, max(merge_data$black_jail_pop)), na.value = "white",low="yellow",high="red") 


blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(), # remove axis lines
    axis.text = element_blank(), # remove axis labels
    axis.ticks = element_blank(), # remove axis ticks
    axis.title = element_blank(), # remove axis titles
    plot.background = element_blank(), # remove gray background
    panel.grid.major = element_blank(), # remove major grid lines
    panel.grid.minor = element_blank(), # remove minor grid lines
    panel.border = element_blank() # remove border around plot
  )


