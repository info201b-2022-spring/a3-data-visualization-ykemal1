trends<-read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

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
  geom_line(mapping = aes(x=year,y=incarceration_rate,color=female_by_race),size=5)+
  labs(x="Year",y="Incarcerayion Rate",color="Female By Race")+
  ggtitle("Yousras Analysis")

make_female_race_plot<-ggplot(data=trends)+
  geom_line(mapping = aes(x=year,y=female_prison_pop),size=5)+
  labs(x="Year",y="Incarcerayion Rate",color="Female By Race")+
  ggtitle("Yousras Analysis")




