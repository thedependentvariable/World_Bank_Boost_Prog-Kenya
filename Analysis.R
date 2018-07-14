

#Libraries and Data Import
library(tidyverse)
library(ggthemes)
library(DataExplorer)
library(plotly)
#load data

kenya_boost_2018<-read.csv(file = "kenya_boost_2013-2018.csv",colClasses="character") 

kenya_boost<-kenya_boost_2018%>% 
  select(Year,Class,Vote.Groups,National.Government.Votes...Counties,
         Sub.Votes,Head.Dept,County.Government.Ministries,Category,Chapter,
         Item,SOF1,SOF2,SOF3,SOF4,Prog1,Prog2,Prog3,
         Initial.Budget..Printed.Estimate.,Reallocation..Transfer,
         Supplementary.Estimate,
         Final.Budget..Approved.Estimate.,Cumulative.Expenditure,
         Outstanding.Commitment,Final.Budget..Approved.Estimate.,Balance) 


#numeric columns with thousand separator
col2cvt<-18:24

#remove the thousand comma separator
kenya_boost[,col2cvt] <- lapply(kenya_boost[,col2cvt],function(kenya_boost){as.numeric(gsub(",", "", kenya_boost))})


#fill NA's with 0, there were no funds allocated
kenya_boost[is.na(kenya_boost)]<-0


#merge same periods with different formats to simmilar format 
kenya_boost[,1] <- lapply(kenya_boost[,1],function(kenya_boost){as.numeric(gsub("2014-15", "2014-15", kenya_boost))})

#quick view of the data
introduce(kenya_boost)


plot_missing(kenya_boost) #which columns have missing values

plot_str(kenya_boost) #Column classes


plot_bar(kenya_boost) #plot of each column


#How much boost did kenya receive every year?

kenya_boost$Year<-as.factor(kenya_boost$Year)


yearly_budget<-kenya_boost%>% 
  group_by(Year) %>% 
  summarise(Number_allocations = n(),
            total_budget = sum(Final.Budget..Approved.Estimate.))

#plot of the number of projects funded in a year.
ggplot(yearly_budget,aes(x= Year,y=Number_allocations,fill=Year))+geom_bar(stat = "identity",show.legend = FALSE)+ ggtitle("Number of projects funded per year")


#Which  class has benefited the most in each years boost
class_allocations<-kenya_boost %>% 
  group_by(Year,Class) %>% 
  summarise(allocations = n(),
            total_budget = sum(Final.Budget..Approved.Estimate.))

ggplot(class_allocations,aes(x=Class,y=allocations,fill= Year))+
  geom_bar(stat = "identity",show.legend = FALSE)+
  facet_wrap(~Year)+ggtitle("Number of allocations per year")+
  theme_hc()

#budgeting per project per year
p<-ggplot(class_allocations,aes(x=Class,y=total_budget,fill= Year))+
  geom_bar(stat = "identity",show.legend = FALSE)+
  facet_wrap(~Year)+ggtitle("Budget per project per year")+
  theme_wsj()
ggplotly(p)
