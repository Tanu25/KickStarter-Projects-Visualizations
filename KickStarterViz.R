#Tanushree Shetty
#Poster Presentation



library(tidyverse)   #For dplyr
library(ggthemes)
library(lubridate)  #dateTime
library(rworldmap)
library(gplots)
library(ggplot2)
library(RColorBrewer)
library(viridis)

data <- read.csv('ks-projects-201801.csv')
str(data)


sapply(data, function(x) sum(is.na(x)))
#Only column 'usd.pledged has 3797 NAs, but that column is not needed in the analysis
#So that column will be dropped

data <- data[,-13]
colnames(data)[13] <- "usd_pledged"
colnames(data)[14] <- "usd_goal"

head(data)


#What types(categories) of projects are most popular?
Freq1 <- data %>%
  group_by(main_category) %>%
  summarize(count=n()) %>%
  arrange(desc(count))
Freq1


Freq1$main_category <- factor(Freq1$main_category, levels=Freq1$main_category)

#Category
ggplot(Freq1, aes(main_category, count, fill=count)) + geom_bar(stat="identity") + 
  ggtitle("No of Projects by Category") + xlab("Project Category") + ylab("Frequency") +
  theme(plot.title=element_text(hjust=0.5), axis.title=element_text(size=12, face="bold"), 
        axis.text.x=element_text(size=12, angle=90), legend.position="null") + 
  scale_fill_gradient(low="chartreuse4", high="cadetblue1")



Freq2 <- data %>%
  group_by(category) %>%
  summarize(count=n()) %>%
  arrange(desc(count))
Freq2

Freq2$category <- factor(Freq2$category, levels=Freq2$category)

#Top 15 subcategories
ggplot(head(Freq2, 15), aes(category, count, fill=count)) + geom_bar(stat="identity") + 
  ggtitle("Top 15 subcategories of Projects") + xlab("Project Subcategory") + ylab("Frequency") + 
  theme(plot.title=element_text(hjust=0.5), axis.title=element_text(size=12, face="bold"), 
        axis.text.x=element_text(size=12, angle=90), legend.position="null") + 
  scale_fill_gradient(low="coral", high="darkgoldenrod4")


#What categories of projects are being funded the most?

#Total funding for each main categories
pledged.tot <- data %>%
  group_by(main_category) %>%
  summarize(total=sum(usd_pledged)) %>%
  arrange(desc(total))

pledged.tot

pledged.tot$main_category <- factor(pledged.tot$main_category, levels=pledged.tot$main_category)

#Amt_pledged~Category
ggplot(pledged.tot, aes(main_category, total/1000000, fill=total)) + geom_bar(stat="identity") + 
  ggtitle("Total Amount Pledged by Category") + xlab("Project Category") + 
  ylab("USD Pledged(in millions)") + 
  theme(plot.title=element_text(hjust=0.5), axis.title=element_text(size=12), 
        axis.text.x=element_text(size=12, angle=90), legend.position="null") + 
  scale_fill_gradient(low="darkseagreen1", high="darkolivegreen")


pledged.avg <- data %>%
  group_by(main_category) %>%
  summarize(pledged=sum(usd_pledged), backers=sum(backers)) %>%
  mutate(avg=pledged/backers) %>%
  arrange(desc(avg))

pledged.avg$main_category <- factor(pledged.avg$main_category, levels=pledged.avg$main_category)

#Avg Amt_Pledged~Category~Backer
ggplot(pledged.avg, aes(main_category, avg, fill=avg)) + geom_bar(stat="identity") + 
  ggtitle("Average Amount Pledged per Backer") + xlab("Project Category") + 
  ylab("Amount Pledged (USD)") + 
  geom_text(aes(label=paste0("$", round(avg,2))), vjust=-0.5) + theme_economist() + 
  theme(plot.title=element_text(hjust=0.5), axis.title=element_text(size=12, face="bold"), 
        axis.text.x=element_text(size=12, angle=90), legend.position="null") + 
  scale_fill_gradient(low="skyblue1", high="royalblue4")

#Boxplot- category~Amt_Pledged
ggplot(data, aes(main_category, usd_pledged, fill=main_category)) + geom_boxplot() + 
  ggtitle("Amount Pledged vs. Project Category") + xlab("Project Category") + 
  ylab("Amount Pledged (USD)") + 
  theme(plot.title=element_text(size=15, face="bold", hjust=0.5), 
        axis.title=element_text(size=12, face="bold"), 
        axis.text.x=element_text(size=12, angle=90), legend.position="null") + 
  coord_cartesian(ylim=c(0,15000))



#How much funding is required?


goal.tops <- data[data$state=="successful",]
goal.avg <- data %>%
  group_by(main_category) %>%
  summarize(goal=sum(usd_goal), projects=n()) %>%
  mutate(avg=goal/projects) %>%
  arrange(desc(avg))

goal.avg$main_category <- factor(goal.avg$main_category, levels=goal.avg$main_category)

#Average Proj_goal~Category[Only for successful projects]~ success
ggplot(goal.avg, aes(main_category, avg, fill=avg)) + geom_bar(stat="identity") + 
  ggtitle("Average Project Goal") + xlab("Project Category") + ylab("Project Goal (USD)") + 
  theme(plot.title=element_text(hjust=0.5), axis.title=element_text(size=12, face="bold"), 
        axis.text.x=element_text(size=12, angle=90), legend.position="null") + 
  scale_fill_gradient(low="pink", high="palevioletred4")

#Boxplot Proj_Goal~Category[Only for successful projects]~success
ggplot(data, aes(main_category, usd_goal, fill=main_category)) + geom_boxplot() + 
  ggtitle("Project Goal vs. Project Category") + xlab("Project Category") + 
  ylab("Project Goal (USD)") + 
  theme(plot.title=element_text(size=15, hjust=0.5), 
        axis.title=element_text(size=12), 
        axis.text.x=element_text(size=12, angle=90), legend.position="null") + 
  coord_cartesian(ylim=c(0,60000)) 



#Since we can expect both distributions to be heavily right-skewed due to many projects 
#that received little to no funding and extremely high outliers, we will use a log 
#transformation on both variables to better visualize their distributions.


usd.amounts <- gather(data, type, amount, usd_pledged, usd_goal, factor_key=T)

#USD_Pledged vs USD_goal
ggplot(usd.amounts, aes(log(amount+1), fill=type)) + 
  geom_histogram(alpha=0.5, position="identity") + 
  ggtitle("Distribution of log(USD Pledged) vs. log(USD Goal)") + xlab("log(USD + 1)") + 
  ylab("Frequency") + scale_fill_discrete("Type", labels=c("USD Pledged", "USD Goal"))



#What types of projects were successful and unsuccessful?

state.freq <- data %>%
  group_by(state) %>%
  summarize(count=n()) %>%
  arrange(desc(count))

state.freq$state <- factor(state.freq$state, levels=state.freq$state)

ggplot(state.freq, aes(state, count, fill=count)) + geom_bar(stat="identity") + 
  ggtitle("Status of Projects") + xlab("Project Status") + ylab("Frequency") + 
  theme(plot.title=element_text(hjust=0.5), axis.title=element_text(size=12), 
        axis.text.x=element_text(size=12), legend.position="null") + 
  scale_fill_gradient(low="lightskyblue", high="dodgerblue4")




#Considering only 'success' and 'failed' projects, since they are the only ones that
#are complete. The rest 'live', 'cancelled' and 'suspended' projects did not even complete 
state.pct <- data %>%
  filter(state %in% c("successful", "failed")) %>%
  group_by(main_category, state) %>%
  summarize(count=n()) %>%
  mutate(pct=count/sum(count)) %>%
  arrange(desc(state), pct)

state.pct$main_category <- factor(state.pct$main_category, 
                                  levels=state.pct$main_category[1:(nrow(state.pct)/2)])

ggplot(state.pct, aes(main_category, pct, fill=state)) + geom_bar(stat="identity") + 
  ggtitle("Success vs. Failure Rate by Project Category") + 
  xlab("Project Category") + ylab("Percentage") + scale_y_continuous(labels=scales::percent) + 
  geom_text(aes(label=paste0(round(pct*100,1),"%")), position=position_stack(vjust=0.5), 
            colour="white", size=5) + 
  theme(plot.title=element_text(hjust=0.5), axis.title=element_text(size=12), 
        axis.text.x=element_text(size=12), legend.position="bottom", 
        legend.title=element_text(size=12)) + coord_flip()


#Does project length affect success rate?

#converting the dates to year-month-day format and finding the interval
#find the length of each project by taking the difference between the project 
#deadline and the project launch date, then dividing by the number of whole days.
data$length <- interval(ymd_hms(data$launched), ymd(data$deadline)) %/% days(1)

#The maximum project duration according to Kickstarter’s rules is 60 days
length.pct <- data %>%
  filter(state %in% c("successful", "failed"), length <= 61) %>%
  group_by(length, state) %>%
  summarize(count=n()) %>%
  mutate(pct=count/sum(count))

length.pct
# Success rate vs Project Length
ggplot(length.pct[length.pct$state=="successful",], aes(length, pct)) + 
  geom_point(colour="royalblue4", size=2.5) + ggtitle("Success Rate vs. Project Length") + 
  xlab("Project Length (Days)") + ylab("Success Rate (%)") + 
  scale_x_continuous(breaks=c(0,10,20,30,40,50,60)) + geom_vline(xintercept=30, colour="red") + 
  theme_economist() + 
  theme(plot.title=element_text(hjust=0.5), axis.title=element_text(size=12, face="bold"))



#Projects by year

year<- ymd_hms(data$launched)
year1<-year(year)
data$year <- year1
unique(data$year)

year.count <- aggregate(data$year, list(year = data$year), length)
year.count<-year.count[!(year.count$year=="1970"),]


#Projects by country
#Map
countries.freq <- data %>%
  filter(country!='N,0"') %>%
  group_by(country) %>%
  summarize(count=n())

countries.match <- joinCountryData2Map(countries.freq, joinCode="ISO2", nameJoinColumn="country")



mapCountryData(countries.match, nameColumnToPlot="count", 
               mapTitle="Number of Projects by Country", catMethod="logFixedWidth", 
               colourPalette="heat")

#Bakers~Category
backers<- aggregate(data$backers, by=list(category=data$main_category), FUN=length)

theme_set(theme_bw())
ggplot(backers, aes(x=category, y=x)) + 
  geom_point(size=3) + 
  geom_segment(aes(x=category, 
                   xend=category, 
                   y=0, 
                   yend=x)) + 
  labs(title="Lollipop Chart", 
       subtitle="Backer$Category") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))


#
amt.state.year<- aggregate(data$usd_pledged, by=list(year = data$year, state = data$state), FUN=mean)
amt.state.year
amt.state.year<-amt.state.year[!(amt.state.year$year=="1970"),]
ggplot(amt.state.year) + aes(x=year, y =x, color = state) + 
  geom_line()+ ylim(c(0, 35000))+geom_point() + 
  scale_x_continuous(breaks=c(2009,2010,2011,2012,2013,2014,2015,2016,2017,2018))+
  ggtitle("Average amount raised by state in each year")



year.count1 <- aggregate(data$year, list(year = data$year, state = data$state), length)
year.count1<-year.count1[!(year.count1$year=="1970"),]
year.count1

#Total Project~Year~State
ggplot(year.count1) + aes(x=year, y =x, color = state) + 
  geom_line()+ ylim(c(0, 45000))+geom_point() + 
  scale_x_continuous(breaks=c(2009,2010,2011,2012,2013,2014,2015,2016,2017,2018))+
  ggtitle("Total projects in every state in each year")

########

amt.cat.year<- aggregate(data$usd_pledged, by=list(year = data$year, Category = data$main_category), FUN=mean)
amt.cat.year
amt.cat.year<-amt.cat.year[!(amt.cat.year$year=="1970"),]
ggplot(amt.cat.year) + aes(x=year, y =x, color = Category) + 
  geom_line()+ ylim(c(0, 50000))+geom_point() + 
  scale_x_continuous(breaks=c(2009,2010,2011,2012,2013,2014,2015,2016,2017,2018))+
  ggtitle("Average amount raised by Category in each year")



tot.cat.year <- aggregate(data$usd_pledged, list(year = data$year, category = data$main_category), length)
tot.cat.year<-tot.cat.year[!(tot.cat.year$year=="1970"),]
tot.cat.year

#Total Project~Year~State
ggplot(tot.cat.year) + aes(x=year, y =x, color = category) + 
  geom_line()+ ylim(c(0, 12000))+geom_point() + 
  scale_x_continuous(breaks=c(2009,2010,2011,2012,2013,2014,2015,2016,2017,2018))+
  ggtitle("Total projects in every Category each year")











