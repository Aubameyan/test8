#Load in the datasets and have a look at them
fulldata2021 <- read.csv("C:/Users/marcu/Downloads/world-happiness-report-2021.csv")
fulldatapast <- read.csv("C:/Users/marcu/Downloads/world-happiness-report.csv")
head(fulldata)
head(fulldatapast)
View(fulldata)
View(fulldatapast)
shape(fulldata)
dim(fulldata)
dim(fulldatapast)

#Rename some of the columns in the past dataset
library(dplyr)

fulldatapast = fulldatapast %>% rename("Ladder.score"="Life.Ladder","Logged.GDP.per.capita" = "Log.GDP.per.capita",  "Healthy.Life.Expectancy" = "Healthy.life.expectancy.at.birth","Country.name" = "ï..Country.name")
fulldata = fulldata %>% rename("Country.name" = "ï..Country.name")

sample_n(fulldatapast,size=1)

#Just get the columns we need for the analysis from the current dataset
data = select(fulldata,"Country.name", "Regional.indicator", "Ladder.score","Logged.GDP.per.capita","Social.support","Healthy.life.expectancy","Freedom.to.make.life.choices","Generosity","Perceptions.of.corruption")

head(data)

#Check if there are any missing values to deal with
sum(is.na(data))
summary(data)

#Check correlations, from this we can see what happiness is positively and negatively correlated with and to what extent
corr = cor(data[sapply(data,is.numeric)])

library(ggplot2)
library(reshape2)
meltedcorrdata <- melt(corr)
head(meltedcorrdata)
corrplot <- ggplot(meltedcorrdata, aes(x=Var1, y = Var2,fill=value))+ geom_tile()+ theme(axis.text.x = element_text(angle=45,hjust = 1))
corrplot

#Have a look at the happiness scores at regional level, can see outliers in latin America and central/eastern Europe
y=data %>%  group_by(Regional.indicator) %>% summarise(mean_happines = mean(Ladder.score),min_happines = min(Ladder.score),max_happines = max(Ladder.score))
y

regionboxplot <- ggplot(data,aes(x=Ladder.score, y = Regional.indicator,fill=Regional.indicator),)+geom_boxplot()
regionboxplot

######CENTRAL AND EASTERN EUROPE######
#Have a look at Central and Eastern Europe in more detail, can see that there are some correlations but less so than the full dataset
dataeasterneurope <- data  %>% filter(Regional.indicator == "Central and Eastern Europe")
correurope <- cor(dataeasterneurope[sapply(data,is.numeric)])
meltedeuropedata <- melt(correurope)
corrplotcentraleurope <- ggplot(meltedeuropedata,aes(x=Var1, y = Var2,fill=value))+ geom_tile()+ theme(axis.text.x = element_text(angle=45,hjust = 1))
corrplotcentraleurope
summary(dataeasterneurope)

easterneuropeboxplot <- ggplot(dataeasterneurope,aes(x=Ladder.score, y = Regional.indicator,fill=Regional.indicator),)+geom_boxplot()
easterneuropeboxplot

countrybarchart <- ggplot(dataeasterneurope, aes(x=Ladder.score, y = Country.name,fill = Ladder.score))+geom_bar(stat="identity")
countrybarchart
#Can see from above plot the Czech republic is the happiest and Albania/North Macedonia the least happy, these 3 countries are possible outliers

countryplot <- ggplot(dataeasterneurope, aes(y=Ladder.score, x = reorder(Country.name,-Ladder.score))) + geom_point(colour="blue",size=10)+geom_text(aes(label=Ladder.score),hjust = 0,vjust=-1.5) + labs(title = "Happines Scope for Central European Countries",x="Country",y="Happiness Score")
countryplot

#We have now looked at 2021 happiness scores, let's look at past trends
easterneuropeancountries <- distinct(dataeasterneurope,Country.name)
#The belwo line converts the tibble to a vector
easterneuropeancountries = pull(easterneuropeancountries)

fulldatapast_easterneurope<-fulldatapast %>% filter(Country.name  %in% easterneuropeancountries)
View(fulldatapast_easterneurope)
#Plot below shows happiness trends of Eastern European countries
trendplot = ggplot(fulldatapast_easterneurope,aes(x=year,y=Ladder.score,col=Country.name))+geom_line(size=1)+ggtitle("Happiness Score Trend of Eastern European Countries")
trendplot

