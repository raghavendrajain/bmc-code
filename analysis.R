
#Load these packages
require(ggplot2) # used in plotting 
require(reshape)
require(plotly)
require(bnlearn) # used for Bayesian Networks
require(mgcv)    # used for Generalized Additive Modeling 
require(fpp2)
require(lubridate)
require(bsts)
require(dplyr)
require(CausalImpact)
require(xtable)
library(dplyr)
library(tidyr)
library(htmlwidgets)
library(DT)
library(bbmle)
library(reshape)
library(caTools)
library(caret)

## The data about connection of one district to another. I am not sure if there are no mistakes in this dataset. 

connected_district <- read.csv("Datasets/prepare_data/connected_district.csv")
connected_district <- connected_district[,-2] # I removed the Thai name
head(connected_district)

## Correctedness guranteed. Personally verified. 

connected_district_code <- read.csv("Datasets/prepare_data/connected_district_code.csv", header = TRUE, check.names = FALSE)
head(connected_district_code)

## Has geocodes, postal code of each of the district, their names, population and area.

district_code <- read.csv("Datasets/prepare_data/district_code.csv")
head(district_code)

# Tells about the number of communities, population from 2014, families and households. The family and household data are same. 

district_code_with_comunity_data <- read.csv("Datasets/prepare_data/district_code_with_comunity_data.csv")
head(district_code_with_comunity_data)

## The data is about garbage collected on average yearly within each district. Then divided by the number of days we get averaged daily data for each district as well.  

district_garbage_data <- read.csv("Datasets/prepare_data/district_garbage_data.csv")
head(district_garbage_data)

match_district_code_table <- read.csv("Datasets/prepare_data/match_district_code_table.csv")
head(match_district_code_table)

## Result: It seems that district_population is most comprehensive. 

district_population <- read.csv("Datasets/prepare_data/district_population.csv")
head(district_population)

#The temperatur data is collected as Diurnal Temperature Range abbreviated as DTR.  DTR is the difference between the daily maximum and minimum temperature.

AverageDTRInBangkok_2008.2015 <- read.csv("Datasets/prepare_data/AverageDTRInBangkok_2008-2015.csv",check.names = FALSE)

AverageDTRInBangkok_2008.2015 <- subset(AverageDTRInBangkok_2008.2015, select = -2) # we remove the data of 2016
AverageDTRInBangkok_2008.2015[,"Month"] <- month.abb
AverageDTRInBangkok_2008.2015
summary_DTR <- subset(AverageDTRInBangkok_2008.2015, select = -c(Month))

# Melting data to put all data on the same plot
meltdf <- melt(AverageDTRInBangkok_2008.2015, id.vars = "Month")
# Everything on the same plot
meltdf$Month <- factor(meltdf$Month, levels = AverageDTRInBangkok_2008.2015$Month)

#The visualization of DTR within Bangkok throughout the years


plot.ts(summary_DTR, main = "DTR in Bangkok throughout the years shown individually")


#DTR within Bangkok throughout the years put together

ggplot(meltdf,aes(x=Month,y=value,colour=variable,group=variable)) + geom_line(size=1.5) + labs(x="Months",y="Diurnal Temperature Range (DTR)") 
# + ggtitle("The diurnal temperature range (DTR) in Bangkok (2008-2015)") 

# The data for average monthly rainfall in Bangkok 

AverageRainInBangkok_2008.2015 <- read.csv("Datasets/prepare_data/AverageRainInBangkok_2008-2015.csv", check.names = FALSE)

AverageRainInBangkok_2008.2015 <- subset(AverageRainInBangkok_2008.2015, select = -2) #removing 2016
AverageRainInBangkok_2008.2015[,"Month"] <- month.abb
AverageRainInBangkok_2008.2015
summary_Rain<- subset(AverageRainInBangkok_2008.2015, select = -c(Month))


meltdf <- melt(AverageRainInBangkok_2008.2015, id.vars = "Month")
# Everything on the same plot
meltdf$Month <- factor(meltdf$Month, levels = AverageRainInBangkok_2008.2015$Month)

#DTR within Bangkok throughout the years put together.


ggplot(meltdf,aes(x=Month,y=value,colour=variable,group=variable)) + geom_line(size=1.5) + labs(x="Months",y="Mean Monthly Rainfall (mm)") 
#+ ggtitle("Average Monthly Rainfall in Bangkok (2008-2015)") 

#DengueSurveillaneData

## Importing the data for dengue incidences
dengue_bangkok_district_level_2008.2015 <- read.csv("Datasets/prepare_data/dengue_bangkok_district_level_2008-2015.csv", header = TRUE)

# Checking the data
head(dengue_bangkok_district_level_2008.2015) 

## Taking the data only for DHF. The codes are:
# DF = 66
# DHF = 26
# DSS = 27 

DHF_bangkok_district_level_2008.2015 <- subset(dengue_bangkok_district_level_2008.2015, disease_code == 26)
DHF_bangkok_district_level_2008.2015 <- subset(DHF_bangkok_district_level_2008.2015, (date_sick_year > 2007) & (date_sick_year < 2016 ))
unique(DHF_bangkok_district_level_2008.2015$date_sick_year)

# Removing the columns about disease code and district codes. The yearly data is presented below
DHF_total_2008.2015 <- subset(DHF_bangkok_district_level_2008.2015, select = - c(geocode_district, disease_code ))
head(DHF_total_2008.2015)



## Aggregating the data on DHF, according to the year. 
totalDHF <- aggregate(count ~ date_sick_year, DHF_total_2008.2015,sum)

totalDHF

# The DHF incidence peaked in 2013 and 2015. It seems that dengue outbreak increases every alternative year.

ggplot(totalDHF, aes(x = date_sick_year, y = count )) + geom_bar(stat = "identity")  + labs(x="Year",y="Annual DHF incidences")
#+ ggtitle("Reported annual DHF incidents in Bangkok (2008-2015)") 

# We can observe a trend in Fig \@ref(fig:DHFTotal) that dengue outbreak increases every alternative year. But is it really a pattern? 
# Well, we don't have enough data for say that, however we will check and see if we can exploit this 'pattern' for better prediction of dengue.

## DegueSurveillaneData_Monthly


totalDHF_month <- aggregate(count ~ date_sick_month, DHF_total_2008.2015, sum)
totalDHF_month$date_sick_month <- month.abb
totalDHF_month$date_sick_month  <- factor(totalDHF_month$date_sick_month, levels = month.abb)

#The DHF incidence peaked in October and November . It seems that dengue outbreak increases every alternative year.

ggplot(totalDHF_month, aes(x = date_sick_month, y = count )) + geom_bar(stat = "identity")  + labs(x="Months",y="Monthly aggregate DHF incidences")
# + ggtitle("Reported Monthly DHF incidents in Bangkok (2008-2015)") 

## totalDHF_TimeWise 

totalDHF_TimeWise <- aggregate(count ~ date_sick_year + date_sick_month , DHF_total_2008.2015, sum)
totalDHF_TimeWise$date_sick_month <- factor(totalDHF_TimeWise$date_sick_month)
levels(totalDHF_TimeWise$date_sick_month) <- month.abb


#The DHF incidence peak every year in the months of October and November. However in the year 2013 it seems that DHF occured for many months continously at an alarming rate.

ggplot(totalDHF_TimeWise, aes(x = date_sick_month, y = count)) + geom_bar(stat = "identity") + labs(x="Months",y="Monthly Aggregate DHF incidences Across Years") + facet_grid(date_sick_year ~. )
# + ggtitle("Reported DHF incidents in Bangkok (2008-2015)") 

## Rearranging the columns 

totalDHF_TimeWise <- totalDHF_TimeWise[, c(2,1,3)]
meltdf<- melt(totalDHF_TimeWise, id.vars = c("date_sick_month", "date_sick_year"))
df_DHF <- cast(totalDHF_TimeWise, date_sick_month ~ date_sick_year )

# df_DHF <- df_DHF[c(1, 9:2)]

meltdf <- melt(df_DHF, id.vars = "date_sick_month")

ggplot(meltdf,aes(x=date_sick_month,y=value, colour = as.character(date_sick_year), group = as.character(date_sick_year) )) + geom_line(size=1.5) + ggtitle("Reported DHF incidents in Bangkok (2008-2015)") + labs(x="Months",y="Aggregated Monthly Count Data Across Years") + guides(fill=guide_legend(title=NULL))

## Reordering columns to be in the same format as that of other data frames. 
df_DHF <- df_DHF[c(1, 9:2)]



#### Data frames

library(reshape)

## This makes the list of dataframes for each district

DHF_dist_2008.2015 <- subset(DHF_bangkok_district_level_2008.2015, select = - c(disease_code ))
length(unique(DHF_dist_2008.2015$geocode_district))

listofdfs <- list()
totalDHFdfs <- list()
meltdfs <- list()
df_DHFs <- list()

for (i in 1000:1050)
{
  listofdfs[[i]] <- subset(DHF_dist_2008.2015, geocode_district == i  )
  totalDHFdfs[[i]] <- aggregate(count ~ date_sick_year + date_sick_month, listofdfs[[i]], sum)
  meltdfs[[i]] <- melt(totalDHFdfs[[i]], id.vars = c("date_sick_month", "date_sick_year"))
  df_DHFs[[i]] <- cast( meltdfs[[i]], date_sick_month ~ date_sick_year )
  
}


### Combing Predictors

## Interaction of Meteorological Variables

library(reshape)
library(ggplot2)
library(plyr)
library(dplyr)

## We Start with taking the large df of DHF incidents. 

head(DHF_dist_2008.2015)
names(DHF_dist_2008.2015) # "geocode_district" "date_sick_year"   "date_sick_month"  "count" 

# We add the DTR and rainfall data in the same dataframe.  
# Both these data are same for all districts. They vary only according to the year and month. 

#Ordering the dataframe according to the month.
DHF_dist_2008.2015 <- DHF_dist_2008.2015[order(DHF_dist_2008.2015$date_sick_month), ] 

#    geocode_district date_sick_year date_sick_month count
# 55              1001           2008               1     6
# 77              1001           2009               1     1
# 99              1001           2010               1    12
# 120             1001           2011               1     4
# 141             1001           2012               1     1
# 162             1001           2013               1    35

# To match with the above dataframe, I will replace the month names to integers from 1 to 12
AverageDTRInBangkok_2008.2015$Month <- c(1:12)
AverageRainInBangkok_2008.2015$Month <- c(1:12)

## Now melting the DTR and rainfall dataframes

meltDTR <- melt(AverageDTRInBangkok_2008.2015, id.vars = "Month")
names(meltDTR) <- c("date_sick_month", "date_sick_year", "DTR")

meltRainfall <- melt(AverageRainInBangkok_2008.2015, id.vars = "Month")
names(meltRainfall) <- c("date_sick_month", "date_sick_year", "Rainfall")

## Climate variables are merged together for all years and months
climateTotal <- merge(meltDTR, meltRainfall)

# The temperature and rainfall data rogether acroos several years

ggplot(climateTotal, aes(x = Rainfall, y = DTR, color )) + geom_point(size=1.5)  + ggtitle("Interaction of Climate Variables (2008-2015)") + labs(x="Rainfall (mm)",y="Diurnal Temperature Range") + facet_grid( date_sick_year ~ date_sick_month ) 

# The temperature and rainfall data rogether in the same plot.

ggplot(climateTotal, aes(x = Rainfall, y = DTR, group = date_sick_year, color = date_sick_year)) + geom_point(size=1.5)  + ggtitle("Interaction of Climate Variables (2008-2015)") + labs(x="Rainfall (mm)",y="Diurnal Temperature Range") + facet_grid( . ~ date_sick_month ) 


# In this chunk, I will deal with the garbage data 

library(matrixStats)

district_garbage <- subset(district_garbage_data, select = -c(district_name,AMP_ID, Gabage.Per.day.2012, Gabage.Per.day.2013, Gabage.Per.day.2014 ))

avgMonGarbage <- rowMeans(district_garbage[, c(2,3,4)])/12
stDMonGarbage <- apply(district_garbage[,-1], 1, sd)/12

district_garbage  <- subset(district_garbage , select = -c(Total.Gagbage.2012, Total.Gagbage.2013, Total.Gagbage.2014))
district_garbage <- cbind(district_garbage,avgMonGarbage, stDMonGarbage)

## ordering the data frame according to district names

district_garbage <- district_garbage[order(district_garbage$geocode_district),]


dist_gb_col <- data.frame()

for (i in 1:50){
  
  garbage <- rnorm(96, mean =  district_garbage[i,2], sd = district_garbage[i,3])
  distCollection <- cbind(rep(1000 + i,96), garbage)
  dist_gb_col <- rbind(dist_gb_col, distCollection )
}

colnames(dist_gb_col) <- c("geocode_district", "garbage")
dist_gb_col$geocode_district <- as.integer(dist_gb_col$geocode_district)
class(dist_gb_col$geocode_district)

## Merging climate variables with DHF data
total <- merge(DHF_dist_2008.2015, climateTotal) 
district_population
names(district_population)

total <- merge(total, district_population, by= "geocode_district") 

any(is.na(total))


# Sum of the neighbors of each district

neighbors<- apply(connected_district_code[,-1],1, sum)

# The following df lists the names of district and their neighbors
dist_neighbor <- cbind(data.frame(connected_district_code$geocode_district), data.frame(neighbors))
names(dist_neighbor) <- c("geocode_district", "neighbors")

total <- merge(total, dist_neighbor, by= "geocode_district") 

names(total)

# totalDataSaved

entireData <- total
#View(entireData)
orderedEntireData <-entireData[order(entireData$date_sick_year, entireData$date_sick_month), ]

# The dataset consists of following entries:
#
# 1. Information of each district
#     + identification codes
#     + name of the district.
#     + population (divided into various age bins)
#     + area in square kms.
#     + Number of communities
#     + number of neighboring districts
# 2. Monthly DHF count in each district from 2008~2015
# 3. Monthly average rainfall in Bangkok from 2008~2015
# 4. Monthly Diurnal Temperature Range (DTR) in Bangkok from 2008~2015

#closeToStream

dist_on_stream <- NULL
dist_cl_stream <- c(29,2,25,1,20,16,15,18,8,13,4,28,31,12,24,33,9,47)
for (i in dist_cl_stream){
  code = 1000 + i
  name <- subset(orderedEntireData, geocode_district ==  code)$district_name[1] 
  #print (as.character(name), quote = FALSE)
  dist_on_stream  <- c(dist_on_stream, as.character(name))
  #cat(as.character(name), ",")
  
}


# There are `r length(dist_on_stream)` districts that are close to stream as shown in Figure \@ref(fig:bkk-map).

print (dist_on_stream)



## This chunk creates implicit missing data into explicit missing data. 

library(reshape)
library(tidyr)

orderedEntireData <- orderedEntireData %>%  group_by(geocode_district,date_sick_year, district_name,
                                                     population_1..From.DDC.Source., 
                                                     Community, area_km2, population_2..From.Bangkok.stat., Age...35, Age..35, X0.4, 
                                                     X5.9, X10.14, X15.19, X20.24,
                                                     X25.29, X30.34, X35.39, X40.44, X45.49, X50.54, X55.59, X60.64, X65.69, X70.74, X75.79, X80.84, X85.89, X90.94, X95.99, X..100, neighbors) %>% complete(date_sick_month = 1:12)


orderedEntireData$district_name.y <- NULL
orderedEntireData$AMP_ID <- NULL
orderedEntireData$time <- rep(1:96, 50) #Time variable is input as additional column

orderedEntireData <- as.data.frame(orderedEntireData)


garbage <- dist_gb_col$garbage
## Attaching the district garbage data with the entire dataset
orderedEntireData  <- cbind(orderedEntireData, garbage )
## Attaching the status (binary) based on whether districts are close to stream or not. 

index <- dist_cl_stream + 1000
nextToStream <- orderedEntireData$geocode_district %in% index
orderedEntireData  <- cbind(orderedEntireData, nextToStream)

## Now we know which district is next to a river stream and which are simply NOT


## The dataset that has all the required values is "orderedEntireData""

#orderedEntireData

## Finding out which of the columns in that dataframe have missing values

colnames(orderedEntireData)[colSums(is.na(orderedEntireData)) > 0]

#The result is
#[1] "count"    "DTR"      "Rainfall"
## Thus we need to fill the data using predictive mean matching


## In the dataset, for some of the 50 districts, we don't have the data for DTR and rainfall.
## However, since the entire Bangkok have the same DTR and rainfall, we will use that data to replace NA from DTR and Rainfall data. 

##  The data is of 8 years i.e. 96 entries. 

y <- complete.cases(orderedEntireData) # take the completed cases
index_NA <- which(!y)   # index on rows with NAs

# time at which NA values were reported
timePoint <- subset(orderedEntireData[index_NA,], select = c("date_sick_year", "date_sick_month" ) )
# decomposing into years and months

year<- as.character(timePoint$date_sick_year)
month<- timePoint$date_sick_month

# storing them in a dataframe
rain_df <- NULL
temp_df <- NULL

for (i in 1:length(month)){
  rain_df[i] <- AverageRainInBangkok_2008.2015[month[i], year[i]]
  temp_df[i] <- AverageDTRInBangkok_2008.2015[month[i], year[i]]
}

## putting them into the dataset

orderedEntireData[index_NA, "DTR"] <- temp_df
orderedEntireData[index_NA, "Rainfall"] <- rain_df

## Now the only column with missing values is "count"


## I wanted to use preditive mean matching for the imputation. 
# However, some of the imputed values were negative for the countd data, which is unacceptable. The online help suggested that this could be because the data was not normally distribited, so they suggested me to take the log of the data. I did test that and found that taking log, did help normalize my data. 
# However, I used another technique called "locf", which simply imputes the value of last month of the same year. 

require(forecast)

DHF_data <- list()
distChunk <- list()
d <- list()
tsa <- list()

for (i in 1:50){
  distChunk[[i]] <- subset(orderedEntireData, geocode_district == 1000 + i )
  DHF_data[[i]] <- distChunk[[i]]$count
  tsa[[i]] <-ts(DHF_data[[i]],start=c(2008,1),end=c(2015,12),frequency=12)
  tsa[[i]]<- na.locf(tsa[[i]])
  tsa[[i]] <- as.vector(tsa[[i]])
}

## combining the list of dataframes

completeCounts <- ldply(tsa, data.frame)

orderedEntireData_withoutNA <- orderedEntireData
orderedEntireData_withoutNA$count <- completeCounts[,1]

## The dataframe "completedEntData" has all the count data i.e. NO missing values.



## Exploring the connected districts

View(connected_district_code)

temp <- subset(connected_district_code, select = - geocode_district)
list_neigh<- apply(temp, 1, function(i) which(i %in% "1"))

## In the row of a particular district, I want to add the dengue incidence counts from its nearby districts that occur at the same time.

N <- length(list_neigh)
allDistrictData <- list()


dist_n <- list()
list_distData <- list()
neighborsDf <- NA


for (i in 1:N){
  distCode <- 1000 + i
  p <- subset(orderedEntireData, geocode_district ==    distCode)
  dist_n <- list_neigh[i][[1]] # taking only data, not index $`i`
  print (dist_n)
  
  #neighborsDf <- data.frame()
  for (j in 1:length(dist_n)){
    l <- length(dist_n)
    neighCode <- 1000 + dist_n[j]
    print (neighCode)
    if (dist_n[j] %in% dist_cl_stream) ## If the neighbor is next to stream
    {
      name <- paste0("neighCountNextStream", j )
    }
    else
    {
      name <- paste0("neighCount", j ) ## otherwise keep the name normal
    }
    ## name <- paste0("neighCountNextStrea", j )
    p[name]<- subset(orderedEntireData, geocode_district == neighCode)$count
  }
  
  print (p)
  neighborColIndex <- grep("neighCount", colnames(p))
  p$totalSurroundingDengue <- rowSums(p[, c(neighborColIndex) ], na.rm = TRUE)
  p$neigNextToStream <- sum(dist_n %in% dist_cl_stream, na.rm = TRUE)
  allDistrictData[[i]] <- p
}


## Now I have the data frame with all neighbor information, nearby dengue counts. 



## Also, i will need to create lagged values for all the district dengue data.
## checkout the dynamic lag-linear models when dengue data is imputed. Also see whether they have restropective influence.


## The following data frame contains everything. 

WholeData <- ldply(allDistrictData, data.frame)

## Creating a df with no left value in count data
WholeData_withoutNA <- WholeData
WholeData_withoutNA$count <- completeCounts[,1]
# sapply(WholeData, function(x) sum(is.na(x)))





## Here I created the lagged values for the entire dataset 
## The Algo is:
# 1. Take data into the list, divide the data into dataframes.
# 2. Create the lags. Add them as columns names as I did previously.
# 3. Add all the dataframes into the lists. 

allDistrictLaggedData <- list()

for (i in 1:50) {
  distCode <- 1000 + i
  p <- subset(WholeData, geocode_district ==  distCode)
  N <- nrow(p)
  
  
  # putting the DHF count data into a vector so that we can tak lags
  countVec <- as.vector(p$count)
  # Lag 0 dengue count variable
  p$count0 <- countVec
  # to append lag variable names, I put the existing names into a column vector
  temp <-colnames(p)
  #totally we need to take the lag of 5 years i.e. 60 months
  
  #totally we need to take the lag of 5 years i.e. 60 months
  
  countLag <- 1
  
  for (j in 1:60){
    #print (j);
    l <- N-j;
    countLag[j] <- paste0("count",j)
    laggedCountVec <- c(rep(NA,j), countVec[1:l]) 
    p <- cbind(p, laggedCountVec)
  }
  
  # creating dengue lag variables for 60 momnths 
  colnames(p) <- c(temp, countLag)
  
  
  ## Creating Lag Variables for total surrounding dengue 
  
  
  # putting the DHF count data into a vector so that we can tak lags
  surroundingCountVec <- as.vector(p$totalSurroundingDengue)
  # Lag 0 dengue count variable
  p$surroundingCountVec0 <- surroundingCountVec
  # to append lag variable names, I put the existing names into a column vector
  temp <-colnames(p)
  #totally we need to take the lag of 5 years i.e. 60 months
  
  #totally we need to take the lag of 5 years i.e. 60 months
  
  countLag <- 1
  
  for (j in 1:60){
    #print (j);
    l <- N-j;
    countLag[j] <- paste0("surroundingCountVec",j)
    laggedCountVec <- c(rep(NA,j), surroundingCountVec[1:l]) 
    p <- cbind(p, laggedCountVec)
  }
  
  # creating dengue lag variables for 60 momnths 
  colnames(p) <- c(temp, countLag)
  
  
  
  ## Now creating lag variables for DTR
  
  
  temperatureVec <- as.vector(p$DTR)
  p$templ0 <- temperatureVec
  temp <- colnames(p)  
  
  tempLag <- 1
  for (j in 1:4){
    #print (j);
    l <- N-j;
    tempLag[j] <- paste0("templ",j)
    laggedDTR <- c(rep(NA,j), temperatureVec[1:l]) 
    p <- cbind(p, laggedDTR)
  }
  
  
  # creating temperature lag variables for last 4 momnths 
  colnames(p) <- c(temp, tempLag)
  
  ## Now creating lag variables for Rainfall
  
  # putting the Rainfall into a vector so that we can take lags
  RainFallVec <- as.vector(p$Rainfall)
  # Lag 0 dengue count variable
  p$rainl0 <- RainFallVec
  
  # to append lag variable names, I put the existing names into a column vector
  temp <-colnames(p)
  
  ## Now creating lag variables for Rainfall
  
  # putting the Rainfall into a vector so that we can take lags
  RainFallVec <- as.vector(p$Rainfall)
  # Lag 0 dengue count variable
  p$rainl0 <- RainFallVec
  
  # to append lag variable names, I put the existing names into a column vector
  temp <-colnames(p)
  
  RainLag <- 1
  for (j in 1:4){
    #print (j);
    l <- N-j;
    RainLag[j] <- paste0("rainl",j)
    laggedRainVec <- c(rep(NA,j), RainFallVec[1:l]) 
    p <- cbind(p, laggedRainVec)
  }
  
  
  # creating rainfall lag variables for 60 momnths 
  colnames(p) <- c(temp, RainLag)
  
  allDistrictLaggedData[[i]] <- p
  
}

WholeDataLagged <- ldply(allDistrictLaggedData, data.frame)
# sapply(WholeDataLagged , function(x) sum(is.na(x)))






## Here I created the lagged values for the entire dataset 
## The Algo is:
# 1. Take data into the list, divide the data into dataframes.
# 2. Create the lags. Add them as columns names as I did previously.
# 3. Add all the dataframes into the lists. 

allDistrictLaggedData_withoutNA <- list()

for (i in 1:50) {
  distCode <- 1000 + i
  p <- subset(WholeData_withoutNA, geocode_district ==  distCode)
  N <- nrow(p)
  
  
  # putting the DHF count data into a vector so that we can tak lags
  countVec <- as.vector(p$count)
  # Lag 0 dengue count variable
  p$count0 <- countVec
  # to append lag variable names, I put the existing names into a column vector
  temp <-colnames(p)
  #totally we need to take the lag of 5 years i.e. 60 months
  
  #totally we need to take the lag of 5 years i.e. 60 months
  
  countLag <- 1
  
  for (j in 1:60){
    #print (j);
    l <- N-j;
    countLag[j] <- paste0("count",j)
    laggedCountVec <- c(rep(NA,j), countVec[1:l]) 
    p <- cbind(p, laggedCountVec)
  }
  
  # creating dengue lag variables for 60 momnths 
  colnames(p) <- c(temp, countLag)
  
  
  ## Creating Lag Variables for total surrounding dengue 
  
  
  # putting the DHF count data into a vector so that we can tak lags
  surroundingCountVec <- as.vector(p$totalSurroundingDengue)
  # Lag 0 dengue count variable
  p$surroundingCountVec0 <- surroundingCountVec
  # to append lag variable names, I put the existing names into a column vector
  temp <-colnames(p)
  #totally we need to take the lag of 5 years i.e. 60 months
  
  #totally we need to take the lag of 5 years i.e. 60 months
  
  countLag <- 1
  
  for (j in 1:60){
    #print (j);
    l <- N-j;
    countLag[j] <- paste0("surroundingCountVec",j)
    laggedCountVec <- c(rep(NA,j), surroundingCountVec[1:l]) 
    p <- cbind(p, laggedCountVec)
  }
  
  # creating dengue lag variables for 60 momnths 
  colnames(p) <- c(temp, countLag)
  
  
  
  ## Now creating lag variables for DTR
  
  
  temperatureVec <- as.vector(p$DTR)
  p$templ0 <- temperatureVec
  temp <- colnames(p)  
  
  tempLag <- 1
  for (j in 1:4){
    #print (j);
    l <- N-j;
    tempLag[j] <- paste0("templ",j)
    laggedDTR <- c(rep(NA,j), temperatureVec[1:l]) 
    p <- cbind(p, laggedDTR)
  }
  
  
  # creating temperature lag variables for last 4 momnths 
  colnames(p) <- c(temp, tempLag)
  
  ## Now creating lag variables for Rainfall
  
  # putting the Rainfall into a vector so that we can take lags
  RainFallVec <- as.vector(p$Rainfall)
  # Lag 0 dengue count variable
  p$rainl0 <- RainFallVec
  
  # to append lag variable names, I put the existing names into a column vector
  temp <-colnames(p)
  
  ## Now creating lag variables for Rainfall
  
  # putting the Rainfall into a vector so that we can take lags
  RainFallVec <- as.vector(p$Rainfall)
  # Lag 0 dengue count variable
  p$rainl0 <- RainFallVec
  
  # to append lag variable names, I put the existing names into a column vector
  temp <-colnames(p)
  
  RainLag <- 1
  for (j in 1:4){
    #print (j);
    l <- N-j;
    RainLag[j] <- paste0("rainl",j)
    laggedRainVec <- c(rep(NA,j), RainFallVec[1:l]) 
    p <- cbind(p, laggedRainVec)
  }
  
  
  # creating rainfall lag variables for 60 momnths 
  colnames(p) <- c(temp, RainLag)
  
  allDistrictLaggedData_withoutNA[[i]] <- p
  
}

WholeDataLagged_withoutNA <- ldply(allDistrictLaggedData_withoutNA, data.frame)
# sapply(WholeDataLagged , function(x) sum(is.na(x)))







## The entire training data is shown here

training2008_2012 <- subset(WholeDataLagged, date_sick_year < 2013)
training2008_2013 <- subset(WholeDataLagged, date_sick_year < 2014)
training2008_2014 <- subset(WholeDataLagged, date_sick_year < 2015)

## The month wise training data is

trainTill2012 <- list()
trainTill2013 <- list()
trainTill2014 <- list()

for (i in 1:50)
{
  distCode <- 1000 + i
  p <- subset(WholeDataLagged, geocode_district ==    distCode)
  trainTill2012[[i]] <-  subset(WholeDataLagged, (geocode_district == distCode) & (date_sick_year < 2013))
  trainTill2013[[i]] <-  subset(WholeDataLagged, (geocode_district == distCode) & (date_sick_year < 2014))
  trainTill2014[[i]] <-  subset(WholeDataLagged, (geocode_district == distCode) & (date_sick_year < 2015))
}

trainTill2012_df <- ldply(trainTill2012, data.frame)
trainTill2013_df <- ldply(trainTill2013, data.frame)
trainTill2014_df <- ldply(trainTill2014, data.frame)



## The entire training data is shown here

training2008_2012_noNA <- subset(WholeDataLagged_withoutNA, date_sick_year < 2013)
training2008_2013_noNA <- subset(WholeDataLagged_withoutNA, date_sick_year < 2014)
training2008_2014_noNA <- subset(WholeDataLagged_withoutNA, date_sick_year < 2015)

## The month wise training data is

trainTill2012_noNA <- list()
trainTill2013_noNA <- list()
trainTill2014_noNA <- list()

for (i in 1:50)
{
  distCode <- 1000 + i
  p <- subset(WholeDataLagged_withoutNA, geocode_district ==    distCode)
  trainTill2012_noNA[[i]] <-  subset(WholeDataLagged_withoutNA, (geocode_district == distCode) & (date_sick_year < 2013))
  trainTill2013_noNA[[i]] <-  subset(WholeDataLagged_withoutNA, (geocode_district == distCode) & (date_sick_year < 2014))
  trainTill2014_noNA[[i]] <-  subset(WholeDataLagged_withoutNA, (geocode_district == distCode) & (date_sick_year < 2015))
}

trainTill2012_noNA_df <- ldply(trainTill2012_noNA, data.frame)
trainTill2013_noNA_df <- ldply(trainTill2013_noNA, data.frame)
trainTill2014_noNA_df <- ldply(trainTill2014_noNA, data.frame)
























### Predictions

###### Generalized Additive (Mixed) Models

## In this function, the gam model is applied indicidually to the list of data frames
library(mgcv)

gam.function <- function(x)  {
  if (nrow(x) < 3) return(" nrow() to small ")
  gam.x <-  gam(form, family=quasipoisson, na.action=na.exclude, data = x)
}

#### association models. All the models are listed below. 


### model 0: Testing with Naive Models



countdata <- trainTill2012_df$count
length(countdata) # this data has 50 districts for 60 months i.e. 5 years
#View(countdata)
countData_m <- matrix(countdata, 60, 50)
dim(countData_m)
#View(countData_m)


monthlyNaive <- function(x){
  monthlyNaviveModel <- c(x[1:12], x[1:12], x[13:24], x[25:36], x[37:48]);
  #temp <- c(monthlyNaviveModel[1:24], monthlyNaviveModel[24:60]);
  return (monthlyNaviveModel);
}



countData_mn <- as.vector(apply(countData_m, 2 , monthlyNaive))

temp<- cbind(trainTill2012_df$count, countData_mn)
View(temp)
# 
# RMSE_MN  <-  sqrt(mean((trainTill2012_df$count-countData_mn )^2,na.rm=T))
# SRMSE_MN <-  sqrt(mean((trainTill2012_df$count-countData_mn)^2,na.rm=T))/sqrt(mean((trainTill2012_df$count)^2,na.rm=T))
# 


form <- as.formula("count ~ s(countData_mn,k=4)")
mod.MN <- gam(form, family=quasipoisson, na.action=na.exclude, data = trainTill2012_df)
summary(mod.MN)
plot.gam(mod.MN)






# 1. meteorology only model - all variables all lags

## the formula
form <- as.formula("count ~ s(templ0, k=4) + s(templ1, k=4) + s(templ2,k=4) + s(templ3,k=4) + s(rainl0,k=4) + s(rainl1,k=4) + s(rainl2,k=4) + s(rainl3,k=4)")
## To the entire dataset, returns the model 
mod.DM <- gam(form, family=quasipoisson, na.action=na.exclude, data = trainTill2012_df)
summary(mod.DM)
plot.gam(mod.DM)


## 2.  meteorology only model the formula for lag>1
form <- as.formula("count ~ s(templ1, k=4) + s(templ2,k=4) + s(templ3,k=4) + s(rainl1,k=4) + s(rainl2,k=4) +  s(rainl3,k=3)")
## To the entire dataset, returns the model 
mod.DMgt1 <- gam(form, family=quasipoisson, na.action=na.exclude, data = trainTill2012_df)
summary(mod.DMgt1)




# 2. Only past dengue incidences model - all variables all lags

## the formula

form1 <- as.formula("count ~ s(count1,k=4) + s(count2,k=4) + s(count3,k=4) + s(count4,k=4)")
## To the entire dataset, returns the model 

## Short-term incidences
mod.DDShort <- gam(form1, family=quasipoisson, na.action=na.exclude, data = trainTill2012_df)

## Short and long together
form2 <- as.formula("count ~ s(count1,k=4) + s(count2,k=4) + s(count3,k=4) + s(count4,k=4) + s(count5,k=4) + s(count6,k=4) + s(count7,k=4) + s(count8,k=4) + s(count9,k=4) + s(count10,k=4) + s(count11,k=4) + s(count12,k=4) + s(count13,k=4) + s(count14,k=4) + s(count15,k=4) + s(count16,k=4) + s(count17,k=4) + s(count18,k=4) + s(count19,k=4) + s(count20,k=4) + s(count21,k=4) + s(count22,k=4) + s(count23,k=4) + s(count25,k=4) + s(count26,k=4) + s(count27,k=4) + s(count28,k=4) + s(count29,k=4) + s(count30,k=4) ")

## Long-Term 
mod.DDLong <- gam(form2, family=quasipoisson, na.action=na.exclude, data = trainTill2012_df)
## The closer analysis shows that for dengue lag 1, lag 18 and lag 23 is the most significant. 

form.Optimal <- as.formula("count ~ s(count1,k=4) + s(count2,k=4) + s(count23,k=4)")

mod.DDOptimal <- gam(form.Optimal, family=quasipoisson, na.action=na.exclude, data = trainTill2012_df)





# 3. meteorology and past dengue incidences model - all variables lag > 1

## the formula
form <- as.formula("count ~ s(templ1, k=4) + s(templ2,k=4) + s(templ3,k=4) +s(rainl1,k=4) + s(rainl2,k=4) + s(count1,k=4) + s(count2,k=4) +  s(count23,k=4)")

## To the entire dataset, returns the model 
mod.DMDgt1 <- gam(form, family=quasipoisson, na.action=na.exclude, data = trainTill2012_df)

# check the summary
summary(mod.DMDgt1)

## check for collinearity






# 4. meteorology, past dengue incidences and surrounding dengue incidences model - all variables all lags

## the formula
form <- as.formula("count ~   s(templ1, k=4) + s(templ2,k=4) + s(templ3,k=4) + s(rainl1,k=4) + s(rainl2,k=4) + s(rainl3,k=4) + s(count1,k=4) + s(count2,k=4) + s(count23,k=4) +  s(surroundingCountVec1,k=4) + s(surroundingCountVec2,k=4)")


## To the entire dataset, returns the model 
mod.DMDS_Shortgt1 <- gam(form, family=quasipoisson, na.action=na.exclude, data = trainTill2012_df)


form <- as.formula("count ~  s(templ1, k=4) + s(templ2,k=4) + s(templ3,k=4) + s(rainl1,k=4) + s(rainl2,k=4) + s(rainl3,k=4) + s(count1,k=4) + s(count2,k=4) + s(count23,k=4) +  s(surroundingCountVec1,k=4) + s(surroundingCountVec2,k=4) +  s(surroundingCountVec12,k=4)")

mod.DMDS_Optimalgt1 <- gam(form, family=quasipoisson, na.action=na.exclude, data = trainTill2012_df)
summary(mod.DMDS_Optimalgt1)



# including lags >=1



# 5. Garbage offset along with meteorology, past dengue incidences and surrounding dengue incidences model - all variables all lags

## the formula
form <- as.formula("count ~  s(templ1, k=4) + s(templ2,k=4) + s(templ3,k=4)  + s(rainl1,k=4) + s(rainl2,k=4) + s(rainl3,k=4) + s(count1,k=4) + s(count2,k=4) + s(count23,k=4) +  s(surroundingCountVec1,k=4) + s(surroundingCountVec2,k=4) +  s(surroundingCountVec12,k=4) + offset(log(garbage)) + s(garbage)")

## To the entire dataset, returns the model 
mod.DMDSO <- gam(form, family=quasipoisson, na.action=na.exclude, data = trainTill2012_df)

summary(mod.DMDSO)



d <- matrix(trainTill2012_df$count, nrow = 60, byrow = FALSE)
d <- as.data.frame(d)
dSum <- rowSums(d,na.rm = TRUE)

# ### Meterological Data 
# 
# In this model, the association of meterologiocal variables i.e. DTR and averrage monthly rainfall is considered.  I call this **Dengue-Meteorological model**.
# 
# 
# The above summary in Appendix \@ref(appDM) suggests that all the temperature and rain lag variables are important factors. Let's visualize the additive model in Figure \@ref(fig:D-M). 
# 


# ```{r D-M, fig.cap= '**Association between the meteorological variables and dengue over lags of 0-3 months.**. Solid lines represent relative risks (RR) of dengue cases and dottted lines depict the upper and lower limits of 95% confidence intervals.', fig.align='center', echo=FALSE}

png("D-M-1.png",width=1600, height=1300, res=300)
par(mfrow=c(2,4))
plot.gam(mod.DM, ylab="log(RR)")
dev.off()


png("D-M-1-gt1.png",width=1600, height=1300, res=300)
par(mfrow=c(2,4))
plot.gam(mod.DMgt1, ylab="log(RR)")
dev.off()



### Plot monthly naive model


par(mfrow=c(1,1))
p <- fitted.values(mod.MN)
length(p)

a<- predict(mod.MN, type="response")
a <- matrix(a, nrow = 60, byrow = FALSE)
a <- as.data.frame(a)
aSum <- rowSums(a ,na.rm = TRUE)
is.na(aSum) <- !aSum

png("Pred-SeasonalNaive.png",width=1600, height=1300, res=300)
plot(1:60, dSum, type="l",ylab="Dengue Cases",axes=T,xlab="Months")
points(aSum,type="l", col="red")
legend(x = 20, y = 1400, c("Observed", "Predicted"), text.font = 40,  lty=1, col=c('black', 'red'), bty='n', cex=.75)
title(main="(A) Seasonal naïve method")

dev.off()


d<-matrix(p, 60, 50)
d<- as.vector(apply(d, 2 , function(x) { c(rep(NA,23), x[24:60]) }))
temp<- cbind(trainTill2012_df$count,p, d)
#View(temp)
RMSE_MN <-  sqrt(mean((trainTill2012_df$count-d)^2,na.rm=T))
SRMSE_MN <-  sqrt(mean((trainTill2012_df$count-d)^2,na.rm=T))/sqrt(mean((trainTill2012_df$count)^2,na.rm=T))
r.sq_MN <-   summary(mod.MN)$r.sq
dev.expl_MN <-  summary(mod.MN)$dev.expl

summary_MN <- data.frame("Seasonal naïve method", RMSE_MN, SRMSE_MN, r.sq_MN, dev.expl_MN)
names(summary_MN) <- c("Model Name", "RMSE", "SRMSE", "R-sq.(adj)", "Deviance Explained")
summary_MN










# ```

# ```{r DMPred, echo=FALSE, fig.cap="Monthly Observed and predicted dengue cases (2008-2012). "}

par(mfrow=c(1,1))
p <- fitted.values(mod.DMgt1)

a<- predict(mod.DMgt1, type="response")
a <- matrix(a, nrow = 60, byrow = FALSE)
a <- as.data.frame(a)
aSum <- rowSums(a ,na.rm = TRUE)
is.na(aSum) <- !aSum

png("Pred-MetOptimal.png",width=1600, height=1300, res=300)
plot(1:60, dSum, type="l",ylab="Dengue Cases",axes=T,xlab="Months")
points(aSum,type="l", col="red")
legend(x = 20, y = 1400, c("Observed", "Predicted"), text.font = 40,  lty=1, col=c('black', 'red'), bty='n', cex=.75)
title(main="(B) Optimal Met Model")

dev.off()


d<-matrix(p, 60, 50)
d<- as.vector(apply(d, 2 , function(x) { c(rep(NA,23), x[24:60]) }))
temp<- cbind(trainTill2012_df$count,p, d)
#View(temp)
RMSE_DM <-  sqrt(mean((trainTill2012_df$count-d)^2,na.rm=T))
SRMSE_DM <-  sqrt(mean((trainTill2012_df$count-d)^2,na.rm=T))/sqrt(mean((trainTill2012_df$count)^2,na.rm=T))


r.sq_DM <-   summary(mod.DM)$r.sq
dev.expl_DM <-  summary(mod.DM)$dev.expl

summary_DM <- data.frame("Optimal Met Model", RMSE_DM, SRMSE_DM, r.sq_DM, dev.expl_DM)
names(summary_DM) <- c("Model Name", "RMSE", "SRMSE", "R-sq.(adj)", "Deviance Explained")
summary_DM







# ```{r DMTable, echo=FALSE, comment=NA}
# knitr::kable(summary_DM, booktabs = TRUE,
#              caption = 'Predictive Performance Statistics of Metereology Model.'
# )
# ```


### Dengue Surveillance Data 

# In this model the association of past denge incidences is considered. 

#### Short-term Lag Model

# The summary of the model is shown in Appendix \@ref(appDDShort). Let's visualize the additive model in Figure \@ref(fig:DDShort). 

# ```{r DDShort, fig.cap= '**Association between past dengue count over lags of 1-4 months and the dengue outbreak.**. Solid lines represent relative risks (RR) of dengue cases and dottted lines depict the upper and lower limits of 95% confidence intervals.', fig.align='center', echo=FALSE}



# png("Fit-ShortLag.png",width=1600, height=1300, res=300)

par(mfrow=c(1,4))
plot.gam(mod.DDShort, ylab="log(RR)")

# dev.off()


# ```{r DDShortPred, echo=FALSE, fig.cap="Monthly Observed and predicted dengue cases (2008-2012). "}

model <- mod.DDShort ## name of the model 

par(mfrow=c(1,1))
p <- fitted.values(model)
a <- predict(model, type="response")
a <- matrix(a, nrow = 60, byrow = FALSE)
a <- as.data.frame(a)
aSum <- rowSums(a ,na.rm = TRUE)
is.na(aSum) <- !aSum

png("Pred-ShortTerm.png",width=1600, height=1300, res=300)

plot(1:60, dSum, type="l",ylab="Dengue Cases",axes=T,xlab="Months")
points(aSum,type="l", col="red")
legend('top', c("Observed", "Predicted"),lty=1, col=c('black', 'red'), bty='n', cex=.75)

title(main="Short-term Lag Surveillance Model")

dev.off()


d<-matrix(p, 60, 50)
d<- as.vector(apply(d, 2 , function(x) { c(rep(NA,23), x[24:60]) }))
temp<- cbind(trainTill2012_df$count,p, d)
#View(temp)

RMSE_DDShort <-  sqrt(mean((trainTill2012_df$count-d)^2,na.rm=T))
SRMSE_DDShort<-  sqrt(mean((trainTill2012_df$count-d)^2,na.rm=T))/sqrt(mean((trainTill2012_df$count)^2,na.rm=T))

# RMSE_DDShort <-  sqrt(mean((trainTill2012_df$count-p)^2,na.rm=T))
# SRMSE_DDShort<-  sqrt(mean((trainTill2012_df$count-p)^2,na.rm=T))/sqrt(mean((trainTill2012_df$count)^2,na.rm=T))
r.sq_DDShort <-   summary(model)$r.sq
dev.expl_DDShort <-  summary(model)$dev.expl

summary_DDShort <- data.frame("Short-term Lag Surveillance Model", RMSE_DDShort, SRMSE_DDShort, r.sq_DDShort, dev.expl_DDShort)
names(summary_DDShort) <- c("Model Name", "RMSE", "SRMSE", "R-sq.(adj)", "Deviance Explained")
summary_DDShort


# # ```{r DDShortTable, echo=FALSE, comment=NA}
# # knitr::kable(summary_DDShort, booktabs = TRUE,
# # caption = 'Predictive Performance Statistics of Short-term Lag Model.'
# # )
# ```


#### Long-term Lag Model

# ```{r AR_GAM, echo=FALSE, results='hide'}

library(dlnm)

totalDHF <- aggregate(count ~ date_sick_year + date_sick_month, trainTill2012_df ,sum)
totalDHF <- totalDHF[order(totalDHF$date_sick_year), ]

## Taking a one lag 

totalDHF_lagged <- aggregate(count1 ~ date_sick_year + date_sick_month, trainTill2012_df ,sum)
totalDHF_lagged <- totalDHF_lagged[order(totalDHF_lagged$date_sick_year), ]
totalDHF_lagged <- c(NA, totalDHF_lagged$count1)



cb1.temp <- crossbasis(totalDHF_lagged, lag=30, argvar=list(df=4), arglag=list(fun="poly",degree=4))
summary(cb1.temp)
mod.dlnm <- glm(totalDHF$count ~  cb1.temp , family=quasipoisson())
pred1.temp <- crosspred(cb1.temp, mod.dlnm)

# to find the lag-response of a unit increase in dengue cases, we need to "var". var' must match values used for prediction.  This value should be within the range of totalDHF_lagged.

range(totalDHF_lagged, na.rm = TRUE)



# 
# 
# I show the simulated lagâ€“response surfaces as relative risk in Figure \@ref(fig:lag-response). 
# 
# ```{r lag-response, echo=FALSE, results="hide", fig.cap= 'This shows the relation between the case intensity and dengue incidences at the lag months', fig.align='center',}


# png("LongLagContour.png",width=1600, height=1300, res=300)

# plot(pred1.temp, "contour", key.title=title("risk"),
#      main = "Influence of Long-term Lagged Data of Target Districts",xlab="Dengue Counts",ylab="Lag (months)")

plot(pred1.temp, "contour", key.title=title("risk"),xlab="DHF incidences",ylab="Lag (months)")


# dev.off()

png("LagResposeLongLag.png",width=1600, height=1300, res=300)

plot(pred1.temp, "slices", var=1100, col=1, ylab="RR",
     ci.arg=list(density=15,lwd=2),
     main="Lag-response curve for a 1100-unit increase in dengue cases")

dev.off()

# png("LagResposeLongLagMany.png",width=1600, height=1300, res=300)

plot(pred1.temp, "slices", var=1100, col=1, ylab="RR",
     ci.arg=list(density=15,lwd=2.5))
    # main="Lag-response curve for differing number of dengue cases")

for(i in 1:3) lines(pred1.temp, "slices", var=c(900,1000,1200)[i], col=i+1, lwd=1.5)
legend("topleft",paste("Dengue Units =",c(1100,900,1000,1200)), col=1:4, lwd=1.5)

# dev.off()



# The summary of the model is shown in Appendix \@ref(appDDOptimal). Let's visualize the additive model in Figure \@ref(fig:DDOptimal). 
# 
# ```{r DDOptimal, fig.cap= '**Association between past dengue count over optimal lags within 1-30 months and the dengue outbreak.**. Solid lines represent relative risks (RR) of dengue cases and dottted lines depict the upper and lower limits of 95% confidence intervals.', fig.align='center', echo=FALSE}
# png("Fit-OptimalLag.png",width=1600, height=1300, res=300)

par(mfrow=c(1,3))
plot.gam(mod.DDOptimal, ylab="log(RR)")

# dev.off()



# ```{r DDOptimalPred, echo=FALSE, fig.cap="Monthly Observed and predicted dengue cases (2008-2012). "}

model <- mod.DDOptimal ## name of the model 

par(mfrow=c(1,1))
p <- fitted.values(model)
a <- predict(model, type="response")
a <- matrix(a, nrow = 60, byrow = FALSE)
a <- as.data.frame(a)
aSum <- rowSums(a ,na.rm = TRUE)
is.na(aSum) <- !aSum

png("Pred-OptimalLag.png",width=1600, height=1300, res=300)

plot(1:60, dSum, type="l",ylab="Dengue Cases",axes=T,xlab="Months")
points(aSum,type="l", col="red")
legend('top', c("Observed", "Predicted"),lty=1, col=c('black', 'red'), bty='n', cex=.75)

title(main="(C) Optimal Lag Surveillance Model")

dev.off()


d<-matrix(p, 60, 50)
d<- as.vector(apply(d, 2 , function(x) { c(rep(NA,23), x[24:60]) }))
temp<- cbind(trainTill2012_df$count,p, d)
#View(temp)

RMSE_DDOptimal  <-  sqrt(mean((trainTill2012_df$count-d)^2,na.rm=T))
SRMSE_DDOptimal <-  sqrt(mean((trainTill2012_df$count-d)^2,na.rm=T))/sqrt(mean((trainTill2012_df$count)^2,na.rm=T))


# RMSE_DDOptimal  <-  sqrt(mean((trainTill2012_df$count-p)^2,na.rm=T))
# SRMSE_DDOptimal <-  sqrt(mean((trainTill2012_df$count-p)^2,na.rm=T))/sqrt(mean((trainTill2012_df$count)^2,na.rm=T))
r.sq_DDOptimal <-   summary(model)$r.sq
dev.expl_DDOptimal <-  summary(model)$dev.expl

summary_DDOptimal <- data.frame("Optimal Lag Surveillance Model", RMSE_DDOptimal, SRMSE_DDOptimal, r.sq_DDOptimal, dev.expl_DDOptimal)
names(summary_DDOptimal) <- c("Model Name", "RMSE", "SRMSE", "R-sq.(adj)", "Deviance Explained")



# ```{r DDOptimalTable, echo=FALSE, comment=NA}
# knitr::kable(summary_DDOptimal, booktabs = TRUE,
# caption = 'Predictive Performance Statistics of Optimal-term Lag Model.'
# )
# ```


### Meteorology and Optimal-term Lag Model.


# The summary of the model is shown in Appendix \@ref(appDMD). Let's visualize the additive model in Figure \@ref(fig:DMD). 
# 
# ```{r DMD, fig.cap= '**Association between the meteorological variables, past dengue count over optimal lags within 1-30 months and the dengue outbreak.**. Solid lines represent relative risks (RR) of dengue cases and dottted lines depict the upper and lower limits of 95% confidence intervals.', fig.align='center', fig.width = 10, fig.height=10, echo=FALSE}
# png("Fit-Met-OptimalLag.png",width=1280, height=760, res=300)

par(mfrow=c(3,4))
plot.gam(mod.DMDgt1, ylab="log(RR)")

# dev.off()


# ```{r DMDPred, echo=FALSE, fig.cap="Monthly Observed and predicted dengue cases (2008-2012). "}

model <- mod.DMDgt1 ## name of the model 

par(mfrow=c(1,1))
p <- fitted.values(model)
a <- predict(model, type="response")
a <- matrix(a, nrow = 60, byrow = FALSE)
a <- as.data.frame(a)
aSum <- rowSums(a ,na.rm = TRUE)
is.na(aSum) <- !aSum

png("Pred-Met-OptimalLag.png",width=1600, height=1300, res=300)

plot(1:60, dSum, type="l",ylab="Dengue Cases",axes=T,xlab="Months")
points(aSum,type="l", col="red")
legend('top', c("Observed", "Predicted"),lty=1, col=c('black', 'red'), bty='n', cex=.75)
title(main="(D) Optimal Met and Lag Surveillance Model")

dev.off()


d<-matrix(p, 60, 50)
d<- as.vector(apply(d, 2 , function(x) { c(rep(NA,23), x[24:60]) }))
temp<- cbind(trainTill2012_df$count,p, d)
#View(temp)

RMSE_DMD  <-  sqrt(mean((trainTill2012_df$count-d)^2,na.rm=T))
SRMSE_DMD <-  sqrt(mean((trainTill2012_df$count-d)^2,na.rm=T))/sqrt(mean((trainTill2012_df$count)^2,na.rm=T))


# RMSE_DMD  <-  sqrt(mean((trainTill2012_df$count-p)^2,na.rm=T))
# SRMSE_DMD <-  sqrt(mean((trainTill2012_df$count-p)^2,na.rm=T))/sqrt(mean((trainTill2012_df$count)^2,na.rm=T))
r.sq_DMD <-   summary(model)$r.sq
dev.expl_DMD <-  summary(model)$dev.expl

summary_DMD <- data.frame("Optimal Met and Lag Surveillance Model", RMSE_DMD, SRMSE_DMD, r.sq_DMD, dev.expl_DMD)
names(summary_DMD) <- c("Model Name", "RMSE", "SRMSE", "R-sq.(adj)", "Deviance Explained")
summary_DMD 


# 
# ```{r DMDTable, echo=FALSE, comment=NA}
# knitr::kable(summary_DMD, booktabs = TRUE,
#              caption = 'Predictive Performance Statistics of Meteorology and Optimal-term Lag Model.'
# )
# ```
# 
# 
### Surrounding Dengue Data 
# 
# ```{r AR-Surrounding, echo=FALSE, results="hide"}

totalSDHF <- aggregate(surroundingCountVec0 ~ date_sick_year + date_sick_month, trainTill2012_df ,sum)
totalSDHF <- totalSDHF[order(totalSDHF$date_sick_year), ]

## Taking a one lag 



library(dlnm)

cb1S.temp <- crossbasis(totalSDHF$surroundingCountVec0, lag=30, argvar=list(df=4), arglag=list(fun="poly",degree=4))
summary(cb1S.temp)
mod.dlnm <- glm(totalDHF$count ~  cb1S.temp , family=quasipoisson())
pred1.temp <- crosspred(cb1S.temp, mod.dlnm)



# I show the simulated lagâ€“response surfaces for **surrounding districts** as relative risk in Figure \@ref(fig:lag-response-surround). 
# 
# ```{r lag-response-surround, echo=FALSE, results="hide", fig.cap= 'This shows the relation between the case intensity and dengue incidences in surrounding districts at the lag months', fig.align='center',}
# 

# png("Contour-SurroundingLongLag.png",width=1600, height=1300, res=300)

plot(pred1.temp, "contour", key.title=title("RR"), 
     plot.title=title("Influence of Long-term Lagged Data of Surrounding Districts",xlab="Dengue Counts",ylab="Lag"))

# dev.off()


# plot(pred1.temp, "slices", var=2200, col=1, ylab="RR",
#      ci.arg=list(density=15,lwd=2.5), ylim = c(0,100),
#      main="Lag-response curve for differing number of dengue cases")
# 
# 
# for(i in 1:3) lines(pred1.temp, "slices", var=c(900,1000,1200)[i], col=i+1, lwd=1.5)
# legend("topright",paste("Dengue Units =",c(1100,900,1000,1200)), col=1:4, lwd=1.5)






### Meteorology, Optimal-term and Short-term Surrounding Lag Model

# The summary of the model is shown in Appendix \@ref(appDMDS_Short). Let's visualize the additive model in Figure \@ref(fig:DMDS_Short). 
# 
# ```{r DMDS_Short, fig.cap= '**Association between the meteorological variables, past dengue count over optimal lags within 1-30 months, surroinding district count over 0-3 months and the dengue outbreak.**. Solid lines represent relative risks (RR) of dengue cases and dottted lines depict the upper and lower limits of 95% confidence intervals.', fig.align='center', fig.width = 10, fig.height=10, echo=FALSE}

par(mfrow=c(4,4))
plot.gam(mod.DMDS_Short, ylab="log(RR)")




# ```{r DMDSShort_Pred, echo=FALSE, fig.cap="Monthly Observed and predicted dengue cases (2008-2012). "}

model <- mod.DMDS_Short ## name of the model 

par(mfrow=c(1,1))
p <- fitted.values(model)
a <- predict(model, type="response")
a <- matrix(a, nrow = 60, byrow = FALSE)
a <- as.data.frame(a)
aSum <- rowSums(a ,na.rm = TRUE)
is.na(aSum) <- !aSum


#png("Pred-OptimalRepresentation.png", width=1600, height=1300, res=120)

plot(1:60, dSum, type="l",ylab="Dengue Cases",axes=T,xlab="Months")
points(aSum,type="l", col="red")
legend('top', c("Observed", "Predicted"),lty=1, col=c('black', 'red'), bty='n', cex=.75)

#title(main="Optimal Representation Model")

#dev.off()

RMSE_DMDS_Short <-  sqrt(mean((trainTill2012_df$count-p)^2,na.rm=T))
SRMSE_DMDS_Short <-  sqrt(mean((trainTill2012_df$count-p)^2,na.rm=T))/sqrt(mean((trainTill2012_df$count)^2,na.rm=T))
r.sq_DMDS_Short <-   summary(model)$r.sq
dev.expl_DMDS_Short <-  summary(model)$dev.expl

summary_DMDS_Short <- data.frame("Meteorology, Optimal(D) Short(D-S) Lag Model", RMSE_DMDS_Short, SRMSE_DMDS_Short, r.sq_DMDS_Short, dev.expl_DMDS_Short)
names(summary_DMDS_Short) <- c("Model Name", "RMSE", "SRMSE", "R-sq.(adj)", "Deviance Explained")


# 
# 
# ```{r DMDShortTable, echo=FALSE, comment=NA}
# knitr::kable(summary_DMDS_Short, booktabs = TRUE,
# caption = 'Predictive Performance Statistics of Meteorology and Optimal-term Lag Model.'
# )
# ```


### Meteorology, Optimal-term and Optimal-term Surrounding Lag Model

# The summary of the model is shown in Appendix \@ref(appDMDS_Optimal). Let's visualize the additive model in Figure \@ref(fig:DMDS_Optimal). 
# 
# ```{r DMDS_Optimal, fig.cap= '**Association between the meteorological variables, past dengue count over optimal lags within 1-30 months, surrounding district count over 0-30 months and the dengue outbreak.**. Solid lines represent relative risks (RR) of dengue cases and dottted lines depict the upper and lower limits of 95% confidence intervals.', fig.align='center', fig.width = 10, fig.height=10, echo=FALSE}


#png("Fit-OptimalRepresentation.png",width=1200, height=800, res=300)
#png("Fit-OptimalRepresentation.png", res=300)
#par(mar=c(5,1,2,2))
#par(mfrow=c(4,4))
#plot.gam(mod.DMDS_Optimal, ylab="log(RR)", cex.lab=1.5)

par(mar=c(5,1,2,2))
par(mfrow=c(2,3))
#png("Fit-OptimalRepresentation.png",width=1200, height=800, res=300)
#par(mfrow=c(2,3))
# 
# plot.gam(mod.DMDS_Optimalgt1, select = 1, xlab = "temp1",  ylab="log(RR)", cex.lab=2.0)
# plot.gam(mod.DMDS_Optimalgt1, select = 2, xlab = "temp2",  ylab="log(RR)", cex.lab=2.0)
# plot.gam(mod.DMDS_Optimalgt1, select = 3, xlab = "temp3",  ylab="log(RR)", cex.lab=2.0)
# plot.gam(mod.DMDS_Optimalgt1, select = 4, xlab = "rain1",  ylab="log(RR)", cex.lab=2.0)
# plot.gam(mod.DMDS_Optimalgt1, select = 5, xlab = "rain2",  ylab="log(RR)", cex.lab=2.0)
# plot.gam(mod.DMDS_Optimalgt1, select = 6, xlab = "rain3",  ylab="log(RR)", cex.lab=2.0)


plot.gam(mod.DMDS_Optimalgt1, select = 7, xlab = "count1",  ylab="log(RR)", cex.lab=2.0)
plot.gam(mod.DMDS_Optimalgt1, select = 8, xlab = "count2",  ylab="log(RR)", cex.lab=2.0)
plot.gam(mod.DMDS_Optimalgt1, select = 9, xlab = "count23",  ylab="log(RR)", cex.lab=2.0)
plot.gam(mod.DMDS_Optimalgt1, select = 10, xlab = "surrounding1",  ylab="log(RR)", cex.lab=2.0)
plot.gam(mod.DMDS_Optimalgt1, select = 11, xlab = "surrounding2",  ylab="log(RR)", cex.lab=2.0)
plot.gam(mod.DMDS_Optimalgt1, select = 12, xlab = "surrounding12",  ylab="log(RR)", cex.lab=2.0)

#dev.off()




# ```{r DMDSOptimalPred, echo=FALSE, fig.cap="Monthly Observed and predicted dengue cases (2008-2012). "}

model <- mod.DMDS_Optimalgt1 ## name of the model 

par(mfrow=c(1,1))
p <- fitted.values(model)
a <- predict(model, type="response")
a <- matrix(a, nrow = 60, byrow = FALSE)
a <- as.data.frame(a)
aSum <- rowSums(a ,na.rm = TRUE)
is.na(aSum) <- !aSum


png("Pred-OptimalRepresentation.png",width=1600, height=1300, res=300)

plot(1:60, dSum, type="l",ylab="Dengue Cases",axes=T,xlab="Months")
points(aSum,type="l", col="red")
legend('top', c("Observed", "Predicted"),lty=1, col=c('black', 'red'), bty='n', cex=.75)

title(main="(E) Optimal Representation Model")

dev.off()


d<-matrix(p, 60, 50)
d<- as.vector(apply(d, 2 , function(x) { c(rep(NA,23), x[24:60]) }))
temp<- cbind(trainTill2012_df$count,p, d)
#View(temp)

RMSE_DMDS_Optimal  <-  sqrt(mean((trainTill2012_df$count-d)^2,na.rm=T))
SRMSE_DMDS_Optimal <-  sqrt(mean((trainTill2012_df$count-d)^2,na.rm=T))/sqrt(mean((trainTill2012_df$count)^2,na.rm=T))


# RMSE_DMDS_Optimal <-  sqrt(mean((trainTill2012_df$count-p)^2,na.rm=T))
# SRMSE_DMDS_Optimal <-  sqrt(mean((trainTill2012_df$count-p)^2,na.rm=T))/sqrt(mean((trainTill2012_df$count)^2,na.rm=T))
r.sq_DMDS_Optimal <-   summary(model)$r.sq
dev.expl_DMDS_Optimal <-  summary(model)$dev.expl

summary_DMDS_Optimal <- data.frame("Optimal Representation Model", RMSE_DMDS_Optimal, SRMSE_DMDS_Optimal, r.sq_DMDS_Optimal, dev.expl_DMDS_Optimal)
names(summary_DMDS_Optimal) <- c("Model Name", "RMSE", "SRMSE", "R-sq.(adj)", "Deviance Explained")
summary_DMDS_Optimal 


# ```{r DMDSOptiTable, echo=FALSE, comment=NA}
# knitr::kable(summary_DMDS_Optimal, booktabs = TRUE,
#              caption = 'Predictive Performance Statistics of Meteorology and Optimal-term Lag Model.'
# )
# ```


## Social-Economic Data 


# The summary of the model is shown in Appendix \@ref(appDMDSO). Let's visualize the additive model in Figure \@ref(fig:DMDSO). 
# 
# ```{r DMDSO, fig.cap= '**Association between the meteorological variables, past dengue count over optimal lags within 1-30 months, surrounding district count over 0-30 months, garbage data and the dengue outbreak.**. Solid lines represent relative risks (RR) of dengue cases and dottted lines depict the upper and lower limits of 95% confidence intervals.', fig.width = 10, fig.height=10, fig.align='center', echo=FALSE}

#png("Fit-SocialIncluded.png",width=1280, height=810, res=300)
# par(mfrow=c(4,4))
# plot.gam(mod.DMDSO, ylab="log(RR)")
#dev.off()

#png("Fit-OptimalRepresentation+Garbage.png",width=1200, height=800, res=300)
#par(mfrow=c(2,3))
# 
# plot.gam(mod.DMDS_Optimalgt1, select = 1, xlab = "temp1",  ylab="log(RR)", cex.lab=2.0)
# plot.gam(mod.DMDS_Optimalgt1, select = 2, xlab = "temp2",  ylab="log(RR)", cex.lab=2.0)
# plot.gam(mod.DMDS_Optimalgt1, select = 3, xlab = "temp3",  ylab="log(RR)", cex.lab=2.0)
# plot.gam(mod.DMDS_Optimalgt1, select = 4, xlab = "rain1",  ylab="log(RR)", cex.lab=2.0)
# plot.gam(mod.DMDS_Optimalgt1, select = 5, xlab = "rain2",  ylab="log(RR)", cex.lab=2.0)
# plot.gam(mod.DMDS_Optimalgt1, select = 6, xlab = "rain3",  ylab="log(RR)", cex.lab=2.0)

par(mar=c(5,1,2,2))
par(mfrow=c(2,4))
plot.gam(mod.DMDSO, select = 7, xlab = "count1",  ylab="log(RR)", cex.lab=2.0)
plot.gam(mod.DMDSO, select = 8, xlab = "count2",  ylab="log(RR)", cex.lab=2.0)
plot.gam(mod.DMDSO, select = 9, xlab = "count23",  ylab="log(RR)", cex.lab=2.0)
plot.gam(mod.DMDSO, select = 10, xlab = "surrounding1",  ylab="log(RR)", cex.lab=2.0)
plot.gam(mod.DMDSO, select = 11, xlab = "surrounding2",  ylab="log(RR)", cex.lab=2.0)
plot.gam(mod.DMDSO, select = 12, xlab = "surrounding12",  ylab="log(RR)", cex.lab=2.0)
plot.gam(mod.DMDSO, select = 13, xlab = "garbage",  ylab="log(RR)", cex.lab=2.0)

#dev.off()



# ```
# 
# ```{r DMDSShortPred, echo=FALSE, fig.cap="Monthly Observed and predicted dengue cases (2008-2012). "}

model <- mod.DMDSO ## name of the model 

par(mfrow=c(1,1))
p <- fitted.values(model)
a <- predict(model, type="response")
a <- matrix(a, nrow = 60, byrow = FALSE)
a <- as.data.frame(a)
aSum <- rowSums(a ,na.rm = TRUE)
is.na(aSum) <- !aSum



png("Pred-SocialIncluded.png",width=1600, height=1300, res=300)

plot(1:60, dSum, type="l",ylab="Dengue Cases",axes=T,xlab="Months")
points(aSum,type="l", col="red")
legend('top', c("Observed", "Predicted"),lty=1, col=c('black', 'red'), bty='n', cex=.75)

title(main="(F) Social-economic data Included")

dev.off()



d<-matrix(p, 60, 50)
d<- as.vector(apply(d, 2 , function(x) { c(rep(NA,23), x[24:60]) }))
temp<- cbind(trainTill2012_df$count,p, d)
#View(temp)

RMSE_DMDSO  <-  sqrt(mean((trainTill2012_df$count-d)^2,na.rm=T))
SRMSE_DMDSO <-  sqrt(mean((trainTill2012_df$count-d)^2,na.rm=T))/sqrt(mean((trainTill2012_df$count)^2,na.rm=T))





# 
# RMSE_DMDSO<-  sqrt(mean((trainTill2012_df$count-p)^2,na.rm=T))
# SRMSE_DMDSO <-  sqrt(mean((trainTill2012_df$count-p)^2,na.rm=T))/sqrt(mean((trainTill2012_df$count)^2,na.rm=T))


# # This code was written for testing
# d<-matrix(p, 60, 50)
# d<- as.vector(apply(d, 2 , function(x) { c(rep(NA,23), x[24:60]) }))
# temp<- cbind(trainTill2012_df$count,p, d)
# #View(d)

r.sq_DMDSO <-   summary(model)$r.sq
dev.expl_DMDSO <-  summary(model)$dev.expl

summary_DMDSO<- data.frame("Social-economic data Included", RMSE_DMDSO, SRMSE_DMDSO, r.sq_DMDSO, dev.expl_DMDSO)
names(summary_DMDSO) <- c("Model Name", "RMSE", "SRMSE", "R-sq.(adj)", "Deviance Explained")
summary_DMDSO



# ```{r DMDSOTable, echo=FALSE, comment=NA}
# knitr::kable(summary_DMDSO, booktabs = TRUE,
# caption = 'Predictive Performance Statistics of Social-economic data Included.'
# )
# ```



## Predictive Performance Statistics 

# On the **training dataset**.
# 
# ```{r TotalPerformance, echo=FALSE, comment=NA}

totalPerformance <- rbind(summary_MN, summary_DM, summary_DDOptimal, summary_DMD,  summary_DMDS_Optimal, summary_DMDSO)
totalPerformance


# knitr::kable(totalPerformance, booktabs = TRUE,
# caption = 'Predictive Performance Statistics of All Models'
# )
# ```



### Making AIC 

dfun <- function(object) {
  with(object,sum((weights * residuals^2)[weights > 0])/df.residual)
}




### For poisson

#Seasonal Naive: 

mod.MN.po <- gam(count ~ s(countData_mn,k=4), family=poisson, na.action=na.exclude, data = trainTill2012_df)

# meteorology only model the formula for lag>1 

mod.DMgt1.po <- gam(count ~ s(templ1, k=4) + s(templ2,k=4) + s(templ3,k=4) + s(rainl1,k=4) + s(rainl2,k=4) + s(rainl3,k=4) , family= poisson, na.action=na.exclude, data = trainTill2012_df)

## Short-term incidences
#mod.DDShort.po <- gam(count ~ s(count1,k=4) + s(count2,k=4) + s(count3,k=4) + s(count4,k=4), family=poisson, na.action=na.exclude, data = trainTill2012_df)

## Optimal lags (short and long included)
mod.DDOptimal.po <- gam(count ~ s(count1,k=4) + s(count2,k=4) + s(count23,k=4), family=poisson, na.action=na.exclude, data = trainTill2012_df)

## meteorology and past dengue incidences model - all variables lag > 1
mod.DMDgt1.po <- gam(count ~ s(templ1, k=4) + s(templ2,k=4) + s(templ3,k=4) +s(rainl1,k=4) + s(rainl2,k=4) + s(rainl3,k=4) + s(count1,k=4) + s(count8,k=4) + s(count18,k=4) + s(count23,k=4) , family=poisson, na.action=na.exclude, data = trainTill2012_df)

### meteorology, past dengue incidences and surrounding dengue incidences model - all variables all lags

mod.DMDS_Optimalgt1.po <- gam(count ~ s(templ1, k=4) + s(templ2,k=4) + s(templ3,k=4) + s(rainl1,k=4) + s(rainl2,k=4) + s(rainl3,k=4) + s(count1,k=4) + s(count2,k=4) + s(count23,k=4) +  s(surroundingCountVec1,k=4) + s(surroundingCountVec2,k=4) +  s(surroundingCountVec12,k=4), family=poisson, na.action=na.exclude, data = trainTill2012_df)

###  Garbage offset along with meteorology, past dengue incidences and surrounding dengue incidences model - all variables all lags

mod.DMDSO.po <- gam( count ~  s(templ1, k=4) + s(templ2,k=4) + s(templ3,k=4)  + s(rainl1,k=4) + s(rainl2,k=4) + s(rainl3,k=4) + s(count1,k=4) + s(count2,k=4) + s(count23,k=4) +  s(surroundingCountVec1,k=4) + s(surroundingCountVec2,k=4) +  s(surroundingCountVec12,k=4) + offset(log(garbage)) + s(garbage) , family=poisson, na.action=na.exclude, data = trainTill2012_df)


### calculate AIC

library(bbmle)
# 


qp_base <- (qAIC(mod.MN.po,dispersion=dfun(mod.MN.po)))
delta_qp.dm <- (qAIC(mod.DMgt1.po,dispersion=dfun(mod.MN.po))) -qp_base
delta_qp.dd   <-  (qAIC(mod.DDOptimal.po,dispersion=dfun(mod.MN.po))) -qp_base
delta_qp.dmd <-   (qAIC(mod.DMDgt1.po,dispersion=dfun(mod.MN.po))) - qp_base
delta_qp.dmds <- (qAIC(mod.DMDS_Optimalgt1.po,dispersion=dfun(mod.MN.po))) -qp_base
delta_qp.dmdso <- (qAIC(mod.DMDSO.po,dispersion=dfun(mod.MN.po))) - qp_base

# qp_base <- (qAIC(mod.DMgt1.po,dispersion=dfun(mod.DMgt1.po)))
# delta_qp.dd   <-  (qAIC(mod.DDOptimal.po,dispersion=dfun(mod.DDOptimal.po))) -qp_base
# delta_qp.dmd <-   (qAIC(mod.DMDgt1.po,dispersion=dfun(mod.DMDgt1.po))) - qp_base
# delta_qp.dmds <- (qAIC(mod.DMDS_Optimalgt1.po,dispersion=dfun(mod.DMDS_Optimalgt1.po))) -qp_base
# delta_qp.dmdso <- (qAIC(mod.DMDSO.po,dispersion=dfun(mod.DMDSO.po))) - qp_base


rbind(0, delta_qp.dm, delta_qp.dd, delta_qp.dmd, delta_qp.dmds, delta_qp.dmdso)

# result <- ICtab(mod.DMDSO.po, mod.DMDS_Optimalgt1.po, mod.DMDgt1.po, mod.DDOptimal.po, dispersion=dfun(mod.DMgt1.po),type="qAIC")
# result


## Evaluation 


# <!-- WholeDataLagged -->
# 
# <!-- Formula: -->
# <!-- count ~ s(templ0, k = 4) + s(templ1, k = 4) + s(templ2, k = 4) +  -->
# <!--     s(templ3, k = 4) + s(rainl0, k = 4) + s(rainl1, k = 4) +  -->
# <!--     s(rainl2, k = 4) + s(rainl3, k = 4) + s(count1, k = 4) +  -->
# <!--     s(count8, k = 4) + s(count18, k = 4) + s(count23, k = 4) +  -->
# <!--     s(count25, k = 4) + s(surroundingCountVec0, k = 4) + s(surroundingCountVec13,  -->
# <!--     k = 4) + s(surroundingCountVec14, k = 4) + s(surroundingCountVec15,  -->
# <!--     k = 4) -->



# ```{r validation, echo=FALSE}

form <- as.formula("count ~  s(templ1, k=4) + s(templ2,k=4) + s(templ3,k=4) + s(rainl1,k=4) + s(rainl2,k=4) + s(rainl3,k=4) + s(count1,k=4) + s(count2,k=4) + s(count23,k=4) + s(surroundingCountVec1,k=4) + s(surroundingCountVec2,k=4) +  s(surroundingCountVec12,k=4) + offset(log(garbage)) + s(garbage)")

preddata <- subset(WholeDataLagged, select = c(templ1, templ2, templ3, rainl1, rainl2, rainl3, count1, count2,  count23, surroundingCountVec1, surroundingCountVec2, surroundingCountVec12, garbage))


train1 <- subset(WholeDataLagged,date_sick_year < 2013)
train2 <- subset(WholeDataLagged,date_sick_year < 2014)
train3 <- subset(WholeDataLagged,date_sick_year < 2015)

## for Train 1
mod.DMDS_1 <- gam(form, family=quasipoisson, na.action=na.exclude, data = train1)
mod.DMDS_2 <- gam(form, family=quasipoisson, na.action=na.exclude, data = train2)
mod.DMDS_3 <- gam(form, family=quasipoisson, na.action=na.exclude, data = train3)

WholeDataLagged$predict1 <-predict(mod.DMDS_1, type="response", newdata=preddata)
WholeDataLagged$predict2 <-predict(mod.DMDS_2, type="response", newdata=preddata)
WholeDataLagged$predict3 <-predict(mod.DMDS_3, type="response", newdata=preddata)

train1 <- subset(WholeDataLagged,date_sick_year < 2013)
train2 <- subset(WholeDataLagged,date_sick_year < 2014)
train3 <- subset(WholeDataLagged,date_sick_year < 2015)

pred1 <- subset(WholeDataLagged,date_sick_year> 2012) 
pred2 <- subset(WholeDataLagged,date_sick_year> 2013)
pred3 <- subset(WholeDataLagged,date_sick_year> 2014)
pred4 <- subset(WholeDataLagged,date_sick_year> 2012 & date_sick_year < 2014)
pred5 <- subset(WholeDataLagged,date_sick_year> 2013 & date_sick_year < 2015)


p1 <- WholeDataLagged$predict1
a <- matrix(p1, nrow = 96, byrow = FALSE)
a <- as.data.frame(a)
aSum_1 <- rowSums(a ,na.rm = TRUE)
is.na(aSum_1) <- !aSum_1

d <- matrix(WholeDataLagged$count, nrow = 96, byrow = FALSE)
d <- as.data.frame(d)
dSum <- rowSums(d,na.rm = TRUE)


png("Validate-2008-2012.png",width=1600, height=1300, res=300)

par(mfrow=c(1,1))
plot(1:96, dSum, type="l", ylab="Dengue Cases",axes=T,xlab="Months")
points(aSum_1 [1:60],type="l", col="red")
points(61:96, aSum_1[61:96],type="l", col="blue")

abline(h=790, col = "gray60")
legend("topleft", c("Observed", "Predicted on Training Set", "Predicted on Validation Set"),lty=1, col=c('black', 'red', "blue"), bty='n', cex=.75)

dev.off()


#title("Training Dataset (2008-2012) was used.")


#for training data

aa <- "2008-2012"

a <-sqrt(mean((train1$count-train1$predict1)^2,na.rm=T))/sqrt(mean((train1$count)^2,na.rm=T))

#for validation data 2013-2015
b<-sqrt(mean((pred1$count-pred1$predict1)^2,na.rm=T))/sqrt(mean((pred1$count)^2,na.rm=T))

#for validation data 2014-2015
c<-sqrt(mean((pred2$count-pred2$predict1)^2,na.rm=T))/sqrt(mean((pred2$count)^2,na.rm=T))

#for validation data Only 2013
d<-sqrt(mean((pred4$count-pred4$predict1)^2,na.rm=T))/sqrt(mean((pred4$count)^2,na.rm=T))

#for validation data Only 2015
dd<-sqrt(mean((pred5$count-pred5$predict1)^2,na.rm=T))/sqrt(mean((pred5$count)^2,na.rm=T))

#for validation data Only 2015
e<-sqrt(mean((pred3$count-pred3$predict1)^2,na.rm=T))/sqrt(mean((pred3$count)^2,na.rm=T))


pred_accuracy1 <- data.frame(aa, a, b, c, d,dd, e)
names(pred_accuracy1) <- c("Training Dataset", "In-sample Error", "Out-Sample (2013-2015)", "Out-Sample (2014-2015)", "Out-Sample (2013)", "Out-Sample (2014)", "Out-Sample (2015)" )


## for train 2



p2 <- WholeDataLagged$predict2
a <- matrix(p2, nrow = 96, byrow = FALSE)
a <- as.data.frame(a)
aSum_2 <- rowSums(a ,na.rm = TRUE)
is.na(aSum_2) <- !aSum_2




png("Validate-2008-2013.png",width=1600, height=1300, res=300)

par(mfrow=c(1,1))
plot(1:96, dSum, type="l", ylab="Dengue Cases",axes=T,xlab="Months")
points(aSum_2[1:72],type="l", col="red")
points(73:96, aSum_2[73:96],type="l", col="blue")
legend('topleft', c("Observed", "Predicted on Training Set", "Predicted on Validation Set"),lty=1, col=c('black', 'red', "blue"), bty='n', cex=.75)

abline(h=790, col = "gray60")
#title("Training Dataset (2008-2013) was used.")

dev.off()


aa <- "2008-2013"


#for training data
a <- sqrt(mean((train2$count-train2$predict2)^2,na.rm=T))/sqrt(mean((train2$count)^2,na.rm=T))

#for validation data 2014-2015
c <- sqrt(mean((pred2$count-pred2$predict2)^2,na.rm=T))/sqrt(mean((pred2$count)^2,na.rm=T))

#for validation data Only 2015
e <- sqrt(mean((pred3$count-pred3$predict2)^2,na.rm=T))/sqrt(mean((pred3$count)^2,na.rm=T))

#for validation data Only 2014
dd <- sqrt(mean((pred4$count-pred4$predict2)^2,na.rm=T))/sqrt(mean((pred4$count)^2,na.rm=T))


pred_accuracy2 <- data.frame(aa, a, b = NA, c, d = NA,dd, e)
names(pred_accuracy2) <- c("Training Dataset", "In-sample Error", "Out-Sample (2013-2015)", "Out-Sample (2014-2015)", "Out-Sample (2013)", "Out-Sample (2014)", "Out-Sample (2015)" )


## for train 3

p3 <- WholeDataLagged$predict3
a <- matrix(p3, nrow = 96, byrow = FALSE)
a <- as.data.frame(a)
aSum_3 <- rowSums(a ,na.rm = TRUE)
is.na(aSum_3) <- !aSum_3



png("Validate-2008-2014.png",width=1600, height=1300, res=300)

par(mfrow=c(1,1))
plot(1:96, dSum, type="l", ylab="Dengue Cases",axes=T,xlab="Months")
points(aSum_3[1:84],type="l", col="red")
points(85:96, aSum_3[85:96],type="l", col="blue")
abline(h=790, col = "gray60")
legend('topleft', c("Observed", "Predicted on Training Set", "Predicted on Validation Set"),lty=1, col=c('black', 'red', "blue"), bty='n', cex=.75)

abline(h=790, col = "gray60")
#title("Training Dataset (2008-2014) was used.")
dev.off()

aa <- "2008-2014"

#for training data
a <- sqrt(mean((train3$count-train3$predict3)^2,na.rm=T))/sqrt(mean((train3$count)^2,na.rm=T))

#for validation data Only 2015
e <-  sqrt(mean((pred3$count-pred3$predict3)^2,na.rm=T))/sqrt(mean((pred3$count)^2,na.rm=T))

pred_accuracy3 <- data.frame(aa, a , b = NA, c = NA, d = NA,dd = NA, e)
names(pred_accuracy3) <- c("Training Dataset", "In-sample Error", "Out-Sample (2013-2015)", "Out-Sample (2014-2015)", "Out-Sample (2013)", "Out-Sample (2014)", "Out-Sample (2015)" )






# ```{r totalAccuracy, echo=FALSE}

all <- rbind(pred_accuracy1, pred_accuracy2, pred_accuracy3)
# knitr::kable(all, booktabs = TRUE,
# caption = 'Predictive Performance Statistics measured using SRMSE'
# )

all



## Residual Diagnosis
# 
# ```{r residual_diganosis, include=FALSE, echo=FALSE, results="hide"}

## The final model is assigned
mod.fin <- mod.DMDS_3 

## Then residual daignosis is performed

resid <- residuals(mod.fin)


# png("resid-diagnosis.png", width=1600, height=1300, res=300)
par(mfrow=c(2,2))
hist(resid, xlab="Residuals", main=" ")
pacf(resid, na.action = na.pass, main=" ")      
#plot.ts(resid, xlab="Month", ylab="Residuals") 
qq.gam(mod.fin, main="")
plot(train3$count, fitted(mod.fin), ylab="Predicted Cases", xlab="Reported Cases", main=" ")
# dev.off()

Box.test(resid)
shapiro.test(resid)


# ```
# 
# ## Senstivity Analysis
# 
# ```{r sensity, echo=FALSE, results="hide", include=FALSE}



# par(mfrow=c(1,1))
# p <- fitted.values(mod.fin)
# plot(1:96, train3$count, type="l")
# points(p,type="l", col="red")
# abline(h=60)
# points(tr,type="p",col="grey")







#title("Training Dataset (2008-2014) was used.")

bind <- train3$count>790 #dengue is 120 length vector, 60 is threshold.
tr <-rep(790,times=96)

value <- aSum_3>790
dengueCount <- dSum>790
xtabs(value~dengueCount)
table(value,dengueCount )
prop.table(table(value,dengueCount))

valueF <- as.factor(value)
dengueCountF <- as.factor(dengueCount)

library(caret)

specificity(valueF, dengueCountF)
sensitivity(valueF, dengueCountF) 
posPredValue(valueF, dengueCountF)
negPredValue(valueF, dengueCountF)  


#title(main="Specificity=92.60, Sensitivity=87.0, PPV=95.72, NPV=78.80")


## for the WHO thresold, it is moving threshold.

who_threshold <- runmean(aSum_3, 60) +  2 * runsd(aSum_3,60)
valueWho <- aSum_3[61:96]   >  who_threshold[61:96] #predicted
dengueCount <- dSum[61:96]  > who_threshold[61:96] #actual
xtabs(valueWho~dengueCount)
table(valueWho,dengueCount )
prop.table(table(valueWho,dengueCount))

valueF <- as.factor(valueWho) #predicted
dengueCountF <- as.factor(dengueCount) #actual


specificity(valueF, dengueCountF) 
sensitivity(valueF, dengueCountF)
posPredValue(valueF, dengueCountF)
negPredValue(valueF, dengueCountF)

#title(main="Specificity=57.14, Sensitivity=100.0, PPV=90.62, NPV=100.0")

conf_matrix <-table(valueF,dengueCountF)
conf_matrix
sensitivity(conf_matrix)
specificity(conf_matrix)


## Plotting the results


png("Final-Prediction.png",width=1600, height=1300, res=300)

par(mfrow=c(1,1))
plot(1:96, dSum, type="l", ylab="Dengue Cases",axes=T,xlab="Months")
points(aSum_3[1:84],type="l", col="red")
points(85:96, aSum_3[85:96],type="l", col="blue")
abline(h=790, col = "gray60")
points(61:96, who_threshold[61:96],type="o", pch = 19, col="magenta", cex = 0.5 )
legend('topleft', c("Observed", "Predicted on Training Set", "Predicted on Validation Set", "Threshold", "Threshold (WHO)"), lty=1, col=c('black', 'red', "blue", "gray60", "magenta"),  pch = c(NA, NA, NA, NA, 19), bty='n', cex=.75)

dev.off()





# avg_5<-mean(dSum[1:60])
# SD_2years <- 2*sd(dSum[1:60])
# threshold <- avg_5 + SD_2years 
# 
# abline(h=threshold, col = "green")
# 
# valueWho <- aSum_3[61:96]>threshold
# dengueCount <- dSum[61:96]>threshold
# 
# xtabs(valueWho~dengueCount)
# table(valueWho,dengueCount )
# prop.table(table(valueWho,dengueCount))
# 
# valueF <- as.factor(valueWho)
# dengueCountF <- as.factor(dengueCount)
# 
# 
# specificity(valueF, dengueCountF)
# sensitivity(valueF, dengueCountF) 
# posPredValue(valueF, dengueCountF)
# negPredValue(valueF, dengueCountF)  

#title(main="Specificity=63.63, Sensitivity=96.0, PPV=85.7, NPV=87.5")

## Take that correlation



## Multiplot function taken from http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

x <- c(0,1,2,3)

cor0_d <- cor(WholeDataLagged$count0, WholeDataLagged$count0, method = "pearson", use = "complete.obs")
cor1_d <- cor(WholeDataLagged$count0, WholeDataLagged$count1, method = "pearson", use = "complete.obs")
cor2_d <- cor(WholeDataLagged$count0, WholeDataLagged$count2, method = "pearson", use = "complete.obs")
cor3_d <- cor(WholeDataLagged$count0, WholeDataLagged$count3, method = "pearson", use = "complete.obs")
cor_d <- c(cor0_d, cor1_d, cor2_d, cor3_d)
d_d <- data.frame(x, y = cor_d)
p1<-ggplot(d_d, aes(x, y)) + geom_point(shape = 16, size = 3, show.legend = FALSE) + theme_minimal() +  xlim(0, 3) +  ylim(-1, 1) + xlab("Lagged DHF incidence") + ylab("DHF incidence")

cor0_d <- cor(WholeDataLagged$count0, WholeDataLagged$templ0, method = "pearson", use = "complete.obs")
cor1_d <- cor(WholeDataLagged$count0, WholeDataLagged$templ1, method = "pearson", use = "complete.obs")
cor2_d <- cor(WholeDataLagged$count0, WholeDataLagged$templ2, method = "pearson", use = "complete.obs")
cor3_d <- cor(WholeDataLagged$count0, WholeDataLagged$templ3, method = "pearson", use = "complete.obs")
cor_t <- c(cor0_d, cor1_d, cor2_d, cor3_d)
d_t <- data.frame(x, y = cor_t)
p2<-ggplot(d_t, aes(x, y)) + geom_point(shape = 16, size = 3, show.legend = FALSE)  + theme_minimal() +  xlim(0, 3) +  ylim(-1, 1) + xlab("DTR") + ylab("DHF incidence")


cor0_d <- cor(WholeDataLagged$count0, WholeDataLagged$rainl0, method = "pearson", use = "complete.obs")
cor1_d <- cor(WholeDataLagged$count0, WholeDataLagged$rainl1, method = "pearson", use = "complete.obs")
cor2_d <- cor(WholeDataLagged$count0, WholeDataLagged$rainl2, method = "pearson", use = "complete.obs")
cor3_d <- cor(WholeDataLagged$count0, WholeDataLagged$rainl3, method = "pearson", use = "complete.obs")
cor_r  <- c(cor0_d, cor1_d, cor2_d, cor3_d)
d_r <- data.frame(x, y = cor_r)

p3<-ggplot(d_r, aes(x, y)) + geom_point(shape = 16, size = 3, show.legend = FALSE)  + theme_minimal() +  xlim(0, 3) +  ylim(-1, 1) + xlab("Rainfall") + ylab("DHF incidence")

cor0_d <- cor(WholeDataLagged$count0, WholeDataLagged$surroundingCountVec0, method = "pearson", use = "complete.obs")
cor1_d <- cor(WholeDataLagged$count0, WholeDataLagged$surroundingCountVec1, method = "pearson", use = "complete.obs")
cor2_d <- cor(WholeDataLagged$count0, WholeDataLagged$surroundingCountVec2, method = "pearson", use = "complete.obs")
cor3_d <- cor(WholeDataLagged$count0, WholeDataLagged$surroundingCountVec3, method = "pearson", use = "complete.obs")

cor_s  <- c(cor0_d, cor1_d, cor2_d, cor3_d)
d_s <- data.frame(x, y = cor_s)

p4<-ggplot(d_s, aes(x, y)) + geom_point(shape = 16, size = 3, show.legend = FALSE)  + theme_minimal() +  xlim(0, 3) +  ylim(-1, 1) + xlab("DHF Surrounding Districts") + ylab("DHF Target District")



#ggarrange(p1, p2, nrow = 1)

multiplot(p1, p2, p3, p4, cols=2)
