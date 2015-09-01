# To use 'melt'
library(reshape2)
# Grammar of Graphics Plotting Library
library(ggplot2)


# importing the data
WDI <- read.csv( "Data/WDI_Data.csv" , stringsAsFactors = FALSE )

NoOfProceduresToStartUp <- subset( WDI , WDI$Indicator.Code == 'IC.REG.PROC' )
NoOfProceduresToStartUp$Country.Name <- NULL
NoOfProceduresToStartUp$Indicator.Name <- NULL
NoOfProceduresToStartUp$Indicator.Code <- NULL
NoOfProceduresToStartUp <- melt( NoOfProceduresToStartUp , id.vars = "Country.Code" )
colnames( NoOfProceduresToStartUp )[2:3] <- c( "Year" , "NoOfProceduresToStartUp" )


EaseOfDoingBusinessIndex <- subset( WDI , WDI$Indicator.Code == 'IC.BUS.EASE.XQ' )
EaseOfDoingBusinessIndex$Country.Name <- NULL
EaseOfDoingBusinessIndex$Indicator.Name <- NULL
EaseOfDoingBusinessIndex$Indicator.Code <- NULL
EaseOfDoingBusinessIndex <- melt( EaseOfDoingBusinessIndex , id.vars = "Country.Code" )
colnames( EaseOfDoingBusinessIndex )[2:3] <- c( "Year" , "EaseOfDoingBusinessIndex" )


StrengthLegalRights <- subset( WDI , WDI$Indicator.Code == 'IC.LGL.CRED.XQ' )
StrengthLegalRights$Country.Name <- NULL
StrengthLegalRights$Indicator.Name <- NULL
StrengthLegalRights$Indicator.Code <- NULL
StrengthLegalRights <- melt( StrengthLegalRights , id.vars = "Country.Code" )
colnames( StrengthLegalRights )[2:3] <- c( "Year" , "StrengthLegalRights" )

TimeRequiredToGetElectricityDays <- subset( WDI , WDI$Indicator.Code == 'IC.ELC.TIME' )
TimeRequiredToGetElectricityDays$Country.Name <- NULL
TimeRequiredToGetElectricityDays$Indicator.Name <- NULL
TimeRequiredToGetElectricityDays$Indicator.Code <- NULL
TimeRequiredToGetElectricityDays <- melt( TimeRequiredToGetElectricityDays , id.vars = "Country.Code" )
colnames( TimeRequiredToGetElectricityDays )[2:3] <- c( "Year" , "TimeRequiredToGetElectricityDays" )


NumberOfProceduresToEnforceContract <- subset( WDI , WDI$Indicator.Code == 'IC.LGL.PROC' )
NumberOfProceduresToEnforceContract$Country.Name <- NULL
NumberOfProceduresToEnforceContract$Indicator.Name <- NULL
NumberOfProceduresToEnforceContract$Indicator.Code <- NULL
NumberOfProceduresToEnforceContract <- melt( NumberOfProceduresToEnforceContract , id.vars = "Country.Code" )
colnames( NumberOfProceduresToEnforceContract )[2:3] <- c( "Year" , "NumberOfProceduresToEnforceContract" )


TimeRequiredToStartBusinessInDays <- subset( WDI , WDI$Indicator.Code == 'IC.REG.DURS' )
TimeRequiredToStartBusinessInDays$Country.Name <- NULL
TimeRequiredToStartBusinessInDays$Indicator.Name <- NULL
TimeRequiredToStartBusinessInDays$Indicator.Code <- NULL
TimeRequiredToStartBusinessInDays <- melt( TimeRequiredToStartBusinessInDays , id.vars = "Country.Code" )
colnames( TimeRequiredToStartBusinessInDays )[2:3] <- c( "Year" , "TimeRequiredToStartBusinessInDays" )

TimeToPrepareAndPayTaxesHours <- subset( WDI , WDI$Indicator.Code == 'IC.TAX.DURS' )
TimeToPrepareAndPayTaxesHours$Country.Name <- NULL
TimeToPrepareAndPayTaxesHours$Indicator.Name <- NULL
TimeToPrepareAndPayTaxesHours$Indicator.Code <- NULL
TimeToPrepareAndPayTaxesHours <- melt( TimeToPrepareAndPayTaxesHours , id.vars = "Country.Code" )
colnames( TimeToPrepareAndPayTaxesHours )[2:3] <- c( "Year" , "TimeToPrepareAndPayTaxesHours" )

ProfitTax <- subset( WDI , WDI$Indicator.Code == 'IC.TAX.PRFT.CP.ZS' )
ProfitTax$Country.Name <- NULL
ProfitTax$Indicator.Name <- NULL
ProfitTax$Indicator.Code <- NULL
ProfitTax <- melt( ProfitTax , id.vars = "Country.Code" )
colnames( ProfitTax )[2:3] <- c( "Year" , "ProfitTax" )

TotalTaxRate <- subset( WDI , WDI$Indicator.Code == 'IC.TAX.TOTL.CP.ZS' )
TotalTaxRate$Country.Name <- NULL
TotalTaxRate$Indicator.Name <- NULL
TotalTaxRate$Indicator.Code <- NULL
TotalTaxRate <- melt( TotalTaxRate , id.vars = "Country.Code" )
colnames( TotalTaxRate )[2:3] <- c( "Year" , "TotalTaxRate" )





StartUpBusinessFactorsData <- 
  cbind( EaseOfDoingBusinessIndex           ,
         NoOfProceduresToStartUp    ,         NumberOfProceduresToEnforceContract,
         ProfitTax                   ,        StrengthLegalRights                ,
         TimeRequiredToGetElectricityDays ,   TimeRequiredToStartBusinessInDays  ,
         TimeToPrepareAndPayTaxesHours     ,  TotalTaxRate                       )



StartUpBusinessFactorsData <- StartUpBusinessFactorsData[,c(1,2,3,6,9,12,15,18,21,24,27)]

rm( EaseOfDoingBusinessIndex , NoOfProceduresToStartUp , NumberOfProceduresToEnforceContract , 
ProfitTax, StrengthLegalRights , TimeRequiredToGetElectricityDays , 
TimeRequiredToStartBusinessInDays , TimeToPrepareAndPayTaxesHours , TotalTaxRate, WDI)

StartUpBusinessFactorsData <- StartUpBusinessFactorsData[complete.cases(StartUpBusinessFactorsData),]


StartUpBusinessFactorsData$Year <- as.integer( 
  substr(StartUpBusinessFactorsData$Year,start = 2 ,stop = nchar( as.character( StartUpBusinessFactorsData$Year )  ) ) )



countries <- read.csv( "Data/WDI_Country.csv" , fileEncoding="latin1" )

countries<- countries[ , c(1,2,8,9,22)]


StartUpCountryData <- merge( StartUpBusinessFactorsData , countries , by="Country.Code")



ggplot( StartUpCountryData, aes( x = NoOfProceduresToStartUp ) ) +
  geom_histogram( binwidth=.4) + 
  # Vertical line indicating the mean value
  geom_vline( aes( xintercept = mean( NoOfProceduresToStartUp ) ), color="red" ) +
  scale_y_continuous( "Frequency of Occurance" ) +
  scale_x_continuous( "No of Procedures To Start Up" )


( max( StartUpCountryData$NoOfProceduresToStartUp ) - min( StartUpCountryData$NoOfProceduresToStartUp ) )/30


ggplot(data=StartUpCountryData, 
       aes(x=Region, y=NoOfProceduresToStartUp, fill=Region) ) + 
  geom_boxplot(outlier.shape = 15, outlier.size = 4) +
  # to show how the individual store sizes are distributed
  geom_jitter() +
  scale_y_continuous( name="No of Procedures To Start Up" ) + 
  scale_fill_brewer( name = "Region" , palette = "Dark2")

summary(StartUpCountryData$Region)

StartUpCountryData [, StartUpCountryData$Region=='']

modelNoOfProcedures <- lm( 
  NoOfProceduresToStartUp ~ . -Country.Code , 
  data = StartUpCountryData ) 


summary(modelNoOfProcedures)


Hyp_NotOECD <- subset( StartUpCountryData , Region != 'High income: OECD' )
Hyp_OECD <- subset( StartUpCountryData , Region == 'High income: OECD' )


pnorm(-2.875818)



# MLR
StartUpCountryData$Country.Code <- as.character( StartUpCountryData$Country.Code )
set.seed( 314 )
split <- sample.split( StartUpCountryData$NoOfProceduresToStartUp ,SplitRatio = .8 )
train <- subset( StartUpCountryData , split == TRUE )
test <- subset( StartUpCountryData , split == FALSE )

modelNoOfProcedures <- lm( 
  formula = NoOfProceduresToStartUp ~ . -Country.Code -Short.Name , 
  data = train ) 


summary( modelNoOfProcedures )



test$predictedNoOfProcedures <- predict( modelNoOfProcedures , newdata = test )


summary( modelNoOfProcedures )





