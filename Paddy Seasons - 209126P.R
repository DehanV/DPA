YalaData = read.csv("C:\\MBA\\Sem 4\\Analytics\\Paddy\\paddy_production_in_yala_season_1952_2012.csv", header = TRUE)
MahaData = read.csv("C:\\MBA\\Sem 4\\Analytics\\Paddy\\paddy_production_in_maha_season_1952-2012.csv", header = TRUE)
#Extract first two letters of Maha Season
first2<-substr(MahaData$Maha.Season, start = 1, stop = 2)
#Extract characters after "/" of Maha Season
MahaData$Maha.Season<-sub(".*/", "", MahaData$Maha.Season)
#Combine extracted characters together
MahaData$Maha.Season<-paste0(first2,MahaData$Maha.Season)
#Replace 1900 with 2000
MahaData$Maha.Season[MahaData$Maha.Season==1900] <- 2000
#Convert to Integer
MahaData$Maha.Season <- as.numeric(as.character(MahaData$Maha.Season)) 
MahaData$Maha.Season
YalaData$Yala.Season
#Rename the column header
names(MahaData)[names(MahaData) == "Maha.Season"] <- "Year"
names(YalaData)[names(YalaData) == "Yala.Season"] <- "Year"
#Add new column with values
MahaData$Season<-"Maha"
YalaData$Season<-"Yala"
#colnames(MahaData)
#colnames(YalaData)
#Append two tables together
MasterData <-rbind(MahaData,YalaData)
MasterData
#Plot all variables
plot(MasterData)
#Summary of MasterData
summary(MasterData)

#Mean Production of each season
aggregate(MasterData$Production.000.Mt.,list(MasterData$Season), mean)

hist(MasterData$Average.Yield.Kg.Ha[MasterData$Season=="Maha"])
hist(MasterData$Average.Yield.Kg.Ha[MasterData$Season=="Yala"])

df <- data.frame(MasterData)


#Calculate Harvesting Percentage
df$HavrestPer<- df$Harvested.000.Ha./df$Sown.000.Ha
#Calculate Production Ratio
df$ProductionRatio<- df$Production.000.Mt./df$Harvested.000.Ha

df$HavrestPer
df$ProductionRatio


plot(df$Year[df$Season == "Yala"], df$Production.000.Mt. [df$Season == "Yala"],type="l",col="red")
lines(df$Year[df$Season == "Maha"],df$Production.000.Mt.[df$Season == "Maha"],col="green")

plot(df$Year[df$Season == "Yala"], df$HavrestPer[df$Season == "Yala"],type="l",col="red")
lines(df$Year[df$Season == "Maha"],df$HavrestPer[df$Season == "Maha"],col="green")

plot(df$Year[df$Season == "Yala"], df$ProductionRatio[df$Season == "Yala"],type="l",col="red")
lines(df$Year[df$Season == "Maha"],df$ProductionRatio[df$Season == "Maha"],col="green")


cor.test(df$HavrestPer[df$Season == "Yala"], df$ProductionRatio[df$Season == "Yala"])
cor.test(df$HavrestPer[df$Season == "Maha"], df$ProductionRatio[df$Season == "Maha"])