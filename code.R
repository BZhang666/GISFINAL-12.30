
window <- as.owin(BoroughMapBNG)
plot(window)
WastepointSub.ppp <- ppp(x=WastepointSub@coords[,1],y=WastepointSub@coords[,2],window=window)
plot(WastepointSub.ppp,pch=16,cex=0.5, main="Waste Point London")
plot(quadratcount(WastepointSub.ppp, nx = 20, ny = 16),add=T,col="red")
Qcount<-data.frame(quadratcount(WastepointSub.ppp, nx = 20, ny = 16))
QCountTable <- data.frame(table(Qcount$Freq, exclude=NULL))
QCountTable


K <- Kest(WastepointSub.ppp)
plot(K)







WastepointSub <- data.frame(WastepointSub@coords[,1:2])
#now run the dbscan analysis
db <- fpc::dbscan(WastepointSub, eps = 1500, MinPts = 3)
#now plot the results
plot(db, WastepointSub, main = "DBSCAN Output", frame = F)
plot(BoroughMapBNG, add=T)
library(ggplot2)
db



qtm(LondonWards)
proj4string(LondonWards) <- CRS("+init=epsg:27700")
tmap_mode("view")
WastepointSub2 <- WastepointBNG[LondonWards,]
tm_shape(LondonWards) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(WastepointSub2) +
  tm_dots(col = "blue")
summary(WastepointSub2)
res <- poly.counts(WastepointSub2, LondonWards)
LondonWards@data$WastepointCount<-res
LondonWards@data$BlueDensity <- LondonWards$WastepointCount/poly.areas(LondonWards)
LondonWards@data


tm_shape(LondonWards) +
  tm_polygons("BlueDensity",
              style="jenks",
              palette="PuOr",
              midpoint=NA,
              title="Blue Plaque Density")
LondonWards@data$WasteDensity <- as.numeric()

library(spdep)
#####
#First calculate the centroids of all Wards in London
coordsW <- coordinates(WardsOUT)
plot(coordsW)
LB_nb <- poly2nb(WardsOUT, queen=T)
#plot them
plot(LB_nb, coordinates(WardsOUT), col="red")
#add a map underneath
plot(WardsOUT, add=T)
Lward.lw <- nb2listw(LB_nb, style="C")
head(Lward.lw$neighbours)
I_LWard_Global_Density <- moran.test(WardsOUT@data$WasteDensity, Lward.lw)
I_LWard_Global_Density

C_LWard_Global_Density <- geary.test(WardsOUT@data$WasteDensity, Lward.lw)
C_LWard_Global_Density

G_LWard_Global_Density <- globalG.test(WardsOUT@data$WasteDensity, Lward.lw)
G_LWard_Global_Density


LondonWardsSF <- st_as_sf(LondonWards)

newvar<-0
attach(LondonWardsSF)
newvar<-0
recode<-function(variable){
  newvar[variable>0]<-TRUE
  newvar[variable==0]<-FALSE
  return(newvar)
}
LondonWardsSF$WastepointCount_recode <- recode(WastepointCount)
LondonWardsSF$WastepointCount <- as.numeric(LondonWardsSF$WastepointCount)

tm_shape(LondonWardsSF) +
  tm_polygons("WastepointCount_recode")

WithWaste <- subset(WardsOUTSF, WastepointCount_recode > 0, select = c(NAME, Value,WastepointCount,WasteDensity))
WithoutWaste<- subset(LondonWardsSF, WastepointCount_recode < 1, select = c(NAME, Value))



qplot(sample = Value, data = WithWaste)
ggplot(WithoutWaste, aes(sample=Value))+stat_qq()

t.test(WithWaste$Value,WithoutWaste$Value,paired = F)

model1 <- lm(log(Value) ~ WastepointCount, data = WardsOUTSF)
model1_res <- tidy(model1)
summary(model1)



