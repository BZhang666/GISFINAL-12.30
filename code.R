
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






res <- poly.counts(Wastepoint, LondonWards)
LondonWards@data$WastepointCount<-res
LondonWards@data$WasteDensity <- LondonWards$WastepointCount/poly.areas(LondonWards)
LondonWards@data


tm_shape(LondonWards) +
  tm_polygons("WasteDensity",
              style="jenks",
              palette="PuOr",
              midpoint=NA,
              title="Blue Plaque Density")
LondonWards@data$WasteDensity <- as.numeric()

library(spdep)
#####
#First calculate the centroids of all Wards in London


C_LWard_Global_Density <- geary.test(LondonWardsBNG@data$WasteDensity, Lward.lw)
C_LWard_Global_Density




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

WithWaste <- subset(WardsOUTSF, WastepointCount_recode== "Yes", select = c(NAME, Value,WastepointCount,WasteDensity))
WithoutWaste<- subset(WardsOUTSF, WastepointCount_recode== "No", select = c(NAME, Value))



qplot(sample = Value, data = WithWaste)
ggplot(WithoutWaste, aes(sample=Value))+stat_qq()

t.test(WithWaste$Value,WithoutWaste$Value,paired = F)
WardsOUTSF=st_as_sf(WardsOUT)
model1 <- lm(log(Value) ~ WastepointCount, data = WithWaste)
model1_res <- tidy(model1)
summary(model1)
WithWaste$model_final_res <- model1$residuals
qtm(WithWaste, fill = "model_final_res")

