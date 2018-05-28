View("Zip_Zhvi_Summary_AllHomes.csv")
zhvi <- read.csv("Zip_Zhvi_Summary_AllHomes.csv", header = T)


View(zhvi)
str(zhvi)

zhvi$PeakQuarter.... <- as.Date(as.character(zhvi$PeakQuarter))

fiveDig$HPI <- as.numeric(as.character(fiveDig$HPI))
fiveDig$HPI.with.1990.base <- as.numeric(as.character(fiveDig$HPI.with.1990.base))
fiveDig$HPI.with.2000.base <- as.numeric(as.character(fiveDig$HPI.with.2000.base))


g <- ggplot(fiveDig, aes(zhvi$State ,zhvi$MoM))

g1 <- g + geom_point(color = 'orange', alpha= 0.4) + xlab("Year") + ylab("Annual change %") + geom_smooth(color = 'blue', alpha= 0.4, span = 0.3) + theme_light() 

g1

zhvi$State <- clean.zipcodes(zhvi$State)


zhvi <- merge(zhvi, zipcode, by.x='Zip', by.y='zip')



map<-get_map(location='united states', zoom=4, maptype = "terrain",
             source='google',color='color')

ggmap(map) + geom_point(
  aes(x=longitude, y=latitude, show_guide = TRUE, color = PeakZHVI), 
  data=zhvi, alpha=.8, na.rm = T)  + 
  scale_color_gradient(low="blue", high="red")

