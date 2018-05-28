fiveDig <- read.csv("HPI_AT_BDL_ZIP5.csv", header = T, skip = 7)

fiveDig <- fiveDig[,-7]

View(fiveDig)
str(fiveDig)

fiveDig$Annual.Change.... <- as.numeric(as.character(fiveDig$Annual.Change....))

fiveDig$HPI <- as.numeric(as.character(fiveDig$HPI))
fiveDig$HPI.with.1990.base <- as.numeric(as.character(fiveDig$HPI.with.1990.base))
fiveDig$HPI.with.2000.base <- as.numeric(as.character(fiveDig$HPI.with.2000.base))

install.packages("zipcode")
library("zipcode")
data(zipcode)



fiveDig$Five.Digit.ZIP.Code <- clean.zipcodes(fiveDig$Five.Digit.ZIP.Code)

fiveDig$Five.Digit.ZIP.Code
fiveDig <- merge(fiveDig, zipcode, by.x='ip', by.y='zip')
View(fiveDig)

View(zipcode)
install.packages("ggmap")
library(ggmap)
map<-get_map(location='united states', zoom=4, maptype = "terrain",
             source='google',color='color')

ggmap(map) + geom_point(
  aes(x=longitude, y=latitude, show_guide = TRUE, color = HPI), 
  data=fiveDig, alpha=.8, na.rm = T)  + 
  scale_color_gradient(low="blue", high="red")



# 2nd
g <- ggplot(fiveDig, aes(fiveDig$Year ,fiveDig$Annual.Change))

g1 <- g + geom_point(color = 'orange', alpha= 0.4) + xlab("Year") + ylab("Annual change %") + geom_smooth(color = 'blue', alpha= 0.4, span = 0.3) + theme_light() 

g1

#2nd 


ga <- ggplot(fiveDig, aes(fiveDig$Year ,fiveDig$HPI))
ga <- ga + geom_jitter(color = 'blue', alpha= 0.4) + xlab("Year") + ylab("HPI") + geom_smooth(color = 'red', alpha= 0.4, span = 0.3) + theme_light() 

multiplot(g1,ga, cols=2)


#3rd
ga <- ggplot(fiveDig, aes(fiveDig$Year ,fiveDig$Annual.Change....))
ga <- ga + geom_boxplot(color = 'blue', alpha= 0.4) + xlab("Year") + ylab("Annual Change") 
ga


