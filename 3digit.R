install.packages("plotly")
install.packages("ggplot2")
library(plotly)
library(ggplot2)
getwd()

setwd("C:/Users/Akshay/Desktop/project")

threeDig <- read.csv("HPI_AT_BDL_ZIP3.csv", header = T, skip = 7)

threeDig <- threeDig[,-7]


str(threeDig)


threeDig[is.na(threeDig)] <- 0

View(threeDig)
str(threeDig)
threeDig$Annual.Change.... <- as.numeric(as.character(threeDig$Annual.Change....))

threeDig$HPI <- as.numeric(as.character(threeDig$HPI))
threeDig$HPI.with.1990.base <- as.numeric(as.character(threeDig$HPI.with.1990.base))
threeDig$HPI.with.2000.base <- as.numeric(as.character(threeDig$HPI.with.2000.base))


threeDig[threeDig == '.'] <- 0

a  <- sum(threeDig$Annual.Change....)
a
#1st
g <- ggplot(threeDig, aes(threeDig$Year ,threeDig$Annual.Change))

g1 <- g + geom_point(color = 'orange', alpha= 0.4) + xlab("Year") + ylab("Annual change %") + geom_smooth(color = 'blue', alpha= 0.4, span = 0.3) + theme_light() 

g1
ggplotly(g1)

#2nd 3rd 4th 5th


ga <- ggplot(threeDig, aes(threeDig$Year ,threeDig$HPI))
ga <- ga + geom_jitter(color = 'blue', alpha= 0.4) + xlab("Year") + ylab("HPI") + geom_smooth(color = 'red', alpha= 0.4, span = 0.3) + theme_light() 

gb <- ggplot(threeDig, aes(threeDig$Year ,threeDig$HPI.with.1990.base))
gb <- gb + geom_jitter(color = 'blue', alpha= 0.4) + xlab("Year") + ylab("HPI 1990") + geom_smooth(color = 'red', alpha= 0.4, span = 0.3) + theme_light() 

gc <- ggplot(threeDig, aes(threeDig$Year ,threeDig$HPI.with.2000.base))
gc <- gc + geom_jitter(color = 'blue', alpha= 0.4) + xlab("Year") + ylab("HPI 2000") + geom_smooth(color = 'red', alpha= 0.4, span = 0.3) + theme_light() 

gd <- ggplot(threeDig, aes(threeDig$Year ,threeDig$Annual.Change....))
gd <- g + geom_jitter(color = 'blue', alpha= 0.4) + xlab("Year") + ylab("Annual change %") + geom_smooth(color = 'red', alpha= 0.4, span = 0.3) + theme_light() 

multiplot(ga,gb,gc,g1, cols=2)

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
