
library(rvest)
library(ggplot2)

# Multiple plot function - from http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
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
  require(grid)
  
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


##Look at all the World Championships between 2006 (when the scoring changed) and 2015 
##Note that not all years have World Championships (no WC during Olympic years)
wcYears <- c(2006, 2007, 2009, 2010, 2011, 2013, 2014, 2015)

##Pull out all medallists in individual events (all-around + event finals)

MenAA <- MenFX <- MenPH <- MenSR <- MenVT <- MenPB <- MenHB <-
  WomenAA <- WomenVT <- WomenUB <- WomenBB <- WomenFX <- matrix(NA, nrow=length(wcYears),
                                                                ncol=3)

for(i in 1:length(wcYears))
{
  wcWeb <- read_html(paste("https://en.wikipedia.org/wiki/", wcYears[i],
                           "_World_Artistic_Gymnastics_Championships", 
                           sep=""))
    
  ##Note that different years have different tables (e.g. some have tables with oldest and youngest contestants and some don't,
  ##post-Olympic years don't have team competitions)
  
  ##Men's 
  if(wcYears[i] == 2006)
  {
    t <- 3:9
  }
  if(wcYears[i] == 2007)
  {
    t <- 5:11 
  }  
  if(wcYears[i] == 2009)
  {
    t <- c(3,5,7,9,11,13,15)
  }
  if(wcYears[i] == 2010)
  {
    t <- c(7,9,11,13,15,17,19)
  }
  if(wcYears[i] == 2011)
  {
    t <- c(21,23,25,27,29,31,33)
  }
  if(wcYears[i] == 2013)
  {
    t <- 4:10
  }
  if(wcYears[i] == 2014)
  {
    t <- 6:12
  }  
  if(wcYears[i] == 2015)
  {
    t <- 5:11
  }  
  
  MenAA[i,] <- (wcWeb %>%
                 html_nodes(".wikitable") %>% .[[t[1]]] %>%  
                 html_table())[1:3,"Total"]
  MenFX[i,] <- (wcWeb %>%
                 html_nodes(".wikitable") %>% .[[t[2]]] %>%
                 html_table())[1:3,"Total"]
  MenPH[i,] <- (wcWeb %>%
                 html_nodes(".wikitable") %>% .[[t[3]]] %>%
                 html_table())[1:3,"Total"]
  MenSR[i,] <- (wcWeb %>%
                 html_nodes(".wikitable") %>% .[[t[4]]] %>%
                 html_table())[1:3,"Total"]
  MenVT[i,] <- (wcWeb %>%
                  html_nodes(".wikitable") %>% .[[t[5]]] %>%
                  html_table())[1:3,"Total"]
  MenPB[i,] <- (wcWeb %>%
                 html_nodes(".wikitable") %>% .[[t[6]]] %>%
                 html_table())[1:3,"Total"]
  MenHB[i,] <- (wcWeb %>%
                 html_nodes(".wikitable") %>% .[[t[7]]] %>%
                 html_table())[1:3,"Total"]
  
  ##Women's 
  if(wcYears[i] == 2006)
  {
    t <- 11:15
  }
  if(wcYears[i] == 2007)
  {
    t <- 13:17
  } 
  if(wcYears[i] == 2009)
  {
    t <- c(17,19,21,23,25)
  }
  if(wcYears[i] == 2010)
  {
    t <- c(23,25,27,29,31)
  }
  if(wcYears[i] == 2011)
  {
    t <- c(9,11,13,15,17)
  }
  if(wcYears[i] == 2013)
  {
    t <- 11:15
  }  
  if(wcYears[i] == 2014)
  {
    t <- 14:18
  } 
  if(wcYears[i] == 2015)
  {
    t <- 13:17
  }  
  
  WomenAA[i,] <- (wcWeb %>%
                   html_nodes(".wikitable") %>% .[[t[1]]] %>%  
                   html_table())[1:3,"Total"]
  WomenVT[i,] <- (wcWeb %>%
                   html_nodes(".wikitable") %>% .[[t[2]]] %>%  
                   html_table())[1:3,"Total"]
  WomenUB[i,] <- (wcWeb %>%
                   html_nodes(".wikitable") %>% .[[t[3]]] %>%  
                   html_table())[1:3,"Total"]
  WomenBB[i,] <- (wcWeb %>%
                   html_nodes(".wikitable") %>% .[[t[4]]] %>%  
                   html_table())[1:3,"Total"]
  WomenFX[i,] <- (wcWeb %>%
                   html_nodes(".wikitable") %>% .[[t[5]]] %>%  
                   html_table())[1:3,"Total"]
}


AllMedals <- data.frame(Year=as.character(wcYears),
                       MenAA=MenAA,
                       MenFX=MenFX,
                       MenHB=MenHB,
                       MenPB=MenPB,                       
                       MenPH=MenPH,
                       MenSR=MenSR,
                       MenVT=MenVT,
                       WomenAA=WomenAA,
                       WomenBB=WomenBB,
                       WomenFX=WomenFX,
                       WomenUB=WomenUB,
                       WomenVT=WomenVT)
##reshape to have all scores for the same event in the same column
AllMedalsLong <- reshape(AllMedals, direction="long",
                         varying=2:37, sep=".")
head(AllMedalsLong)
##change "time" to "Position"
colnames(AllMedalsLong)[colnames(AllMedalsLong)=="time"] <- "Position"

##add a column for quad
AllMedalsLong$Quad <- "2006-2007"
AllMedalsLong$Quad[as.numeric(as.character(AllMedals$Year)) >= 2009] <- "2009-2011"
AllMedalsLong$Quad[as.numeric(as.character(AllMedals$Year)) >= 2013] <- "2013-2015"

##save in a text file just in case things on Wikipedia change
write.table(x=AllMedalsLong[,c(1:14, 16)],file="Scores_medals_WC.txt",
            sep="\t", row.names=FALSE,
            quote=FALSE)

gg1 <- ggplot(AllMedalsLong, aes(x=Year, y=MenAA, col=Quad)) +
  geom_point(size=2.2) + ylab("Score") + ggtitle("Men - AA")

gg2 <- ggplot(AllMedalsLong, aes(x=Year, y=MenFX, col=Quad)) +
  geom_point(size=2.2) + ylab("Score") + ggtitle("Men - FX")

gg3 <- ggplot(AllMedalsLong, aes(x=Year, y=MenHB, col=Quad)) +
  geom_point(size=2.2) + ylab("Score") + ggtitle("Men - HB")

gg4 <- ggplot(AllMedalsLong, aes(x=Year, y=MenPB, col=Quad)) +
  geom_point(size=2.2) + ylab("Score") + ggtitle("Men - PB")

gg5 <- ggplot(AllMedalsLong, aes(x=Year, y=MenPH, col=Quad)) +
  geom_point(size=2.2) + ylab("Score") + ggtitle("Men - PH")

gg6 <- ggplot(AllMedalsLong, aes(x=Year, y=MenSR, col=Quad)) +
  geom_point(size=2.2) + ylab("Score") + ggtitle("Men - SR")

gg7 <- ggplot(AllMedalsLong, aes(x=Year, y=MenVT, col=Quad)) +
  geom_point(size=2.2) + ylab("Score") + ggtitle("Men - VT")

png("Scores_medals_WC_Men.png", width=1440, height=960)
multiplot(gg1, gg2, gg3, gg4, gg5, gg6, gg7, cols=3)
graphics.off()

gg8 <- ggplot(AllMedalsLong, aes(x=Year, y=WomenAA, col=Quad)) +
  geom_point(size=2.2) + ylab("Score") + ggtitle("Women - AA")

gg9 <- ggplot(AllMedalsLong, aes(x=Year, y=WomenBB, col=Quad)) +
  geom_point(size=2.2) + ylab("Score") + ggtitle("Women - BB")

gg10 <- ggplot(AllMedalsLong, aes(x=Year, y=WomenFX, col=Quad)) +
  geom_point(size=2.2) + ylab("Score") + ggtitle("Women - FX")

gg11 <- ggplot(AllMedalsLong, aes(x=Year, y=WomenUB, col=Quad)) +
  geom_point(size=2.2) + ylab("Score") + ggtitle("Women - UB") 

gg12 <- ggplot(AllMedalsLong, aes(x=Year, y=WomenVT, col=Quad)) +
  geom_point(size=2.2) + ylab("Score")  + ggtitle("Women - VT")

png("Scores_medals_WC_Women.png", width=1440, height=640)
multiplot(gg8, gg9, gg10, gg11, gg12, cols=3)
graphics.off()


