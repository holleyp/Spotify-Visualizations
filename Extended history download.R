library("jsonlite")
library("lubridate")
library("ggplot2")
library(igraph)
library(Matrix)

#
#
#reading the files and parsing them
#
#

# function to read a .json file and make it into a nice data frame I can use
# this function takes the file name and returns the data frame 
readJSON <- function(filename) {
  initial <- as.data.frame(t(as.matrix(as.data.frame(do.call(cbind,fromJSON(filename, file))))))
  rownames(initial) <- NULL
  toReturn <- as.data.frame(initial)
  return(toReturn)
}

setwd("C:/Users/paulh/OneDrive/Desktop/Code/R/Spotify Project/Extended Data")
# reading the data from the three files
endsong_0 <- readJSON("endsong_0.json")
endsong_1 <- readJSON("endsong_1.json")
endsong_2 <- readJSON("endsong_2.json")

# one big data frame with all my data
data <- rbind(endsong_0, endsong_1, endsong_2)



#
#
# cleaning the data
#
#

# Fixing the time stamp column
# Parsing into date time
data$ts <- ymd_hms(unlist(data[,1]))
# Putting the data frame in order of songs played
data <- data[order(data$ts),]





#
#
# analysis
#
#



#songs I played intentionally the most
#subset the data so it is only ones that started playing when it was not shuffled
selected <- subset(data, reason_start == "clickrow")
#get count of each how many times each track is played
selectedCount <- selected %>% count(master_metadata_track_name) 
#sort that so I can get my results
sorted <- selectedCount[order(selectedCount$n, decreasing = TRUE),]
head(sorted, n = 10)

#song I played the most intentionally is "1985" by Freddie Gibs with 16 deliberate play


#songs that I skipped instead of letting them finish the most
skipped <- subset(data, reason_end == 'fwdbtn')
skippedCount <- skipped %>% count(master_metadata_track_name)
skipSort <- skippedCount[order(skippedCount$n, decreasing = TRUE),]
head(skipSort, n = 10)

#most skipped song is "Norf Norf" by Vincent Staples with 48 skips



#Total Time listened
sum(unlist(data$ms_played)) / 60000
#123364.8 minutes of music listened to on my spotify account so far


#total minutes listened to for each day 
perDay <- tapply(X = unlist(data$ms_played) / 60000, date(data$ts), sum, simplify = TRUE)
# vector of all the dates so I can 
perDayNames <- ymd(names(perDay))
# cumulative sum
accumulative <- cumsum(perDay)

perDayDF <- data.frame(perDayNames, perDay, accumulative)

# per day
ggplot(perDayDF, aes(x = perDayNames, y = perDay)) + geom_col()


ggplot(perDayDF, aes(x = perDayNames, y = accumulative)) + geom_line()

# how much I listen on average per day of the week
ggplot(perDayDF, aes(x = wday(perDayNames, label = TRUE), y = perDay, fill = wday(perDayNames, label = TRUE))) + 
  geom_boxplot()  +
  xlab("Day of the Week") +
  ylab("Minutes Spent Listening") +
  ggtitle("Which Day of the Week Do I Listen to the Most Music?")


#per month box plots
ggplot(perDayDF, aes(x = month(perDayNames, label = TRUE), y = perDay, fill = month(perDayNames, label = TRUE))) + 
  geom_boxplot()  +
  xlab("Month of the Year") +
  ylab("Minutes Spent Listening") +
  ggtitle("Which Month Do I Listen to the Most Music?")
geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = .2)

#per year box plots
ggplot(perDayDF, aes(x = year(perDayNames), y = perDay, fill = year(perDayNames), group = year(perDayNames))) + 
  geom_boxplot()  +
  xlab("Year") +
  ylab("Minutes Spent Listening") +
  ggtitle("Which Year Have I Listen to the Most Music?")


# the box plots are made with median values I want to compare it to the mean
tapply(perDay, wday(perDayNames, label = TRUE), median)     
tapply(perDay, wday(perDayNames, label = TRUE), mean)
# the mean values are almost twice as large as the median values

tapply(perDay, wday(perDayNames, label = TRUE), sum)

tapply(perDay, year(perDayNames), median)

#histogram but it does not look very good right now
ggplot(perDayDF, aes(x = perDay)) +
  geom_histogram(binwidth = 1, aes(y = ..density..), color = "black", fill = "white") + 
  geom_density(alpha = .2, fill = "#FF6666")












#
#
# Subsetting 2021 -> 
#
#

recent <- subset(data, date(ts) > date("2020-12-30"))
recentTime <- subset(perDayDF, date(perDayNames) > ymd("2020-12-30"))



ggplot(recentTime, aes(x = perDay)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "white", bins = 30) + 
  geom_density(alpha = .2, fill = "#FF6666") + 
  ggtitle("Minutes Listend to Histogram")



tapply(recentTime$perDay, wday(recentTime$perDayNames, label = TRUE), median)     
tapply(recentTime$perDay, wday(recentTime$perDayNames, label = TRUE), mean)


#graph with home much music I listen to during each day of the week
ggplot(recentTime, aes(x = wday(perDayNames, label = TRUE), y = perDay, fill = wday(perDayNames, label = TRUE))) + 
  geom_boxplot()  +
  xlab("Day of the Week") +
  ylab("Minutes Spent Listening") +
  ggtitle("Which Day of the Week Do I Listen to the Most Music?")







#
#
# work in progress make the same boxplots but with hour of the day not day listened to
#
#


#getting that data cleaned





#math
#need segments from each hour from a certain time, each 3600 block
#get sum of seconds listened to in this hour
#need to make sure that when I get seconds that a full hour does equal 3600 seconds
#make a new dataframe of the values and the times
#run the same box plot code on this


# 
perDay <- tapply(unlist(data$ms_played) / 60000, date(data$ts), sum, simplify = TRUE)
# 
perDayNames <- ymd(names(perDay))

hours <- tapply(unlist(data$ms_played) / 60000, hour(data$ts), sum)
hourNames <- names(hours)
data$ms
hourDF <- data.frame(hours, hourNames)
ggplot(data, aes(x = hour(ts)), y = ms_played, fill = hour(ts)) + 
  geom_boxplot()

hour(data$ts[1])

ggplot(data, aes(x = hour(ts),  y = perDay, fill = hour(ts))) + 
  geom_boxplot()  +
  xlab("Day of the Week") +
  ylab("Minutes Spent Listening") +
  ggtitle("Which Day of the Week Do I Listen to the Most Music?")
names(hourDF)

ggplot(hourDF, aes(x = reorder(hourNames, hours), y = hours)) + geom_col()







#
#
# GRAPH PROJECT
#
#

#make adjacency matrix
  #create function that does that from the large data frame I give it
#look into the package 



n = length(unique(data$master_metadata_album_artist_name))
Adj = matrix(0,nrow = n, ncol = n)
#spot on the artist list will be the row/col for this artist

artist = unlist(unique(data$master_metadata_album_artist_name))

which(artist == data$master_metadata_album_artist_name[30000])

# creates graph from data frame,
# there is a link between nodes when songs are played consecutively 
# graph is stored in an adjacency matrix
createGraph <- function(data) {
  artist = unlist(unique(data$master_metadata_album_artist_name))
  nArt = length(artist)
  Adj = Matrix(0, nrow = nArt, ncol = nArt)
  for (i in 1:nrow(data)) {
    if (data$reason_start[i] == "trackdone") {
      curr = which(artist == data$master_metadata_album_artist_name[i])
      prev = which(artist == data$master_metadata_album_artist_name[i - 1])
      # increase both entries for the matrix
      Adj[curr, prev] = Adj[curr, prev] + 1
      Adj[prev, curr] = Adj[prev, curr] + 1
    }
  }
  
  # convert matrix to graph using graph package, and give it proper vairable names
  G = graph_from_adjacency_matrix(Adj, mode = "undirected", diag = FALSE, weighted = TRUE)
  V(G)$label <- artist
  V(G)$degree <- degree(G)
  
  #delete vertices with no connections
  G <- delete_vertices(G, V(G)[degree(G) == 0])
  
  return(G)
}

network = createGraph(data)

# variable that stores the degree of the 100th node
percentile = tail(sort(V(network)$degree),50)[1]

#getting the top 50 most played artists
counter = 1
while (counter <= length(artist) - 50) {
  if (V(network)$degree[counter] < percentile) {
    network = delete_vertices(network, counter)
  } else {
    counter = counter + 1
  }
}
plot(network)


#function to get the top N artist out of the graph
topN <- function(data, N) {
  check = tail(sort(V(data)$degree),N)[1]
  counter = 1
  toDel = c()
  while (counter <= length(data)) {
    if (V(data)$degree[counter] < check) {
      toDel = c(toDel, counter)
    } 
    counter = counter + 1
    
  }
  data <- delete_vertices(data,toDel)
  return(data)
}

#function to ge the bottom M artists out of the graph
BottoM <- function(data, M) {
  check = head(sort(V(data)$degree),M)[1]
  counter = 1
  toDel = c()
  while (counter <= length(data)) {
    if (V(data)$degree[counter] > check) {
      toDel = c(toDel, counter)
    }
    counter = counter + 1
  }
  data <- delete_vertices(data,toDel)
  return(data)
}


fifty = topN(network,50)

network = createGraph(data)

#plot the graph with a layout that tries to space everything out
plot(topN(network,50),layout = layout.fruchterman.reingold)
plot(BottoM(network,1500), layout = layout.fruchterman.reingold)
plot(network, layout = layout.fruchterman.reingold)

# I have having difficulty presentign this data well, this comes from most of 
# the top 50 entries being connected to each other and all entries become meaningless
# since there are over 1000 different artists.

# need to find package that deals with handling visualization of large graphs
