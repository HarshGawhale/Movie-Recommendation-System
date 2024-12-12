# Content Based Filter recommendation system
library(data.table)
library(reshape2)
library(proxy)
library(ggplot2)

# Retrieve and display data
setwd("D:/Movie Recommendation System")
movies<- read.csv("movies.csv",stringsAsFactors=FALSE)
ratings<- read.csv("ratings.csv")
str(movies)
str(ratings)
genres <- as.data.frame(movies$genres, stringsAsFactors=FALSE)
genres2 <- as.data.frame(tstrsplit(genres[,1], '[|]', type.convert=TRUE), stringsAsFactors=FALSE)
colnames(genres2) <- c(1:10)

genre_list <- c("Action", "Adventure", "Animation", "Children", "Comedy", "Crime","Documentary", "Drama", "Fantasy","Film-Noir", "Horror", "Musical", "Mystery","Romance","Sci-Fi", "Thriller", "War", "Western")

genre_matrix <- matrix(0,9743,18) #empty matrix
genre_matrix[1,] <- genre_list #set first row to genre list
colnames(genre_matrix) <- genre_list #set column names to genre list

#iterate through matrix
for (i in 1:nrow(genres2)) {
  for (c in 1:ncol(genres2)) {
    genmat_col = which(genre_matrix[1,] == genres2[i,c])
    genre_matrix[i+1,genmat_col] <- 1
  }
}

#convert into dataframe
genre_matrix2 <- as.data.frame(genre_matrix[-1,], stringsAsFactors=FALSE) #remove first row, which was the genre list
for (c in 1:ncol(genre_matrix2)) {
  genre_matrix2[,c] <- as.integer(genre_matrix2[,c])
} #convert from characters to integers

binaryratings <- ratings
for (i in 1:nrow(binaryratings)){
  if (binaryratings[i,3] > 3){
    binaryratings[i,3] <- 1
  }
  else if (binaryratings[i,3] == 3){
    binaryratings[i,3] <- 0
  }
  else {
    binaryratings[i,3] <- -1
  }
}

binaryratings2 <- dcast(binaryratings, movieId~userId, value.var = "rating", na.rm=FALSE)
for (i in 1:ncol(binaryratings2)){
  binaryratings2[which(is.na(binaryratings2[,i]) == TRUE),i] <- 0
}
binaryratings2 = binaryratings2[,-1] #remove movieIds col. Rows are movieIds, cols are userIds

#Remove rows that are not rated from movies dataset
movieIds <- length(unique(movies$movieId))
ratingmovieIds <- length(unique(ratings$movieId)) 
movies2 <- movies[-which((movies$movieId %in% ratings$movieId) == FALSE),]
rownames(movies2) <- NULL
#Remove rows that are not rated from genre_matrix2
genre_matrix3 <- genre_matrix2[-which((movies$movieId %in% ratings$movieId) == FALSE),]
rownames(genre_matrix3) <- NULL

#Calculate dot product for User Profiles
result = matrix(0,18,610)
for (c in 1:ncol(binaryratings2)){
  for (i in 1:ncol(genre_matrix3)){
    result[i,c] <- sum((genre_matrix3[,i]) * (binaryratings2[,c]))
  }
}

#Convert to Binary scale
for (c in 1:ncol(result)){
  for (i in 1:nrow(result)){
    if (result[i,c] < 5){
      result[i,c] <- 0
    }
    else {
      result[i,c] <- 1
    }
  }
}

result2 <- result[,5] #User's profile
sim_mat <- rbind.data.frame(result2, genre_matrix3)
sim_mat <- data.frame(lapply(sim_mat,function(x){as.integer(x)})) #convert data to type integer

#Calculate Jaccard distance between user profile and all movies
sim_results <- dist(sim_mat, method = "Jaccard")
sim_results <- as.data.frame(as.matrix(sim_results[1:9724]))
rows <- which(sim_results == min(sim_results))
#Recommended movies
movies[rows,]


#Bar Graph
genre_count <- colSums(genre_matrix3)
distrib <- data.frame(Genre=genre_list, Count=genre_count)
rownames(distrib) <- NULL

ggplot(distrib, aes(x = Genre, y = Count)) +
  geom_bar(stat="identity", fill = 'steelblue') +
  geom_text(aes(label=Count), vjust=-0.3, size=3.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Genre Distribution in the Dataset")
