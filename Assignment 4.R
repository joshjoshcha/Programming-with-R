# Question 1 
movie_info <- list(
  Title = "Inception",
  Release_Year = 2010,
  Box_Office_Revenue = 829.9,  
  Rotten_Tomatoes_Scores = list(Tomatometer = 87, Audience = 91),
  Cast = c("Leonardo DiCaprio", "Joseph Gordon-Levitt", "Ellen Page", "Tom Hardy")
)

movie_info

# a
str(movie_info)

# b 
movie_info$Director <- "Christopher Nolan"
movie_info

# c
movie_info$Rotten_Tomatoes_Scores$Tomatometer
movie_info[["Rotten_Tomatoes_Scores"]][["Tomatometer"]]
movie_info

# d
movie_info2 <- list(
  Title = "The Conjuring",
  Release_Year = 2013,
  Box_Office_Revenue = 319.5,
  Rotten_Tomatoes_Scores = list(Tomatometer = 86, Audience = 83),
  Cast = c("Vera Farmiga", "Patrick Wilson", "Lili Taylor", "Ron Livingston"),
  Director = "James Wan"
)
movie_info2
movie_collection <- list(Inception = movie_info, Conjuring = movie_info2)
movie_collection

# e
movie_titles <- as.character(sapply(movie_collection, function(x) x$Title))
movie_titles



