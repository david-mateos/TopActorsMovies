library(jsonlite)
library(httr)
library(tidyverse)
library(stringr)

## get API key @ www.omdbapi.com/apikey.aspx* ##
key <<- ""

get_type <- function(imdb_id){
  call <- paste("http://www.omdbapi.com/?apikey=",key,"&i=",imdb_id, sep="")
  result <- content(GET(call, accept_json()))
  if (result$Response == "True"){
    return(result$Type)
  } else { return(result$Response) }
}
get_movie <- function(imdb_id){
  call <- paste("http://www.omdbapi.com/?apikey=",key,"&i=",imdb_id, sep="")
  result <- content(GET(call, accept_json()))
  return(result)
}
get_show <- function(imdb_id){
  call <- paste("http://www.omdbapi.com/?apikey=",key,"&i=",imdb_id, sep="")
  result <- content(GET(call, accept_json()))
  return(result)
}

#### read in file with ids from imdb ####
# Download @ https://datasets.imdbws.com/  
imdb_data <- as_tibble(read.table(file = 'title.ratings.tsv', sep = '\t', header = TRUE))
imdb_data <- imdb_data %>%
              filter(averageRating >= 7) %>%  # only keep ids with a 7+ rating
              filter(numVotes >= 150000) %>%  # and 250k votes
              select(tconst) %>%
              rename(id = tconst)

## See if it's a movie or series ID since both have different response 
## structures and sizes. Append the type to the tible with ids  
status <- "True"
i <- 1

while(is.na(imdb_data$id[i]) == F){
  if(status != "False"){
    
    imdb_data$type[i] <- get_type(imdb_data$id[i])
    status= imdb_data$type[i]
    i= i+1
    
  } else if (status == "False"){ break }
  
}

## split into movie and tv sets
movie_data <- imdb_data %>% filter(type == "movie") %>% select(id)
#tv_data <- imdb_data %>% filter(type == "series") %>% select(id)

## get full data and make a big table from the responses
status <- "True"
i <- 1
results <- list()
ids_skipped <- c()

while(is.na(movie_data$id[i]) == F){
  
  if (status == "True"){
    
    movie <- get_movie(movie_data$id[i])
    
    if(length(movie$Ratings) %in% c(0,1,2)){
      ## skip movies that don't have 3 ratings
      status= movie$Response
      ids_skipped <- append(ids_skipped, movie$imdbID) # keep them just in case
      i= i+1
    } else {
      results[[i+1]] <- unlist(movie)
      status= movie$Response
      i= i+1
    }
  } else{ break }
}

movie_data <- as_tibble(do.call("rbind", results))
movie_data <- movie_data %>%
  mutate(Year = as.numeric(Year)) %>%
  mutate(Released = parse_date_time(Released, orders = c("dmy"))) %>%
  mutate(imdbVotes = as.numeric(gsub(",", "", imdbVotes, fixed = T))) %>%
  mutate(Runtime = as.numeric(gsub(" min", "", Runtime, fixed = T)))

#View(movie_data) # see the complete dataset here

## Movie List 1: A list of movies that feature a
## "the Best 10 Actors". A top Actor will be an actor
## that has appeared in the most movies in this set of 
## successful and/or popular films

## S1: make a list of all actors 
all_actors<- trimws(as_vector(unlist(movie_data %>%
                                       mutate(Actors = strsplit(Actors, ",")) %>%
                                       select(Actors), use.names = F)), "l")

# S2: Count Unique appearances in the list to proxy success
top_actors<- as_tibble(table(all_actors)) %>%
                  arrange(desc(n)) %>%
                  top_n(10) %>% 
                  select(all_actors)

## S3: filter the bigger dataset for movies with the Top 10 Actors:
movies_with_top_actors <- movie_data %>% 
                            filter(str_detect(Actors, 
                                                paste(top_actors$all_actors, collapse = "|"))) %>%
                            select(Title, Year, Genre, Plot, Actors, imdbRating) %>%
                            arrange(desc(imdbRating))

#### There are 151 films with these actors ###

# View(movies_with_top_actors) 
# write.csv(movies_with_top_actors, file= "moviesToWatch1.csv")  #Export it!
