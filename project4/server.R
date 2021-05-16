###############################
# RUN THESE LOCALLY FIRST
#install.packages("devtools")
#devtools::install_github("stefanwilhelm/ShinyRatingInput")
###############################


#################################
#Install ShinyRatingInput
#################################
library("shinydashboard")
library("shinyjs")
library("ShinyRatingInput")
library("reshape2")
library("recommenderlab")
library("dplyr")
library("data.table")
library("pryr")

# load functions
#source('functions/cf_algorithm.R') # collaborative filtering
#source('functions/similarity_measures.R') # similarity measures

set.seed(1234)

# preprocess movies info
preprocess_movies <- function(movies) {
  
  movies = strsplit(movies, split = "::", fixed = TRUE, useBytes = TRUE)
  movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
  movies = data.frame(movies, stringsAsFactors = FALSE)
  
  colnames(movies) = c('MovieID', 'Title', 'Genres')
  movies$MovieID = as.integer(movies$MovieID)
  movies$Title = iconv(movies$Title, "latin1", "UTF-8")
  
  return (movies)
}


#load data - remote
read_data_remote <- function() {
  myurl = "https://liangfgithub.github.io/MovieData/"
  movies = readLines(paste0(myurl, 'movies.dat?raw=true'))
  
  movies <- preprocess_movies(movies)
  
  small_image_url = "https://liangfgithub.github.io/MovieImages/"
  movies$image_url = sapply(movies$MovieID, function(x) paste0(small_image_url, x, '.jpg?raw=true'))
  
  return(movies)
}


#load data - local
read_data_remote_ratings <- function() {
  
  ratings = read.csv(paste0('data/ratings.dat'), 
                     sep = ':',
                     colClasses = c('integer', 'NULL'), 
                     header = FALSE)
  colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')
  
  return(ratings)
}


# user ratings
get_user_ratings = function(value_list) {
  dat = data.table(MovieID = sapply(strsplit(names(value_list), "_"), 
                                    function(x) ifelse(length(x) > 1, x[[2]], NA)),
                   Rating = unlist(as.character(value_list)))
  dat = dat[!is.null(Rating) & !is.na(MovieID)]
  dat[Rating == " ", Rating := 0]
  dat[, ':=' (MovieID = suppressWarnings(as.numeric(MovieID)), Rating = suppressWarnings(as.numeric(Rating)))]
  dat = dat[Rating > 0]
}


### MAIN
movies <- read_data_remote() # change to local if required
ratings <- read_data_remote_ratings()

#### SYSTEM 1 GLOBAL SETTINGS ####

get_genre_matrix <- function (filtered_movies) {
  
  genres = as.data.frame(filtered_movies$Genres, stringsAsFactors = FALSE)
  tmp = as.data.frame(tstrsplit(genres[,1], '[|]', type.convert = TRUE), stringsAsFactors = FALSE)
  
  genre_list = c("Action", "Adventure", "Animation", 
                 "Children's", "Comedy", "Crime",
                 "Documentary", "Drama", "Fantasy",
                 "Film-Noir", "Horror", "Musical", 
                 "Mystery", "Romance", "Sci-Fi", 
                 "Thriller", "War", "Western")
  
  m = length(genre_list)
  genre_matrix = matrix(0, nrow(filtered_movies), length(genre_list))
  
  for(i in 1:nrow(tmp)){
    genre_matrix[i, genre_list %in% tmp[i,]]=1
  }
  
  colnames(genre_matrix) = genre_list
  remove("tmp", "genres")  
  
  return (genre_matrix)
}

get_updated_ratings <- function (filtered_movies) {
  genre_matrix <- get_genre_matrix(filtered_movies)
  tmp <- ratings %>% left_join(data.frame(MovieID = movies$MovieID, genre_matrix), by = "MovieID")
}

# System 1, Algorithm 1:
get_top_movies_filtered_bygenre = function (selected_genre, count) {
  
  if (!is.na(selected_genre)) {
    #filter movies first
    filtered_movies_genre <- movies %>% filter(grepl(selected_genre, Genres))  
    #featured_ratings <- get_updated_ratings(movies)
    #featured_ratings <- featured_ratings %>% filter (!!as.symbol(selected_genre) == 1)
  } else {
    filtered_movies_genre <- movies
  }
  
  #group by movie id and sort by average ratings in descending.
  
  movies_in_genre_by_popularity <- ratings %>% group_by(MovieID) %>% 
    summarize(ratings_per_movie = n(), ave_ratings = round(mean(Rating), dig=3)) %>%
    inner_join(filtered_movies_genre, by = 'MovieID') %>%
    top_n(count, ave_ratings) %>%
    arrange(desc(ratings_per_movie))
  
  return (movies_in_genre_by_popularity)

}

# System 1, Algorithm 2:
get_top_rated_movies_in_genre <- function(selected_genre, count) {
  # Definition of top-rated
  # Movies that have high rating by a large number of reviewers (at least 1000)
  if (!is.na(selected_genre)) {
    filtered_movies_genre <- movies %>% filter(grepl(selected_genre, Genres)) 
  } else {
    filtered_movies_genre <- movies
  }
  top_rated_in_genre <- ratings %>% 
    group_by(MovieID) %>% 
    summarize(ratings_per_movie = n(), ave_ratings = round(mean(Rating), dig=3)) %>%
    inner_join(filtered_movies_genre, by = 'MovieID') %>%
    filter(ratings_per_movie > 200) %>%
    top_n(count, ave_ratings) %>%
    arrange(desc(ave_ratings))
  return(top_rated_in_genre)
}

# System 1, Algorithm 3:
get_top_rated_movies_in_genre_weighted <- function(selected_genre, count, my_top_n=50) {
  # System 1, Algorithm 3: Weighted score, shrinkage estimator
  # WR = (v / (v + m)) * r + (m / (v + m)) * C
  # r = mean rating 
  # v = number votes 
  # m = min votes to be in top n 
  # C = mean vote across genre
  if (!is.na(selected_genre)) {
    filtered_movies_genre <- movies %>% filter(grepl(selected_genre, Genres)) 
  } else {
    filtered_movies_genre <- movies
  }
  
  ratings_in_genre <- ratings %>% 
    group_by(MovieID) %>% 
    summarize(ratings_per_movie = n(), ave_ratings = round(mean(Rating), dig=3)) %>%
    inner_join(filtered_movies_genre, by = 'MovieID')
  C <- mean(ratings_in_genre$ave_ratings)
  
  top_votes <- ratings_in_genre %>%
    top_n(my_top_n, ratings_per_movie) %>%
    arrange(desc(ratings_per_movie))
  m <- min(top_votes$ratings_per_movie)
  
  top_rated_in_genre <- 
    mutate(ratings_in_genre, weighted_rating = 
      (ratings_per_movie /  (ratings_per_movie + m)) * ave_ratings + 
      (m / (ratings_per_movie + m)) * C) %>%
    top_n(count, weighted_rating) %>%
    arrange(desc(weighted_rating))
  
  return(top_rated_in_genre)
}

#### SYSTEM 2 GLOBAL SETTINGS ############

#create train and test set
train.id = sample (nrow(ratings), floor(nrow(ratings)) * 0.8)  
train = ratings[train.id,] 
test  = ratings[-train.id,] 

create_utility_matrix <- function (input_data){
  
  i = paste0('u', input_data$UserID)
  j = paste0('m', input_data$MovieID)
  x = input_data$Rating
  tmp = data.frame(i, j, x, stringsAsFactors = T)
  Rmat = sparseMatrix(as.integer(tmp$i), as.integer(tmp$j), x = tmp$x)
  rownames(Rmat) = levels(tmp$i)
  colnames(Rmat) = levels(tmp$j)
  Rmat = new('realRatingMatrix', data = Rmat)
  
  return (Rmat)
}

# Try training methods here.
create_popular_training <- function(input_data) {
  rec_popular = Recommender(input_data, method='POPULAR')
  return (rec_popular)
}

# Collaborative methods
create_ubcf_training <- function(input_data) {
  rec_UBCF = Recommender(data = input_data, method='UBCF', parameter = 
                           list (normalize = 'Z-score', method = 'Cosine', nn = 25))
  return (rec_UBCF)
}

create_ibcf_training <- function(input_data) {
  rec_IBCF = Recommender(data = input_data, method='IBCF', parameter = 
                           list (normalize = 'Z-score', method = 'Cosine', nn = 25))
  return (rec_IBCF)
}

create_svd_training <- function(input_data) {
  rec_svd = Recommender(data = input_data, method='SVD')
  return (rec_svd)
}

retrieve_top_rated_users <- function(top_count=2000, is_unique_genre = TRUE, is_unique_movie=TRUE) {

  selected_genre <- NA # Since we need to collect all unique genres
  top_rated_movies <- get_top_rated_movies_in_genre_weighted(selected_genre, count=top_count)
  
  if (is_unique_genre) {
    top_rated_movies <- top_rated_movies %>% distinct(Genres, .keep_all = TRUE)
  }
    
  if (is_unique_movie) {
    top_rated_movies <- top_rated_movies %>% 
      mutate(first_two_words_movie_name = sub("(\\w+\\s+\\w+).*", "\\1", Title)) %>%
      distinct(first_two_words_movie_name, .keep_all = TRUE)
  }
    
  return (top_rated_movies)
}

top_rated_movies_by_genre <- retrieve_top_rated_users()
filtered_top_rated_movies <- ratings[ratings$MovieID %in% top_rated_movies_by_genre$MovieID,]


shinyServer(function(input, output, session) {
  
  print(mem_used())
  
  ########### SYSTEM 1 #############
  # Popular genre's when button is clicked
  df_genre <- eventReactive(input$genre_btn, {
    withBusyIndicatorServer("genre_btn", { # showing the busy indicator
      
      # hide the rating container
      useShinyjs()
      jsCode <- "document.querySelector('[data-widget=collapse]').click();"
      runjs(jsCode)
      
      # get the user's rating data
      selected_genre <- input$selected_genre
      
      #genre_popularity <- get_top_movies_filtered_bygenre(selected_genre, 10)
      genre_popularity <- get_top_rated_movies_in_genre_weighted(selected_genre, 10)
      
    }) # still busy
    
  }) # clicked on button
  
  
  output$topPopularMoviesByGenre <- renderUI({
    num_rows <- 2
    num_movies <- 5
    
    recom_top_popular_result <- df_genre()
    #print(recom_top_popular_result)
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
            
            div(style = "text-align:center", 
                a(img(src = recom_top_popular_result$image_url[(i - 1) * num_movies + j], height = 150))
            ),
            div(style="text-align:center; font-size: 100%", 
                strong(recom_top_popular_result$Title[(i - 1) * num_movies + j])
            )
            
        )        
      }))) # columns
    }) # rows
  })
 
  
  
  
  ########### SYSTEM 2 #############
  
  # show the movies to be rated
  output$ratings <- renderUI({
    
    num_rows <- 20
    num_movies <- 6 # movies per row
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        list(box(width = 2, 
                 div(style = "text-align:center", img(src = top_rated_movies_by_genre$image_url[(i - 1) * num_movies + j], height = 150)),
                 div(style = "text-align:center", strong(top_rated_movies_by_genre$Title[(i - 1) * num_movies + j])),
                 div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", 
                     ratingInput(paste0("select_", top_rated_movies_by_genre$MovieID[(i - 1) * num_movies + j]), label = "", dataStop = 5))))
      })))
    })
  })
  
  # Calculate recommendations when the button is clicked
  df <- eventReactive(input$btn, {
    withBusyIndicatorServer("btn", { # showing the busy indicator
      # hide the rating container
      useShinyjs()
      jsCode <- "document.querySelector('[data-widget=collapse]').click();"
      runjs(jsCode)
      
      # get the user's rating data
      value_list <- reactiveValuesToList(input)
      user_ratings <- get_user_ratings(value_list)
      
      #print(user_ratings)
      #print(paste0('num_rows : ', nrow(user_ratings)))
      
      if(!is.null(user_ratings) & nrow(user_ratings) > 0) {
        
        user_ratings <- na.omit(user_ratings)
        #print(user_ratings)
        
        user_movie_ids <- user_ratings$MovieID
        user_movie_ratings <- user_ratings$Rating
        
        #evaluationScheme not used here...
        user_input = data.frame(UserID=0, MovieID=user_movie_ids, Rating = user_movie_ratings, Timestamp = NA)
        new.train <- rbind(user_input, filtered_top_rated_movies) # bring the user_input as top entry
        user_util_mat <- create_utility_matrix(new.train)
        
        rec_model <- create_ubcf_training(user_util_mat)
        
        recom = predict(rec_model, user_util_mat[1:1], n = 10,  type = 'topNList')
        recom_ratingMat = predict(rec_model, user_util_mat[1:1], n = 10,  type = 'ratingMatrix')
        
        recom_ratingMatList = as(recom_ratingMat,'list')
        #print(recom_ratingMatList)
        
        best_10 = as(bestN(recom, n = 10), 'list')
        #print(best_10)
        
        user_predicted_ids = suppressWarnings(as.numeric(sub('.', '', (best_10[[1]]))))
        final_filtered_movies <- movies[movies$MovieID %in% user_predicted_ids,] #TODO: preserve order
        
        user_results = (1:10)/10 # TODO: can be replaced by recom_ratingMat predictions but not really required here.
        recom_results <- data.table(Rank = 1:10, 
                                    MovieID = final_filtered_movies$MovieID, 
                                    Title = final_filtered_movies$Title, 
                                    Predicted_rating =  user_results)
        
        #print(head(final_filtered_movies))
        
        remove(rec_model, recom, user_predicted_ids, final_filtered_movies)
      
      } else {
        #TODO: Default values. Change to top rated movies for cold start.
        user_results = (1:10)/10
        user_predicted_ids = 1:10
        recom_results <- data.table(Rank = 1:10, 
                                    MovieID = filtered_top_rated_movies$MovieID[user_predicted_ids], 
                                    Title = filtered_top_rated_movies$Title[user_predicted_ids], 
                                    Predicted_rating =  user_results)
      }
      
      #print(mem_used())
      #print(recom_results)
      
      recom_results
    }) # still busy
    
  }) # clicked on button
  
  
  # display the recommendations
  output$results <- renderUI({
    num_rows <- 2
    num_movies <- 5
    recom_result <- df()
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
            
            div(style = "text-align:center", 
                a(img(src = movies$image_url[recom_result$MovieID[(i - 1) * num_movies + j]], height = 150))
            ),
            div(style="text-align:center; font-size: 100%", 
                strong(movies$Title[recom_result$MovieID[(i - 1) * num_movies + j]])
            )
            
        )        
      }))) # columns
    }) # rows
    
  }) # renderUI function
  
}) # server function
