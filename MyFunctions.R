#b) sunartisi omoiotitas
compare_vectors <- function(train_row, test_row) {
  #remove NAs
  train_rem_na <- train_row %>% select(-which(is.na(train_row)))
  test_rem_na <- test_row %>% select(-which(is.na(test_row)))
  
  #columns that appear in both
  common_columns <- intersect(colnames(train_rem_na), colnames(test_rem_na))
  
  #select columns that are in common columns
  train_clean <- train_rem_na %>% select(common_columns)
  test_clean <- test_rem_na %>% select(common_columns)
  
  #save the row as vector to compare
  train_vector <- as.numeric(train_clean[1,])
  test_vector <- as.numeric(test_clean[1,])
  
  #compare vectors
  result <- cor(train_vector, test_vector,  method = "pearson", use = "complete.obs")
  
  return(result)
}

calculate_similarities <- function(){
  similarities <- data.frame(reader= character(), NU1 = numeric(), NU2 = numeric())
  colLen <- ncol(train_data)
  rowLen <- nrow(train_data)
  
   #loop of train_data, compare nu1 and nu2
  for(row in 1:rowLen){
    
    #to create similarities dataset
    reader <- as.character(train_data[row,1]) 
    nu1 <-  as.character(test_data[1,1])
    nu2 <- as.character(test_data[2,2])
    
    #get row
    train_row <- train_data[row,2:colLen]
    test_row_nu1 <- test_data[1,2:colLen]
    test_row_nu2 <- test_data[2,2:colLen]
    
    res_nu1 <- compare_vectors(train_row, test_row_nu1)
    res_nu2 <- compare_vectors(train_row, test_row_nu2)
    
    similarities[row,] <- c(reader,res_nu1,res_nu2)
  }
  
  return(similarities)
}

#d) upologismos plisiesterou geitona
get_k_nearest <- function(i,k_NN){
  NU <- i+1
  sim_sorted <- sim[order(sim[,NU], decreasing = TRUE),]
  top_similarities <- as.numeric(sim_sorted[1:3,NU])
  readers <- as.character(sim_sorted[1:3,1])
  
  test <- train_data[train_data$TRAIN_READER %in% c(readers[1],readers[2],readers[3]),]
  test <- test %>% mutate(similarities = c(top_similarities[1],top_similarities[2],top_similarities[3]))
  
  return(test)
}

#e) times diavathmisis
calculate_predictions <- function(i, k_near){
  colnames(k_near) <- NULL
  res <- c()
  
  #vector of similarity 
  sim <- unlist(k_near[1:3,10])
  len <- ncol(k_near) -1
  
  #for each book sum caclulate the similarity
  for(col in 2:len){
    #get list of ratings
    ratings <- unlist(k_near[1:3,col])
    sum <- 0
    divider <- 0
    
    for(pos in 1:3){
      if(!is.na(ratings[pos])){
        mul <- 0
        mul <- ratings[pos] * sim[pos]
        sum <- sum + mul

        divider <- divider + sim[pos]
      }
    }
    
    res <- append(res,sum / divider)
  }
  
  return(res)
}

#st) synartisi gia NAs
spot_the_NAs <- function(i){
  NAs <- c()
  row <- test_data[i,]
  for(col in 2:ncol(row)){
    if(is.na(row[1,col])){
      NAs <- append(NAs,col -1)
    }
  }
  
  return(NAs)
}

#z) sunartisi gia sustaseis sta NAs
calculate_recommendations <- function(i, need_recommendation, predictions, recommendations){
  colnames(test_data) <- NULL
  temp <- unlist(test_data[1:2,1])
  NU <- temp[i]
  
  for(i in 1:length(need_recommendation)){
    item <- need_recommendation[i]
    new_row <- data.frame(reader = NU, book = books[item], ranking = predictions[item])
    recommendations <- rbind(recommendations,new_row)
    
  }

  return(recommendations)
}

#h) mea