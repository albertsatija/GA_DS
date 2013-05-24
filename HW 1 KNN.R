# this is a function to run a knn test and cross validate it 'y' times
# optimal k neighbors I found was 34 and that produced an error rate of 27.5% 


knnvalidation <- function () 
{
# run a knn
  max.k <- 50  # 1000 breaks it
  totalerr.rates <- matrix(,max.k,1)
  labels <-german.table$classification
  german.table$classification <- NULL
  set.seed(1234)
  N <- nrow(german.table)

  for (x in 1:5) {                ## the 5 was originally 'y' but I changed it to a constant for trouble-shooting        
    train.pct <- .7
    train.index <- sample(1:N, train.pct * N)
    train.data <- german.table[train.index, ]
    test.data <- german.table[-train.index, ]
    train.labels <- as.factor(as.matrix(labels)[train.index, ])
    test.labels <- as.factor(as.matrix(labels)[-train.index, ])
    err.rates <- data.frame()
    

    # perform fit for various values of k
    for (k in 1:max.k) {
      knn.fit <- knn(
        train = train.data,         # training set
        test = test.data,           # test set
        cl = train.labels,          # true labels
        k = k                       # number of NN to poll
      )

      # print params and confusion matrix for each value k
      cat('\n', 'k = ', k, ', train.pct = ', train.pct, '\n', sep='')
      print(table(test.labels, knn.fit))

      # store generalation error and append to total results
      this.err <- sum(test.labels != knn.fit) / length(test.labels)
      err.rates <- rbind(err.rates, this.err)
    }
    
    # creating a datafrane with the err-rates per k for each of my cross validation
    
    totalerr.rates <- cbind(totalerr.rates,err.rates)
  }
  
  totalerr.rates[,1] <- NULL    
  avgerr.rates <- apply(totalerr.rates, 1, mean) # create a vector with the average error for each k

  ## OUTPUT RESULTS

  results <- data.frame(1:max.k, avgerr.rates)   # create results summary data frame
  names(results) <- c('k', 'err.rate')        # label columns of results df

  # create title for results plot
  title <- paste('knn results (train.pct = ', train.pct, ')', sep='')

  # create results plot
  results.plot <- ggplot(results, aes(x=k, y=avgerr.rates)) + geom_point() + geom_line()
  results.plot <- results.plot + ggtitle(title)

  # draw results plot
  results.plot

  # optimal k neighbors I found was 34 and that produced an error rate of 27.5% 
  #  I callled avgerr.rates to find the exact lowest

}



