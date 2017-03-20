#' Predict a price for an Airbnb listing using the characteristics of similar listings.
#'
#' @description Given a listing ID, \code{predictPrice} uses the \code{xgboost} package to predict a price
#' for that listing based on its characteristics and data from nearby listings. A listing ID can be found
#' on the end of the URL for the listing on Airbnb's site. For example, a listing URL for Airbnb
#' looks like \code{https://www.airbnb.com/rooms/17634206?s=QAXAT6DD}. In this example, the listing
#' ID is "17634206." Passing this value as a character string will yield a price prediction based
#' on its characteristics, implying over- or undervaluation.
#'
#' @param listingID the ID that Airbnb has assigned to the listing. This can be taken from the end of a listing's
#' URL. This should be passed as a string (a character vector of length one).
#' @param maxSample The maximum number of nearby listings on which to base the price prediction. There is a
#' tradeoff-a higher value will increase prediction precision, but it will take longer to run.
#' @param nfold For advanced users. The number of cross validation folds used by \code{xgboost}.
#' @param nrounds For advanced users. The maximum number of rounds that \code{xgboost} will run
#' when finding the optimal number of rounds through cross validation.
#' @param early_stopping_rounds For advanced users. This parameter will cause xgboost to stop
#' training new models after the specified number of rounds if cross validation error does not improve.
#' @param max_depth For advanced users. The maximum depth of a tree fitted by \code{xgboost}.
#' @param eta For advanced users. The shrinkage parameter used to control the learning rate for
#' \code{xgboost}.
#' @param listing.detail a dataset for the particular listing if you've already pulled it using 
#' \code{listingDetails(listingID)}.
#' @param trainData a dataset consisting of listings with details if you've already pulled it 
#' using \code{searchLocation()} and \code{addDetails()}.
#'
#'
#' @export
#'
#' @importFrom xgboost xgb.DMatrix xgb.cv xgb.train
#' @importFrom dplyr filter select rename bind_rows left_join
#' @importFrom magrittr %>%
#' @importFrom utils capture.output
#' @importFrom stats predict
#'

predictPrice <- function(listingID,
                         maxSample=500,
                         nfold=10,
                         nrounds=1000,
                         early_stopping_rounds=50,
                         max_depth=4,
                         eta=0.1,
                         listing.detail=NULL,
                         trainData=NULL
                          ){

  if(missing(listing.detail) | missing(trainData)){
    listing.detail <- listingDetails(listingIDs=listingID)

    # Get similar geographical listings based on zipcode
    cat("Pulling Data for Similar Listings.\n")
    trainData <- searchLocation(location=listing.detail$zipcode) %>%
    {.$results$data}
    cat("Done!\n")

    # Take a random sample if the number of listings is above the cutoff
    if(nrow(trainData)>maxSample){
      sample <- sample(1:nrow(trainData),maxSample)
      trainData <- trainData[sample,]
    }

    # Merge in details of the listings -- this is unfortunately slow
    cat("Merging in the details for similar listings.\n")
    cat("This may take a while.\n")
    cat("Try reducing maxSample if it takes too long.\n")
    trainData <-listingDetailsFromList(trainData$id) %>%
    {mergeDetails(trainData,.)}
    cat("Done merging details!\n")
  }

  # Ensure that the listing of interest is in the dataset
  # If not, add it in for further processing
  if(!(listing.detail$id %in% trainData$id)){
    trainData <- dplyr::bind_rows(trainData,listing.detail)
  }

  # Keep variables on which to train XGBoost + id and price
  varlist <- c("id","price","lat","lng","bedrooms","beds","bathrooms","room.type",
               "instant.bookable","is.business.travel.ready","reviews.count",
               "cancellation.policy","min.nights","person.capacity","clean.fee.native",
               "star.rating","bed.type","property.type","primary.host.is.superhost",
               "primary.host.reviewee.count","primary.host.has.profile.pic",
               "primary.host.identity.verified"
               )
  # Keep out of this set those that are in the listing of interest and the other listings
  varlist <- varlist[varlist %in% names(listing.detail) & varlist %in% names(trainData)]

  trainData <- trainData[,names(trainData) %in% varlist | grepl("^amenity.",names(trainData))]

  # If an amentity is missing, assume that it doesn't have that amenity
  amenity.index <- vapply(names(trainData),grepl,FUN.VALUE=TRUE,pattern="^amenity")
  trainData[,amenity.index][is.na(trainData[,amenity.index])] <- FALSE

  # Convert character to numeric
  pred <- names(trainData)
  for (p in pred) {
    if (class(trainData[[p]])=="character" & p!="id") {
      if(suppressWarnings(all(!is.na(as.numeric(trainData[[p]]))))){
        trainData[[p]] <- as.numeric(trainData[[p]])
      }else{
        levels <- unique(trainData[[p]])
        trainData[[p]] <- as.integer(factor(trainData[[p]], levels=levels))
      }
    }
  }

  # Train on everything but the user's own listing
  testData <- dplyr::filter(trainData,id==listingID)
  trainData <- dplyr::filter(trainData,id!=listingID)

  # Dependent variable = price
  y <- trainData$price

  # Get rid of ID var for prediction, and price for test set
  testData <- dplyr::select(testData,-id,-price)
  trainData <- dplyr::select(trainData,-id,-price)

  matrix <- data.matrix(trainData)
  ## Create xgboost dataset
  dx <- xgboost::xgb.DMatrix(data.matrix(trainData),label=y,missing=NaN)

  ## Train the model
  cat("Starting Prediction.\n")
  utils::capture.output(xgb.model <- xgboost::xgb.cv(data=dx,
                                              objective='reg:linear',
                                              nfold=nfold,
                                              early_stopping_rounds = early_stopping_rounds,
                                              nrounds=nrounds,
                                              max_depth=max_depth,
                                              eta=eta,
                                              maximize=FALSE))

  # Train the optimal model
  opt.model <- xgboost::xgb.train(data=dx,
                                  nrounds=xgb.model$best_iteration,
                                  max_depth=max_depth,
                                  eta=eta)
  dtest <- xgboost::xgb.DMatrix(as.matrix(testData),missing=NaN)

  # predict for test set
  price.hat <- stats::predict(opt.model,dtest)

  cat(paste("The predicted price based on similar listings is ",format(price.hat,digits=2),"\n",sep=""))
  pDif <- (price.hat-listing.detail[["price"]])/listing.detail[["price"]]
  cat(paste("This represents a ",format(100*abs(pDif),digits=2),"%",ifelse(pDif>0," premium"," discount")," to the current listing price.\n",sep=""))

  list(listingID=listingID,
       currentPrice=listing.detail[["price"]],
       predictedPrice=price.hat,
       trainingData=trainData,
       listingData=testData)
}
