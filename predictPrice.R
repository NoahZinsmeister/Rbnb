library(xgboost)
library(dplyr)

# from dplyr filter select rename bind_rows left_join
# from xgboost xgb.DMatrix xgb.train predict


predictPrice <- function(listingID,
                         maxSample=500,
                         nfold=10,
                         nrounds=1000,
                         early.stop.round=50,
                         max_depth=6
                         ){
  
  listing.detail <- getListingDetail(listingID=listingID)
  
  # Get similar geographical listings based on zipcode
  trainData <- searchLocation(location=listing.detail$zipcode) %>%
      {.$results$data}
  
  trainData <- dplyr::rename(trainData,id=listing.id)

  # Take a random sample if the number of listings is above the cutoff
  if(nrow(trainData)>maxSample){
    sample <- sample(1:nrow(trainData),maxSample)
    trainData <- trainData[sample,]
  }
  
  # Merge in details of the listings -- this is unfortunately slow
  trainData <-listingDetailsForMerge(trainData$id) %>%
                  {left_join(trainData,.,by="id")}
                    
  # Ensure that the listing of interest is in the dataset
  # If not, add it in for further processing
  if(!(listing.detail$id %in% trainData$id)){
    trainData <- dplyr::bind_rows(trainData,listing.detail)
  }

  # Keep variables on which to train XGBoost
  trainData <- dplyr::select(trainData,
                             id,
                             price,
                             starts_with("amenity."),
                             lat,
                             lng,
                             bedrooms,
                             beds,
                             bathrooms,
                             room.type,
                             instant.bookable,
                             is.business.travel.ready,
                             reviews.count,
                             hosts.identity.verified,
                             hosts.is.superhost,
                             hosts.reviewee.count,
                             cancellation.policy,
                             min.nights,
                             person.capacity,
                             cleaning.fee.native,
                             star.rating
                          )
  
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
  
  # Get rid of ID var for prediction
  testData <- dplyr::select(testData,-id)
  trainData <- dplyr::select(trainData,-id)
  
  ## Dependent variable = price
  y <- trainData$price
  
  matrix <- data.matrix(trainData)
  ## Create xgboost dataset
  dx <- xgboost::xgb.DMatrix(data.matrix(trainData),label=y,missing=NaN)
  
  ## Train the model
  xgb.model <- xgboost::xgb.cv(data=dx,
                               objective='reg:linear',
                               nfold=nfold,
                               early.stop.round = early.stop.round,
                               nrounds=nrounds,
                               max_depth=max_depth,
                               maximize=FALSE)

  # Take the best model (lowest CV error)
  best.n <- min(which(xgb.model$test.rmse.mean==min(xgb.model$test.rmse.mean)))
  
  # Train the optimal model
  opt.model <- xgboost::xgb.train(data=dx,
                                  nrounds=best.n)
  dtest <- xgboost::xgb.DMatrix(as.matrix(testData))
  
  # predict for test set
  price.hat <- xgboost::predict(opt.model,dtest)
  price.hat
}
