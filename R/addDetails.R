#' Pull in detailed listing data to merge with location search data.
#'
#' @description After using the \code{searchLocation}, you can add greater detail on each listing
#' using \code{addDetails}. It takes as an input the dataset of listings outputted from
#' \code{searchLocation} and merges in details based on the listing ID. Note that this process
#' is very time consuming, and so it may be unwise to pass too large of a listing dataset.
#'
#' @param searchData a dataset of listings outputted from \code{searchLocation}.
#'
#' @export
#'
#' @importFrom httr RETRY content
#' @importFrom dplyr bind_rows mutate_at left_join
#' @importFrom magrittr %>%
#'
#' @name addDetails
NULL

##
## Written by Kroeger
##
addDetails <- function(searchData){
  combined <-listingDetails(searchData$id) %>%
  {mergeDetails(searchData,.)}
}

#' @rdname addDetails
#'
#' @description The following is a function that will take in a character vector of
#' listing IDs and return a dataset. This function does not necessarily need to be used
#' with \code{locationSearch}.
#'
#' @param listingIDs a character vector of listing IDs. These can be found on the URLs
#' of Airbnb listings or on the output of \code{searchLocation}.
#' @param client.id best left alone. This is a key that authorizes requests to the Airbnb API.
#'
#'
#'
#' @export
#'
listingDetails <- function(listingIDs,
                           client.id = "d306zoyjsyarp7ifhu67rjxn52tv0t20") {
  results <- dplyr::bind_rows(lapply(unique(listingIDs),
                                     function(x) getListingDetail(x, client.id = client.id)))

  # remove extra user. prefix
  names(results) <- lapply(names(results),
                           function(name,pref){gsub(paste("^",pref,sep=""),"",name)},
                           pref="user.")
  # remove listing. prefix
  names(results) <- lapply(names(results),
                           function(name,pref){gsub(paste("^",pref,sep=""),"",name)},
                           pref="listing.")
  # Certain vars are trivial duplicates once the prefix is removed
  # Take only unique vars
  results <- results[,unique(names(results))]

  # Change class of numeric vars
  numericList <- c("lat","lng","price","price.native","user.reviewee.count","bathrooms",
                   "bedrooms","beds","hosts.reviewee.count","min.nights","person.capacity",
                   "primary.host.reviewee.count","reviews.count","cleaning.fee.native",
                   "max.nights","security.deposit.native","security.price.native",
                   "price.for.extra.person.native","guests.included","star.rating",
                   "weekend.price.native","monthly.price.native","weekly.price.native",
                   "square.feet")

  # make sure the vars are in the dataset
  numericList <- numericList[numericList %in% names(results)]

  results <- dplyr::mutate_at(.tbl=results,.cols=numericList,funs("as.numeric"))

  results
}

#' @rdname addDetails
#'
#' @description The following is a function that will take in a character vector of
#' listing IDs and return a dataset. This function does not necessarily need to be used
#' with \code{locationSearch}.
#'
#' @param searchResults a dataset of listings that is outputted from a call to
#' \code{searchLocation}.
#' @param details a dataset of detailed listing information that is outputted from a
#' call to \code{listingDetails}
#'
#'
mergeDetails <- function(searchResults,details){
  # Filter out overlapping variables form the search results
  FilterVars <- function(i) {
    if(
      # Redundant variables also in listing search
      i=="room.type" |
      i=="lat" |
      i=="lng" |
      i=="property.type" |
      i=="bedrooms" |
      i=="bathrooms" |
      i=="beds" |
      i=="city" |
      i=="instant.bookable" |
      i=="is.business.travel.ready" |
      i=="localized.city" |
      i=="person.capacity" |
      i=="primary.host.has.profile.pic" |
      i=="primary.host.id" |
      i=="primary.host.is.superhost" |
      i=="property.type.id" |
      i=="public.address" |
      i=="reviews.count" |
      i=="room.type.category" |
      i=="star.rating" |
      i=="user.id"
    ){
      searchResults[i] <<- NULL
    }
  }
  lapply(names(searchResults),FilterVars)
  dplyr::left_join(searchResults,details,by="id")
}

#' @rdname addDetails
#'
#' @description The following is a function that will take in a character vector of
#' listing IDs and return a dataset. This function does not necessarily need to be used
#' with \code{locationSearch}.
#'
#' @param listingID a character vector of length one consisting of an Airbnb listing ID.
#' These can be found on the URLs of Airbnb listings or on the output of \code{searchLocation}.
#' @param client.id best left alone. This is a key that authorizes requests to the Airbnb API.
#'
#' @importFrom magrittr %>%
getListingDetail <- function(listingID,
                             client.id = "d306zoyjsyarp7ifhu67rjxn52tv0t20") {
  params = list(client_id=client.id,
                "_format"="v1_legacy_for_p3",
                locale = "en-US"
  )
  endpoint_url = paste("https://api.airbnb.com/v2/listings/",as.character(listingID),sep="")
  request = httr::RETRY("GET", url = constructGET(endpoint_url, params))

  # if the request wasn't successful, stop
  if (request$status_code != 200)
    stop(paste0("Airbnb's API returned an error.\n\n",
                paste(names(parsed_results), parsed_results,
                      sep = ": ", collapse = "\n")))

  listing.details <- httr::content(request,as="parsed") %>%
    .$listing %>%
    unlist() %>%
    as.list() %>%
    dplyr::bind_rows(.)

  ## Function to get rid of unwanted variables (image URLS, formatting preferences, etc.)
  FilterVars <- function(i) {
    if(grepl(pattern="image",x=i) |
       grepl(pattern="picture",x=i) |
       grepl(pattern="scrim",x=i) |
       grepl(pattern="encoded_png",x=i)|
       grepl(pattern="thumbnail",x=i) |
       grepl(pattern="url",x=i) |
       grepl(pattern="name",x=i) |
       grepl(pattern="amenities_id",x=i) |
       grepl(pattern="photo",x=i)
    ){
      listing.details[i] <<- NULL
    }
    if(grepl(pattern="^amenities[0-9]*$",x=i)){
      listing.details[paste("amenity.",gsub(pattern=" ",replacement=".",x=listing.details[i]),sep="")] <<- TRUE
      listing.details[i] <<- NULL
    }
  }
  lapply(names(listing.details),FilterVars)

  names(listing.details) <- lapply(names(listing.details),gsub,pattern="_",replacement=".") %>%
    unlist()

  listing.details
}
