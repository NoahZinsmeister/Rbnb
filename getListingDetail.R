library(httr)
library(plyr)
library(dplyr)
library(magrittr)
library(tibble)
# from httr RETRY content

listingDetailsFromList <- function(listingIDs,
                              client_id = "d306zoyjsyarp7ifhu67rjxn52tv0t20") {
  results <- bind_rows(lapply(unique(listingIDs),
                     function(x) getListingDetail(x, client_id = client_id)))
  lapply(names(results),FilterVars)
  
  results
}

mergeDetails <- function(searchResults,details){
  # Filter out overlapping variables form the search results
  FilterVars <- function(i) {
    if( 
      ## Redundant variables also in listing search
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
  left_join(searchResults,details,by="id")
}


getListingDetail <- function(listingID,
                             client_id = "d306zoyjsyarp7ifhu67rjxn52tv0t20") {
  params = list(client_id=client_id,
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
    bind_rows()
  
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
