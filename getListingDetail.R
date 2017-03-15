library(httr)
library(plyr)
library(dplyr)
library(magrittr)
library(tibble)
# from httr RETRY content

getListingDetail <- function(listing_id,
                              client_id = "d306zoyjsyarp7ifhu67rjxn52tv0t20") {
  params = list(client_id=client_id,
                "_format"="v1_legacy_for_p3",
                locale = "en-US"
  )
  endpoint_url = paste("https://api.airbnb.com/v2/listings/",as.character(listing_id),sep="")
  request = httr::RETRY("GET", url = construct.GET(endpoint_url, params))
  
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
  
  filter_vars <- function(i) {
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
      listing.details[gsub(pattern=" ",replacement="_",x=listing.details[i],)] <<- TRUE
      listing.details[i] <<- NULL
    }
  }
  lapply(names(listing.details),filter_vars)
  
  listing.details
}