##
## File written by noahwz
##

#' Search for Airbnb listings by location.
#'
#' @description Given an input value, lets Airbnb parse it, and return listings
#' in the same region.
#'
#' @param location a string representing the desired search region.
#' @param verbose a boolean indicating whether or not to print status updates.
#' @param metadata.only a boolean indicating whether or not to return just
#' metadata (no listings).
#' @param client.id a string represting your own Airbnb API key.
#' @return named list containing the serach output.
#' @export
#'
#' @importFrom httr RETRY
#' @importFrom magrittr %>%
#' @importFrom dplyr bind_rows mutate select arrange rename lead mutate_at funs
#'
#' @examples
#' searchLocation("Peoria IL")
#'
searchLocation = function(location,
                          verbose = TRUE,
                          metadata.only = FALSE,
                          client.id = "d306zoyjsyarp7ifhu67rjxn52tv0t20") {
    # default search parameters
    params = list(location = location,
                  guests = 1,
                  ib = FALSE,
                  min_bathrooms = 0,
                  min_bedrooms = 0,
                  min_beds = 0,
                  client_id = client.id,
                  locale = "en-US",
                  currency = "USD",
                  price_min = 0,
                  price_max = 2500,
                  sort = 1,
                  "_format" = "for_search_results",
                  "_limit" = 1,
                  "_offset" = 0)
    endpoint.url = "https://api.airbnb.com/v2/search_results"

    # make an exploratory call, only return 1 listing
    request = httr::RETRY("GET", url = constructGET(endpoint.url, params))
    checkRequest(request)
    primary.results = content(request, as = "parsed")

    # check number of listings returned for the location
    num.listings = primary.results$metadata$listings_count

    # if 0 listings, stop
    if (num.listings == 0)
        stop("No results found. Try a different search term.")

    if (metadata.only)
        return(list(passed.location = location,
                    metadata = parseMetadata(primary.results$metadata)))

    # else, figure out how many max prices cutoffs we need to get x or fewer listings between cutoffs
    # NOTE: this is because if num.listings is over 1000, it means that there are more results
    # than we're able to get by manipulating limit and offset, hence using price cutoffs
    getNumListings = function(p.low, p.high) {
        # gets number of listings in [p.low, p.high-1]
        params$price_min = p.low
        params$price_max = p.high-1
        request = httr::RETRY("GET", url = constructGET(endpoint.url, params))
        checkRequest(request)
        parsed.results = content(request, as = "parsed")
        # return number of listings returned
        parsed.results$metadata$listings_count
    }

    # starting cutoffs [0-2500]
    price.cutoffs = c(0, 2501)

    # make cutoffs more granular until we have bins of <300 listings
    while (any(num.listings > 300)) {
        # between which cutoffs are there most listings?
        max.index = which.max(num.listings)
        # split that range in half to get new cutoff
        new.cutoff = as.integer(mean(c(price.cutoffs[max.index], price.cutoffs[max.index+1])))
        if (new.cutoff %in% price.cutoffs)
            stop("Couldn't find sufficiently granular price cutoffs to return all results.")
        # insert new cutoff
        price.cutoffs = append(price.cutoffs, new.cutoff, after = max.index)
        # add in number of listings for new cutoffs
        num.listings = append(num.listings, c(getNumListings(price.cutoffs[max.index], price.cutoffs[max.index+1]),
                                              getNumListings(price.cutoffs[max.index+1], price.cutoffs[max.index+2])),
                              after = max.index)
        # and remove the old cutoff
        num.listings = num.listings[-max.index]
    }

    #if >2000 listings, warn
    if (sum(num.listings) > 2000) {
        warning(paste0(sum(num.listings), " listings found. Performance may be impaired."), immediate. = TRUE)
    } else {
        if (verbose) print(paste0(sum(num.listings), " listings found! Retrieving data..."))
    }

    # for every pair of price cutoffs, load the data in, given max chunk size of 50
    all.results = list()
    params$`_limit` = 50
    for (i in 1:length(num.listings)) {
        if (num.listings[i] == 0) next
        if (verbose) print(paste0("Batch ", i, " of ", length(num.listings)))
        params$price_min = price.cutoffs[i]
        params$price_max = price.cutoffs[i+1]-1

        # make the necessary calls, saving results only (not metadata) at each step
        repeat {
            request = httr::RETRY("GET", url = constructGET(endpoint.url, params))
            checkRequest(request)
            parsed.results = content(request, as = "parsed")
            all.results = c(all.results, parsed.results$search_results)
            params$`_offset` = parsed.results$metadata$pagination$next_offset
            if (parsed.results$metadata$pagination$result_count < 50) break
        }
        params$`_offset` = 0
    }

    # return the list of all results, metadata from last call, and passed location
    list(passed.location = location,
         metadata = parseMetadata(primary.results$metadata),
         results = list(num.listings = length(all.results),
                        data = parseResults(all.results)))
}

constructGET = function(base.url, parameters) {
    # first, replace all spaces with %20
    parameters = lapply(parameters, function(x) gsub(" ", "%20", x))
    # append param names and values into a string like: name=value&...
    url.args = paste(names(parameters), parameters, sep = "=", collapse = "&")
    # return the appropriate url call
    paste0(base.url, "?", url.args)
}

checkRequest = function(request) {
    # if the request wasn't successful, stop
    if (request$status_code != 200)
        stop(paste0("Airbnb's API returned an error.\n\n",
                    paste(names(content(request, as = "parsed")),
                          content(request, as = "parsed"),
                          sep = ": ", collapse = "\n")))
}

parseMetadata = function(metadata) {
    # geography info
    geography = list(city = metadata$geography$city,
                     state = metadata$geography$state,
                     country = metadata$geography$country,
                     result.type = metadata$geography$result_type,
                     precision = metadata$geography$precision,
                     lat = metadata$geography$lat,
                     lng = metadata$geography$lng)
    geography$google.maps.loc = paste0("http://maps.google.com/maps?t=m&q=loc:",
                                       geography$lat, "+", geography$lng)
    # facet info
    facets = list()
    # "at least x" variables
    for (i in c("bedrooms", "bathrooms", "beds")) {
        facets[[i]] = lapply(metadata$facets[[i]], lapply, function(x) ifelse(is.null(x), 0, x)) %>%
                      dplyr::bind_rows(.) %>%
                      dplyr::mutate(value = as.numeric(as.character(value))) %>%
                      dplyr::select(value, count) %>%
                      dplyr::arrange(value) %>%
                      dplyr::rename(cumulative.count = count) %>%
                      dplyr::mutate(count = cumulative.count - dplyr::lead(cumulative.count, default = 0))
    }
    num.listings = facets$bedrooms$cumulative.count[1]
    # factor variables
    for (i in c("room_type", "hosting_amenity_ids", "top_amenities")) {
        facets[[i]] = lapply(metadata$facets[[i]], lapply,
                             function(x) ifelse(is.null(x), 0, x)) %>%
            dplyr::bind_rows(.) %>%
            dplyr::mutate(value = as.character(value)) %>%
            dplyr::select(value, count) %>%
            dplyr::arrange(dplyr::desc(count))
    }
    list(num.listings = num.listings,
         geography = geography,
         facets = facets)
}

parseResults = function(results) {
    results <- dplyr::bind_rows(lapply(results, function(x) dplyr::bind_rows(as.list(unlist(x)))))

    filterResults <- function(i) {
      if(grepl(pattern="image",x=i) |
         grepl(pattern="url",x=i) |
         grepl(pattern="photo",x=i) |
         grepl(pattern="png",x=i) |
         grepl(pattern="scrim",x=i) |
         grepl(pattern="listing.user.id",x=i)
      ){
        results[i] <<- NULL
      }
    }
    lapply(names(results),filterResults)

    #remove listing prefix
    remPref <- function(name,pref){
      gsub(paste("^",pref,sep=""),"",name)
    }
    names(results) <- lapply(names(results),remPref,pref="listing.")

    # replace underscores with periods
    names(results) <- lapply(names(results),gsub,pattern="_",replacement=".") %>%
                        unlist()

    # change class of certain vars to numeric
    numericList <- c("bathrooms","beds","bedrooms","lat","lng","picture.count",
                     "person.capacity","reviews.count","star.rating","pricing.quote.guests",
                     "pricing.quote.guest.details.number.of.adults","pricing.quote.localized.nightly.price",
                     "pricing.quote.localized.service.fee","pricing.quote.localized.total.price",
                     "pricing.quote.long.term.discount.amount.as.guest","pricing.quote.nightly.price",
                     "pricing.quote.service.fee","pricing.quote.total.price")

    # make sure the vars are in the dataset
    numericList <- numericList[numericList %in% names(results)]

    results <- dplyr::mutate_at(.tbl=results,.cols=numericList, dplyr::funs("as.numeric"))

    results
}

