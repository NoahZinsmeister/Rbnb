#' Search for Airbnb listings by location.
#'
#' @description Lets Airbnb parse a given input location and return listings
#' in that location.
#'
#' @param location a character string representing the desired search region.
#' @param verbose a boolean indicating whether or not to print status updates.
#' @param metadata.only a boolean indicating whether or not to return just
#' metadata (no listings).
#' @param client.id a string represting your own Airbnb API key. Ours has been
#' provided for convenience.
#' @return named list containing various search outputs.
#' @export
#'
#' @importFrom curl curl_fetch_memory new_pool curl_fetch_multi multi_run
#' @importFrom jsonlite fromJSON
#' @importFrom magrittr %>%
#' @importFrom dplyr bind_rows mutate select arrange rename lead mutate_at funs
#'
#' @examples
#' searchLocation("IRS Regional Examination Center, Peoria, IL")
#'
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
                  price_max = 5000,
                  sort = 1,
                  "_format" = "for_search_results",
                  "_limit" = 1,
                  "_offset" = 0)
    endpoint.url = "https://api.airbnb.com/v2/search_results"

    # make an exploratory call, only return 1 listing
    call = curl::curl_fetch_memory(constructGET(endpoint.url, params)) %>%
        parseRequest(subset="both")

    # if 0 listings returned, stop
    num.listings = call$metadata$num.listings
    if (call$metadata$num.listings == 0)
        stop("No results found. Try a different search term.")

    if (metadata.only)
        return(list(passed.location = location,
                    metadata = call$metadata))

    # else, figure out how many max prices cutoffs we need to get x or fewer
    # listings between cutoffs NOTE: this is because if num.listings is over
    # 1000, it means that there are more results than we're able to get by
    # manipulating limit and offset, hence using price cutoffs
    getNumListings = function(p.low, p.high) {
        # gets number of listings in [p.low, p.high-1]
        params$price_min = p.low
        params$price_max = p.high-1
        request = curl_fetch_memory(constructGET(endpoint.url, params)) %>%
            parseRequest(subset="metadata")
        # return number of listings returned
        request$num.listings
    }

    # starting cutoffs [0-price_max]
    price.cutoffs = c(0, params$price_max+1)

    # make cutoffs more granular until we have bins of <=300 listings
    while (any(num.listings > 300)) {
        # between which cutoffs are there most listings?
        max.index = which.max(num.listings)
        # split that range in half to get new cutoff
        new.cutoff = as.integer(mean(c(price.cutoffs[max.index], price.cutoffs[max.index+1])))
        if (new.cutoff %in% price.cutoffs)
            stop("Couldn't find sufficiently granular price cutoffs to return all results.")
        # insert new cutoff
        price.cutoffs = append(price.cutoffs, new.cutoff, after=max.index)
        # add in number of listings for new cutoffs
        num.listings = append(num.listings, c(getNumListings(price.cutoffs[max.index], price.cutoffs[max.index+1]),
                                              getNumListings(price.cutoffs[max.index+1], price.cutoffs[max.index+2])),
                              after = max.index)
        # and remove the old cutoff
        num.listings = num.listings[-max.index]
    }

    # possible to get cutoffs with 0 listings in between, fix that
    while (any(num.listings == 0)) {
        zero.index = which.min(num.listings)
        num.listings = num.listings[-zero.index]
        price.cutoffs = price.cutoffs[-zero.index]
    }

    # if >2000 listings, warn
    if (sum(num.listings) > 2000) {
        warning(paste0(sum(num.listings), " listings found. Performance may be impaired. Retrieving data..."),
                immediate.=TRUE)
    } else {
        if (verbose) print(paste0(sum(num.listings), " listings found! Retrieving data..."))
    }

    # for every pair of price cutoffs, load the data in, given max chunk size of 50
    params$`_limit` = 50
    offsets = lapply(num.listings, function(x) seq(0, x-1, 50))
    pool <- curl::new_pool()
    out = NULL
    for (i in 1:length(offsets)) {
        params$price_min = price.cutoffs[i]
        params$price_max = price.cutoffs[i+1]-1
        for (j in offsets[[i]]) {
            params$`_offset` = j
            curl::curl_fetch_multi(constructGET(endpoint.url, params), done=function(x) out<<-bind_rows(out, parseRequest(x)), pool=pool)
        }
    }
    outcome = curl::multi_run(pool=pool)

    # return the list of all results, metadata from first call (- the num.listings), and passed location
    call$metadata$num.listings = NULL
    list(passed.location = location,
         metadata = call$metadata,
         results = out)
}

constructGET = function(base.url, parameters) {
    # first, replace all spaces with %20
    parameters = lapply(parameters, function(x) gsub(" ", "%20", x))
    # append param names and values into a string like: name=value&...
    url.args = paste(names(parameters), parameters, sep = "=", collapse = "&")
    # return the appropriate url call
    paste0(base.url, "?", url.args)
}

parseRequest = function(request, subset="results") {
    content = checkSuccessfulreturnContent(request) %>%
        rawToChar %>%
        jsonlite::fromJSON(simplifyDataFrame=TRUE, flatten=TRUE)

    #parse and return results/metadata separately
    if (subset=="results")
        return(parseResults(content$search_results))
    else if (subset=="metadata")
        return(parseMetadata(content$metadata))
    else
        return(list(results=parseResults(content$search_results),
                    metadata=parseMetadata(content$metadata)))
}

checkSuccessfulreturnContent = function(request) {
    # note: this is where we could check header content if we weren't lazy...

    # if the request wasn't successful, stop
    if (request$status_code != 200) {
        error=jsonlite::fromJSON(rawToChar(request$content))
        stop(paste0("API error at ", request$url, "\n\n",
                    paste(names(error), error, sep = ": ", collapse = "\n")))
    }
    else
        return(request$content)
}

parseMetadata = function(metadata) {
    # this is where we take everything we want from metadata

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
    num.listings = metadata$listings_count
    # factor variables
    for (i in c("room_type", "hosting_amenity_ids", "top_amenities")) {
        facets[[i]] = metadata$facets[[i]] %>%
            dplyr::select(value, count) %>%
            dplyr::arrange(dplyr::desc(count))
    }
    list(num.listings=num.listings,
         geography=geography,
         facets=facets)
}

parseResults = function(results) {
    # this is where we take everything we want from results

    results =  as.tbl(results)

    filterResults = function(i) {
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
    lapply(names(results), filterResults)

    #remove listing prefix
    remPref <- function(name, pref){
      gsub(paste("^", pref, sep=""), "", name)
    }
    names(results) = lapply(names(results), remPref, pref="listing.")

    # replace underscores with periods
    names(results) = lapply(names(results), gsub, pattern="_",
                            replacement=".") %>%
        unlist()

    # change class of certain vars to numeric
    numericList <- c("bathrooms", "beds","bedrooms", "lat", "lng",
                     "picture.count", "person.capacity", "reviews.count",
                     "star.rating", "pricing.quote.guests",
                     "pricing.quote.guest.details.number.of.adults",
                     "pricing.quote.localized.nightly.price",
                     "pricing.quote.localized.service.fee",
                     "pricing.quote.localized.total.price",
                     "pricing.quote.long.term.discount.amount.as.guest",
                     "pricing.quote.nightly.price",
                     "pricing.quote.service.fee",
                     "pricing.quote.total.price")

    # make sure the vars are in the dataset
    numericList <- numericList[numericList %in% names(results)]

    results <- dplyr::mutate_at(.tbl=results, .cols=numericList, dplyr::funs("as.numeric"))

    return(results)
}
