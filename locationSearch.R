library(httr)
library(plyr)
library(dplyr)
library(magrittr)
library(ggplot2)
library(grid)
library(gridExtra)
library(tibble)

# write a metadata thing with just metadata
# graphing metadata function
#

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

searchLocation = function(location,
                          verbose = TRUE,
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
                  price_max = 2000,
                  sort = 1,
                  "_format" = "for_search_results",
                  "_limit" = 1,
                  "_offset" = 0)
    endpoint.url = "https://api.airbnb.com/v2/search_results"

    # make an exploratory call, only return 1 listing
    request = RETRY("GET", url = constructGET(endpoint.url, params))
    checkRequest(request)
    primary.results = content(request, as = "parsed")

    # check number of listings returned for the location
    num.listings = primary.results$metadata$listings_count

    # if 0 listings, stop
    if (num.listings == 0)
        stop("No results found. Try a different search term.")

    # else, figure out how many max prices cutoffs we need to get x or fewer listings between cutoffs
    # NOTE: this is because if num.listings is over 1000, it means that there are more results
    # than we're able to get by manipulating limit and offset, hence using price cutoffs
    get.num.listings = function(p.low, p.high) {
        # gets number of listings in [p.low, p.high-1]
        params$price_min = p.low
        params$price_max = p.high-1
        request = RETRY("GET", url = constructGET(endpoint.url, params))
        checkRequest(request)
        parsed.results = content(request, as = "parsed")
        # return number of listings returned
        parsed.results$metadata$listings_count
    }

    # starting cutoffs [0-2000]
    price.cutoffs = c(0, 2001)

    # make cutoffs more granular until we get the proper amount
    while (any(num.listings > 250)) {
        # between which cutoffs are there most listings?
        max.index = which.max(num.listings)
        # split that range in half to get new cutoff
        new.cutoff = as.integer(mean(c(price.cutoffs[max.index], price.cutoffs[max.index+1])))
        if (new.cutoff %in% price.cutoffs)
            stop("Couldn't find sufficiently granular price cutoffs to return all results.")
        # insert new cutoff
        price.cutoffs = append(price.cutoffs, new.cutoff, after = max.index)
        # add in number of listings for new cutoffs
        num.listings = append(num.listings, c(get.num.listings(price.cutoffs[max.index], price.cutoffs[max.index+1]),
                                              get.num.listings(price.cutoffs[max.index+1], price.cutoffs[max.index+2])),
                              after = max.index)
        # and remove the old cutoff
        num.listings = num.listings[-max.index]
    }

    #if >2500 listings, warn
    if (sum(num.listings) > 2500) {
        warning(paste0(sum(num.listings), " listings found. Performance may be impaired."))
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

        # check how many calls we need to make
        finished.iterations = FALSE

        # make the necessary calls, saving results only (not metadata) at each step
        while (!finished.iterations) {
            request = RETRY("GET", url = constructGET(endpoint.url, params))
            checkRequest(request)
            parsed.results = content(request, as = "parsed")
            all.results = c(all.results, parsed.results$search_results)
            params$`_offset` = parsed.results$metadata$pagination$next_offset
            if (parsed.results$metadata$pagination$result_count < 50) finished.iterations = TRUE
        }
        params$`_offset` = 0
    }

    # return the list of all results, metadata from last call, and passed location
    list(passed.location = location,
         metadata = primary.results$metadata,
         results = all.results)
}

parseMetadata = function(content) {
    # geography info
    geography = list(city = content$metadata$geography$city,
                     state = content$metadata$geography$state,
                     country = content$metadata$geography$country,
                     result.type = content$metadata$geography$result.type,
                     precision = content$metadata$geography$precision,
                     lat = content$metadata$geography$lat,
                     lng = content$metadata$geography$lng)
    geography$google.maps.loc = paste0("http://maps.google.com/maps?t=m&q=loc:",
                                       geography$lat, "+", geography$lng)
    # facet info
    facets = list()
    # "at least x" variables
    for (i in c("bedrooms", "bathrooms", "beds")) {
        facets[[i]] = lapply(content$metadata$facets[[i]], lapply,
                             function(x) ifelse(is.null(x), 0, x)) %>%
                      bind_rows %>%
                      mutate(value = as.numeric(as.character(value))) %>%
                      select(value, count) %>%
                      arrange(value) %>%
                      rename(cumulative.count = count) %>%
                      mutate(count = cumulative.count - lead(cumulative.count,
                                                             default = 0))
    }
    # factor variables
    for (i in c("room_type", "hosting_amenity_ids", "top_amenities",
                "languages")) {
        facets[[i]] = lapply(content$metadata$facets[[i]], lapply,
                             function(x) ifelse(is.null(x), 0, x)) %>%
            bind_rows %>%
            mutate(value = as.character(value)) %>%
            select(value, count) %>%
            arrange(desc(count))
    }
    list(passed.location = content$passed.location,
         num.listings = content$num.listings,
         geography = geography,
         facets = facets)
}

parseResults = function(content) {
    bind_rows(lapply(content$results,
                     function(x) bind_rows(as.list(unlist(x)))))
}

location = "10019"
content = searchLocation(location)

rename.vars = c("listing.name" = "listing.name", "listing.id" = "id", "listing.bedrooms" = "bedrooms",
                "listing.beds" = "beds", "listing.bathrooms" = "bathrooms", "listing.person.capacity" = "capacity",
                "listing.city" = "city", "listing.public.address" = "public.address",
                "listing.lat" = "lat", "listing.lng" = "lng", "listing.property.type" = "property.type",
                "listing.room.type" = "room.type", "listing.star.rating" = "rating",
                "listing.primary.host.first.name" = "host.name", "listing.primary.host.id" = "host.id",
                "listing.instant.bookable" = "instant.book",
                "listing.is.business.travel.ready" = "business.travel.ready", "listing.picture.count" = "num.pictures",
                "listing.reviews.count" = "num.reviews", "pricing.quote.guests" = "price.quote.num.guests",
                "pricing.quote.localized.currency" = "price.quote.currency", "pricing.quote.localized.nightly.price" = "price.quote.price",
                "pricing.quote.localized.service.fee" = "price.quote.fee")
# want to put this in but not defined for all places..."listing.neighborhood" = "neighborhood"

result = parseResults(content)
metadata = parseMetadata(content)
