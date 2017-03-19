library(httr)
library(plyr)
library(dplyr)
library(magrittr)
library(ggplot2)
library(grid)
library(gridExtra)
library(tibble)

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
                  price_max = 2000,
                  sort = 1,
                  "_format" = "for_search_results_with_minimal_pricing",
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
        request = RETRY("GET", url = constructGET(endpoint.url, params))
        checkRequest(request)
        parsed.results = content(request, as = "parsed")
        # return number of listings returned
        parsed.results$metadata$listings_count
    }

    # starting cutoffs [0-2000]
    price.cutoffs = c(0, 2001)

    # make cutoffs more granular until we have bins of <250 listings
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
        num.listings = append(num.listings, c(getNumListings(price.cutoffs[max.index], price.cutoffs[max.index+1]),
                                              getNumListings(price.cutoffs[max.index+1], price.cutoffs[max.index+2])),
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

        # make the necessary calls, saving results only (not metadata) at each step
        repeat {
            request = RETRY("GET", url = constructGET(endpoint.url, params))
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

parseMetadata = function(metadata) {
    # geography info
    geography = list(city = metadata$geography$city,
                     state = metadata$geography$state,
                     country = metadata$geography$country,
                     result.type = metadata$geography$result.type,
                     precision = metadata$geography$precision,
                     lat = metadata$geography$lat,
                     lng = metadata$geography$lng)
    geography$google.maps.loc = paste0("http://maps.google.com/maps?t=m&q=loc:",
                                       geography$lat, "+", geography$lng)
    # facet info
    facets = list()
    # "at least x" variables
    for (i in c("bedrooms", "bathrooms", "beds")) {
        facets[[i]] = lapply(metadata$facets[[i]], lapply,
                             function(x) ifelse(is.null(x), 0, x)) %>%
                      bind_rows %>%
                      mutate(value = as.numeric(as.character(value))) %>%
                      select(value, count) %>%
                      arrange(value) %>%
                      rename(cumulative.count = count) %>%
                      mutate(count = cumulative.count - lead(cumulative.count,
                                                             default = 0))
    }
    num.listings = facets$bedrooms$cumulative.count[1]
    # factor variables
    for (i in c("room_type", "hosting_amenity_ids", "top_amenities",
                "languages")) {
        facets[[i]] = lapply(metadata$facets[[i]], lapply,
                             function(x) ifelse(is.null(x), 0, x)) %>%
            bind_rows %>%
            mutate(value = as.character(value)) %>%
            select(value, count) %>%
            arrange(desc(count))
    }
    list(num.listings = num.listings,
         geography = geography,
         facets = facets)
}

parseResults = function(results) {
    tbl = bind_rows(lapply(results, function(x) bind_rows(as.list(unlist(x)))))

    # to do...
    rename.vars = c("listing.name" = "listing.name", "listing.id" = "id", "listing.bedrooms" = "bedrooms",
                    "listing.beds" = "beds", "listing.bathrooms" = "bathrooms", "listing.person.capacity" = "capacity",
                    "listing.city" = "city", "listing.public.address" = "public.address",
                    "listing.neighborhood" = "neighborhood", "listing.lat" = "lat", "listing.lng" = "lng",
                    "listing.property.type" = "property.type",
                    "listing.room.type" = "room.type", "listing.star.rating" = "rating",
                    "listing.primary.host.first.name" = "host.name", "listing.primary.host.id" = "host.id",
                    "listing.instant.bookable" = "instant.book",
                    "listing.is.business.travel.ready" = "business.travel.ready", "listing.picture.count" = "num.pictures",
                    "listing.reviews.count" = "num.reviews", "pricing.quote.guests" = "price.quote.num.guests",
                    "pricing.quote.localized.currency" = "price.quote.currency", "pricing.quote.localized.nightly.price" = "price.quote.price",
                    "pricing.quote.localized.service.fee" = "price.quote.fee")
    tbl
}


location = "10019"
content = searchLocation(location)
