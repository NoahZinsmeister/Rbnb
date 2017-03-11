library(httr)
library(plyr)
library(dplyr)
library(magrittr)
library(ggplot2)
library(grid)
library(gridExtra)
library(tibble)

construct.GET = function(base_url, parameters) {
    # append parameter names and values into a string like: name=value&...
    url_args = c()
    for (name in names(parameters))
        url_args = c(url_args, paste(name, parameters[[name]], sep = "="))
    url_args = paste0(url_args, collapse = "&")
    # return the appropriate url call
    paste0(base_url, "?", url_args)
}

search.location = function(location,
                           client_id = "d306zoyjsyarp7ifhu67rjxn52tv0t20") {
    # default search parameters for maximal results.
    params = list(location = gsub(" ", "%20", location),
                  guests = 1,
                  ib = FALSE,
                  min_bathrooms = 0,
                  min_bedrooms = 0,
                  min_beds = 0,
                  client_id = client_id,
                  locale = "en-US",
                  currency = "USD",
                  "_format" = "for_search_results",
                  "_limit" = 50,
                  "_offset" = 0)
    
    # make an exploratory call, only return 1 listing
    endpoint_url = "https://api.airbnb.com/v2/search_results"
    request = RETRY("GET", url = construct.GET(endpoint_url, params))
    parsed_results = content(request, as = "parsed")

    # if the request wasn't successful, stop
    if (request$status_code != 200)
        stop(paste0("Airbnb's API returned an error.\n\n",
                    paste(names(parsed_results), parsed_results,
                          sep = ": ", collapse = "\n")))
    
    # if it was, check how many listings are returned for the location
    num_listings = parsed_results$metadata$listings_count
    # if 0 listings, stop
    if (num_listings == 0)
        stop(paste("No results found. Try a different search term."))
    #if >1000 listings, stop
    if (num_listings > 1000) {
        print(paste("Warning: >1000 results found. Try being more specific."))
        num_listings = 1000
    }
    # check how many calls we need to make
    max_chunk_size = 50
    required_iterations = ceiling(num_listings/max_chunk_size)
    params$`_limit` = max_chunk_size
    
    # make the necessary calls, saving results only (not metadata) at each step
    all_results = list()
    for (i in 1:required_iterations) {
        request = RETRY("GET", url = construct.GET(endpoint_url, params))
        parsed_results = content(request, as = "parsed")
        if (request$status_code != 200)
            stop(paste0("Airbnb's API returned an error on iteration ", i,
                        ".\n\n", paste(names(parsed_results), parsed_results,
                                      sep = ": ", collapse = "\n")))
        all_results = c(all_results, parsed_results$search_results)
        # might want to use this later
        #parsed_results$metadata$pagination$next_offset
        params$`_offset` = params$`_offset` + params$`_limit`
        print(paste0("Completed iteration ", i, " of ", required_iterations))
    }

    # return the list of all results, metadata from last call, passed location, and number of listings
    list(passed_location = location,
         num_listings = num_listings,
         metadata = parsed_results$metadata,
         results = all_results)
}

parse.metadata = function(content) {
    # geography info
    geography = list(city = content$metadata$geography$city,
                     state = content$metadata$geography$state,
                     country = content$metadata$geography$country,
                     result_type = content$metadata$geography$result_type,
                     precision = content$metadata$geography$precision,
                     lat = content$metadata$geography$lat,
                     lng = content$metadata$geography$lng)
    geography$google_maps_loc = paste0("http://maps.google.com/maps?t=m&q=loc:",
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
                      rename(cumulative_count = count) %>%
                      mutate(count = cumulative_count - lead(cumulative_count,
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
    list(passed_location = content$passed_location,
         num_listings = content$num_listings,
         geography = geography,
         facets = facets)
}

parse.results = function(content) {
    bind_rows(lapply(content$results,
                     function(x) bind_rows(as.list(unlist(x)))))
}

describe = function(metadata, results) {
    # description
    print(paste0("Your search for Airbnb properties in '",
                 metadata$passed_location, "' returned ",
                 prettyNum(metadata$num_listings, big.mark = ","),
                " listings. Your search term was recognized as a ",
                metadata$geography$result_type, ", specifically a ",
                metadata$geography$precision,
                ", which Airbnb resolved to the following location: ",
                metadata$geography$city, ", ", metadata$geography$state, ", ",
                metadata$geography$country, ". ",
                "The center of your search area is here: ",
                metadata$geography$google_maps_loc, ". A graphic detailing ",
                "characteristics of the listings returned by your search is ",
                "being saved in your working directory."))

    # messy plotting
    common_aes = aes(x = value, y = count)
    common_bar = geom_bar(stat = "identity", fill = "lightblue")
    common_percent_lab = geom_text(aes(label = scales::percent(round(count/metadata$num_listings, 2))), vjust = -.2)
    p1 = ggplot(metadata$facets$bedrooms, common_aes) + common_bar + common_percent_lab + labs(title = "Bedrooms", x = "", y = "Count") + scale_x_continuous(breaks = metadata$facets$bedrooms$value)
    p2 = ggplot(metadata$facets$beds, common_aes) + common_bar + common_percent_lab + labs(title = "Beds", x = "", y = "Count") + scale_x_continuous(breaks = metadata$facets$beds$value)
    p3 = ggplot(metadata$facets$bathrooms, common_aes) + common_bar + common_percent_lab + labs(title = "Bathrooms", x = "", y = "Count") + scale_x_continuous(breaks = metadata$facets$bathrooms$value)
    p4 = ggplot(metadata$facets$room_type, common_aes) + common_bar + common_percent_lab + labs(title = "Room Type", x = "", y = "Count") + scale_x_discrete(limits = metadata$facets$room_type$value)
    p5 = ggplot(metadata$facets$top_amenities, common_aes) + common_bar + common_percent_lab + labs(title = "Top Amenities", x = "", y = "Count") + scale_x_discrete(limits = metadata$facets$top_amenities$value)
    p6 = ggplot(metadata$facets$hosting_amenity_ids, aes(x = value, weight = (count/metadata$num_listings)*100)) + geom_bar(fill = "lightblue") + labs(title = "All Amenities", x = "", y = "Percent") + scale_x_discrete(limits = metadata$facets$hosting_amenity_ids$value) + coord_flip()
    p = grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 2, ncol = 3, top = textGrob(paste0("Characteristics of the ", prettyNum(metadata$num_listings, big.mark = ","),  " Listings in '", content$passed_location, "'"), gp = gpar(fontsize = 20)))
    ggsave(file = paste0(metadata$passed_location, ".pdf"), p, width = 16, height = 9)
}

location = "60615"
content = search.location(location)
metadata = parse.metadata(content)
result = parse.results(content) %>%
          select(-starts_with("listing.xl_picture_urls"),
                 -starts_with("listing.picture_urls"))
describe(metadata, results)
