library(httr)
library(plyr)
library(dplyr)
library(magrittr)
library(ggplot2)
library(grid)
library(gridExtra)
library(tibble)

construct.GET = function(base_url, parameters) {
    url_args = c()
    for (name in names(parameters))
        url_args = c(url_args, paste(name, parameters[[name]], sep = "="))
    url_args = paste(url_args, sep = "", collapse = "&")

    paste(base_url, "?", url_args, sep = "")
}

search.listings = function(location) {
    # default search parameters
    params = list(location = gsub(" ", "%20", location),
                  guests = 1,
                  ib = FALSE,
                  min_bathrooms = 0,
                  min_bedrooms = 0,
                  min_beds = 0,
                  client_id = "d306zoyjsyarp7ifhu67rjxn52tv0t20",
                  locale = "en-US",
                  currency = "USD",
                  "_format" = "for_search_results",
                  "_limit" = 1,
                  "_offset" = 0)

    endpoint_url = "https://api.airbnb.com/v2/search_results"

    request = GET(construct.GET(endpoint_url, params))
    parsed_results = content(request, as = "parsed")

    if (request$status_code != 200)
        stop(paste("Airbnb's API returned an error.\n\n",
                   "Error code: ", parsed_results$error_code,
                   "\nError: ", parsed_results$error,
                   "\nError Message: ", parsed_results$error_message, sep =""))

    num_listings = parsed_results$metadata$listings_count

    if (num_listings == 0)
        stop(paste("No results found. Try a different search term."))
    else {
        required_iterations = ceiling(num_listings/50)
        params[["_limit"]] = 50
    }

    all_results = list()
    for (i in 1:required_iterations) {
        request = GET(construct.GET(endpoint_url, params))
        parsed_results = content(request, as = "parsed")
        # this hopefully won't ever happen, but might because of a timeout or something. need to thnik about this more..
        if (request$status_code != 200)
            stop(paste("Airbnb's API returned an error.\n\n",
                       "Error code: ", parsed_results$error_code,
                       "\nError: ", parsed_results$error,
                       "\nError Message: ", parsed_results$error_message, sep =""))
        all_results = c(all_results, parsed_results$search_results)
        params[["_offset"]] = params[["_offset"]] + 50
    }

    # return the list of all results, metadata from last call, passed location, and number of listings
    list(passed_location = location, results = all_results, metadata = parsed_results$metadata, num_listings = num_listings)
}

parse.metadata = function(content) {
    # geography info
    geography = list(country = content$metadata$geography$country,
                     state = content$metadata$geography$state,
                     city = content$metadata$geography$city,
                     result_type = content$metadata$geography$result_type,
                     precision = content$metadata$geography$precision,
                     google_maps_coords = paste("http://maps.google.com/maps?t=m&q=loc:", content$metadata$geography$lat, "+", content$metadata$geography$lng, sep = ""))
    # facet info
    facets = list()
    # "at least x" variables
    for (i in c("bedrooms", "bathrooms", "beds")) {
        facets[[i]] = lapply(content$metadata$facets[[i]], lapply, function(x) ifelse(is.null(x), NA, x)) %>%
                      bind_rows %>%
                      mutate(value = as.numeric(as.character(value))) %>%
                      select(value, count) %>%
                      arrange(value) %>%
                      rename(cumulative_count = count) %>%
                      mutate(count = cumulative_count - lead(cumulative_count, default = 0))
    }
    # factor variables
    for (i in c("room_type", "hosting_amenity_ids", "top_amenities")) {
        facets[[i]] = lapply(content$metadata$facets[[i]], lapply, function(x) ifelse(is.null(x), 0, x)) %>%
            bind_rows %>%
            mutate(value = as.character(value)) %>%
            select(value, count) %>%
            arrange(desc(count))
    }
    list(geography = geography, facets = facets, num_listings = content$num_listings, passed_location = content$passed_location)
}

parse.results = function(content) {
    bind_rows(lapply(content$results, function(x) bind_rows(as.list(unlist(x)))))
}

describe = function(metadata, results) {
    # description
    print(paste("Your search for Airbnb properties in '", metadata$passed_location, "' returned ", prettyNum(metadata$num_listings, big.mark = ","),
                " listings. Your search term was recognized as a ", metadata$geography$result_type, ", specifically a ", metadata$geography$precision,
                ", which Airbnb resolved to the following location: ", metadata$geography$city, ", ", metadata$geography$state, ", ", metadata$geography$country, ". ",
                "The center of your search area, according to Airbnb, is here: ", metadata$geography$google_maps_coords,
                ". A graphic detailing characteristics of the listings returned by your search is being saved in your working directory.", sep = ""))

    # messy plotting
    common_aes = aes(x = value, y = count, weight = (count/metadata$num_listings)*100)
    common_bar = geom_bar(stat = "identity", fill = "lightblue")
    common_percent_lab = geom_text(aes(label = scales::percent(round(count/metadata$num_listings, 2))), vjust = -.2)
    p1 = ggplot(metadata$facets$bedrooms, common_aes) + common_bar + common_percent_lab + labs(title = "Bedrooms", x = "", y = "Count") + scale_x_continuous(breaks = metadata$facets$bedrooms$value)
    p2 = ggplot(metadata$facets$beds, common_aes) + common_bar + common_percent_lab + labs(title = "Beds", x = "", y = "Count") + scale_x_continuous(breaks = metadata$facets$beds$value)
    p3 = ggplot(metadata$facets$bathrooms, common_aes) + common_bar + common_percent_lab + labs(title = "Bathrooms", x = "", y = "Count") + scale_x_continuous(breaks = metadata$facets$bathrooms$value)
    p4 = ggplot(metadata$facets$room_type, common_aes) + common_bar + common_percent_lab + labs(title = "Room Type", x = "", y = "Percent") + scale_x_discrete(limits = metadata$facets$room_type$value)
    p5 = ggplot(metadata$facets$top_amenities, common_aes) + common_bar + common_percent_lab + labs(title = "Top Amenities", x = "", y = "Percent") + scale_x_discrete(limits = metadata$facets$top_amenities$value)
    p6 = ggplot(metadata$facets$hosting_amenity_ids, aes(x = value, weight = (count/metadata$num_listings)*100)) + geom_bar( fill = "lightblue") + labs(title = "All Amenities", x = "", y = "Percent") + scale_x_discrete(limits = metadata$facets$hosting_amenity_ids$value) + coord_flip()
    p = grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 2, ncol = 3, top = textGrob(paste("Characteristics of the ", prettyNum(metadata$num_listings, big.mark = ","),  " Listings in '", content$passed_location, "'", sep = ""), gp = gpar(fontsize = 20)))
    ggsave(file = paste(metadata$passed_location, ".png", sep = ""), p, width = 16, height = 9)
}

location = "13035"
content = search.listings(location)
metadata = parse.metadata(content)
results = parse.results(content) %>%
          select(-starts_with("listing.xl_picture_urls"), -starts_with("listing.picture_urls"))
describe(metadata, results)
