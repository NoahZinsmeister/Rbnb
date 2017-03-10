library(httr)
library(plyr)
library(dplyr)
library(magrittr)
library(ggplot2)
library(grid)
library(gridExtra)
library(tibble)

search.listings = function(location,
                           guests = 1,
                           ib = FALSE) {
    params = list(location = location,
                  guests = guests,
                  ib = ib,
                  min_bathrooms = 0,
                  min_bedrooms = 0,
                  min_beds = 0,
                  client_id = "d306zoyjsyarp7ifhu67rjxn52tv0t20",
                  locale = "en-US",
                  currency = "USD")
    #_format = "for_search_results")

    endpoint_url = "https://api.airbnb.com/v2/search_results"

    url_args = c()
    for (name in names(params))
        url_args = c(url_args, paste(name, params[[name]], sep = "="))
    url_args = paste(url_args, sep = "", collapse = "&")

    url = paste(endpoint_url, "?", url_args, sep = "")

    request = GET(url = url)
    if (request$status_code != 200) {
        bad_request = content(request, as = "parsed")
        stop(paste("\n\nError code: ", bad_request$error_code, "\nError: ", bad_request$error, "\nError Message: ", bad_request$error_message, sep =""))
    }
    c(passed_location = location, content(request, as = "parsed"))
}

parse.metadata = function(metadata, passed_location) {
    # geography info
    country = metadata$geography$country
    state = metadata$geography$state
    city = metadata$geography$city
    recognized_as = paste(metadata$geography$result_type, ": ", metadata$geography$precision, sep = "")
    #google_center = browseURL(paste("http://maps.google.com/maps?t=m&q=loc:", metadata$geography$lat, "+", metadata$geography$lng, sep = ""))
    print(paste(country, city, state, recognized_as, sep = ", "))

    #facet info
    facets = list()
    # "at least x" variables
    for (i in c("bedrooms", "bathrooms", "beds")) {
        facets[[i]] = lapply(metadata$facets[[i]], lapply, function(x) ifelse(is.null(x), NA, x)) %>%
                      bind_rows %>%
                      mutate(value = as.numeric(as.character(value))) %>%
                      select(value, count) %>%
                      arrange(value) %>%
                      rename(cumulative_count = count) %>%
                      mutate(count = cumulative_count - lead(cumulative_count, default = 0))
    }
    # factor variables
    for (i in c("room_type", "hosting_amenity_ids", "top_amenities")) {
        facets[[i]] = lapply(metadata$facets[[i]], lapply, function(x) ifelse(is.null(x), 0, x)) %>%
            bind_rows %>%
            mutate(value = as.character(value)) %>%
            select(value, count) %>%
            arrange(desc(count))
    }
    # messy plotting
    num_listings = sum(facets$bedrooms$count)
    p1 = ggplot(facets$bathrooms, aes(x = value, weight = count)) + geom_bar(fill = "coral2") + geom_text(aes(y = count, label = scales::percent(round(count/sum(count), 2))), vjust = -.25) + labs(title = "Bathrooms", x = "", y = "Count") + scale_x_continuous(breaks = facets$bathrooms$value)
    p2 = ggplot(facets$bedrooms, aes(x = value, weight = count)) + geom_bar(fill = "coral2") + geom_text(aes(y = count, label = scales::percent(round(count/sum(count), 2))), vjust = -.25) + labs(title = "Bedrooms", x = "", y = "Count") + scale_x_continuous(breaks = facets$bedrooms$value)
    p3 = ggplot(facets$beds, aes(x = value, weight = count)) + geom_bar(fill = "coral2") + geom_text(aes(y = count, label = scales::percent(round(count/sum(count), 2))), vjust = -.25) + labs(title = "Beds", x = "", y = "Count") + scale_x_continuous(breaks = facets$beds$value)
    p4 = ggplot(facets$room_type, aes(x = value, weight = (count/num_listings)*100)) + geom_bar(fill = "skyblue2") + labs(title = "Room Type", x = "", y = "Percent") + scale_x_discrete(limits = facets$room_type$value)
    p5 = ggplot(facets$top_amenities, aes(x = value, weight = (count/num_listings)*100)) + geom_bar(fill = "skyblue2") + labs(title = "Top Amenities", x = "", y = "Percent") + scale_x_discrete(limits = facets$top_amenities$value)
    p6 = ggplot(facets$hosting_amenity_ids, aes(x = value, weight = (count/num_listings)*100)) + geom_bar(fill = "skyblue2") + labs(title = "All Amenities", x = "", y = "Percent") + scale_x_discrete(limits = facets$hosting_amenity_ids$value) + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))
    p = grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 2, ncol = 3, top = textGrob(paste("Listing Characteristics for ", passed_location, "\n", city, ", ", state, sep = ""), gp = gpar(fontsize = 20)))
    ggsave(file = paste(passed_location, ".png", sep = ""), p, width = 16, height = 9)
}

c = search.listings(10019)
parse.metadata(c$metadata, c$passed_location)
