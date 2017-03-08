library(httr)
library(plyr)
library(dplyr)
library(magrittr)
library(ggplot2)
library(grid)
library(gridExtra)

search_listings = function(location,
                            guests = 1,
                            ib = FALSE,
                            min_bathrooms = 1,
                            min_bedrooms = 1,
                            min_beds = 1) {
    params = list(location = location,
                  guests = as.character(guests),
                  ib = tolower(ib),
                  min_bathrooms = as.character(min_bathrooms),
                  min_bedrooms = as.character(min_bedrooms),
                  min_beds = as.character(min_beds),
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
    content(request, as = "parsed")
}

testing_random_stuff = function(c) {
    # geography info
    country = c$metadata$geography$country
    state = c$metadata$geography$state
    city = c$metadata$geography$city
    recognized_as = paste(c$metadata$geography$result_type, ": ", c$metadata$geography$precision, sep = "")
    google_center = browseURL(paste("http://maps.google.com/maps?t=m&q=loc:", c$metadata$geography$lat, "+", c$metadata$geography$lng, sep = ""))
    print(paste(country, city, state, recognized_as, sep = ", "))

    #facet info
    facets = list()
    # "at least x" variables
    for (i in c("bedrooms", "bathrooms", "beds")) {
        facets[[i]] = as.tbl(do.call(rbind, lapply(c$metadata$facets[[i]], data.frame))) %>%
                               mutate(value = as.numeric(as.character(value))) %>%
                               select(value, count) %>%
                               arrange(value) %>%
                               rename(cumulative_count = count) %>%
                               mutate(count = cumulative_count - lead(cumulative_count, default = 0))
    }
    # factor variables
    for (i in c("room_type", "hosting_amenity_ids", "top_amenities")) {
        facets[[i]] = as.tbl(do.call(rbind, lapply(c$metadata$facets[[i]], data.frame))) %>%
            mutate(value = as.character(value)) %>%
            select(value, count) %>%
            arrange(desc(count))
    }
    # messy plotting, colors don't seem to be working?
    p1 = ggplot(facets$bathrooms) + geom_bar(aes(x = value, weight = count, fill = "blue"), show.legend = FALSE) + ggtitle("Bathrooms") + scale_x_continuous(breaks = facets$bathrooms$value)
    p2 = ggplot(facets$bedrooms) + geom_bar(aes(x = value, weight = count, fill = "#56B4E9"), show.legend = FALSE) + ggtitle("Bedrooms") + scale_x_continuous(breaks = facets$bedrooms$value)
    p3 = ggplot(facets$beds) + geom_bar(aes(x = value, weight = count, fill = "#6D9DE5"), show.legend = FALSE) + ggtitle("Beds") + scale_x_continuous(breaks = facets$beds$value)
    p4 = ggplot(facets$room_type) + geom_bar(aes(x = value, weight = count, fill = "#6D9DE5"), show.legend = FALSE) + ggtitle("Room Type") + scale_x_discrete(limits = facets$room_type$value)
    p5 = ggplot(facets$hosting_amenity_ids) + geom_bar(aes(x = value, weight = count, fill = "#6D9DE5"), show.legend = FALSE) + ggtitle("All Amenities") + scale_x_discrete(limits = facets$hosting_amenity_ids$value) + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))
    p6 = ggplot(facets$top_amenities) + geom_bar(aes(x = value, weight = count, fill = "#6D9DE5"), show.legend = FALSE) + ggtitle("Top Amenities") + scale_x_discrete(limits = facets$top_amenities$value)
    grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 2, ncol = 3, top = textGrob(paste("Listing Characteristics for ", city, ", ", state, sep = ""), gp = gpar(fontsize = 20)))
}

c = search_listings(60615)
