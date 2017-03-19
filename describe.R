library(httr)
library(plyr)
library(dplyr)
library(magrittr)
library(ggplot2)
library(grid)
library(gridExtra)
library(tibble)
library(ggmap)

describeMetadata = function(content) {
    metadata = content$metadata
    # description
    print(paste0("Airbnb provided some metadata on ", metadata$num.listings,
                 " for your search term '", content$passed.location,
                 ". Your search term was recognized as a ",
                metadata$geography$result.type, ", specifically a ",
                metadata$geography$precision,
                ", which Airbnb resolved to the following location: ",
                metadata$geography$city, ", ", metadata$geography$state, ", ",
                metadata$geography$country, ". ",
                "The center of your search area is here: ",
                metadata$geography$google.maps.loc, "."))

    # messy plotting
    common.aes = ggplot2::aes(x = value, y = count)
    common.bar = ggplot2::geom_bar(stat = "identity", fill = "#99ccff")
    common.percent.lab = ggplot2::geom_text(ggplot2::aes(label = scales::percent(round(count/metadata$num.listings, 2))), vjust = -.2)
    p1 = ggplot(metadata$facets$bedrooms, common.aes) + common.bar + common.percent.lab + ggplot2::labs(title = "Bedrooms", x = "", y = "Count") + ggplot2::scale_x_continuous(breaks = metadata$facets$bedrooms$value)
    p2 = ggplot(metadata$facets$beds, common.aes) + common.bar + common.percent.lab + ggplot2::labs(title = "Beds", x = "", y = "Count") + ggplot2::scale_x_continuous(breaks = metadata$facets$beds$value)
    p3 = ggplot(metadata$facets$bathrooms, common.aes) + common.bar + common.percent.lab + ggplot2::labs(title = "Bathrooms", x = "", y = "Count") + ggplot2::scale_x_continuous(breaks = metadata$facets$bathrooms$value)
    p4 = ggplot(metadata$facets$room_type, common.aes) + common.bar + common.percent.lab + ggplot2::labs(title = "Room Type", x = "", y = "Count") + ggplot2::scale_x_discrete(limits = metadata$facets$room_type$value)
    p5 = ggplot(metadata$facets$top_amenities, common.aes) + common.bar + common.percent.lab + ggplot2::labs(title = "Top Amenities", x = "", y = "Count") + ggplot2::scale_x_discrete(limits = metadata$facets$top_amenities$value)
    p6 = ggplot(metadata$facets$hosting_amenity_ids, aes(x = value, weight = (count/metadata$num.listings)*100)) + ggplot2::geom_bar(fill = "#99ccff") + ggplot2::labs(title = "All Amenities", x = "", y = "Percent") + ggplot2::scale_x_discrete(limits = metadata$facets$hosting_amenity_ids$value) + ggplot2::coord_flip()
    gridExtra::grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 2, ncol = 3, top = grid::textGrob(paste0("Characteristics of the ", metadata$num.listings, " Listings in '", content$passed.location, "'"), gp = gpar(fontsize = 20)))
    #ggplot2::ggsave(file = paste0(metadata$passed.location, ".pdf"), p, width = 16, height = 9)
}



describeResults = function(content) {
    data = content$results$data %>% mutate_each_(funs(as.numeric), c("pricing_quote.rate.amount"))

    # description
    print(paste0("Airbnb identified ", content$results$num.listings,
                 " listings in the search area for ", content$passed.location,
                 ". Your search term was recognized as a ",
                 metadata$geography$result.type, ", specifically a ",
                 metadata$geography$precision,
                 ", which Airbnb resolved to the following location: ",
                 metadata$geography$city, ", ", metadata$geography$state, ", ",
                 metadata$geography$country, ". ",
                 "The center of your search area is here: ",
                 metadata$geography$google.maps.loc, "."))

    # messy plotting
    common.aes = ggplot2::aes(x = pricing_quote.rate.amount, fill = listing.room_type)
    common.hist = ggplot2::geom_histogram(binwidth = 25, boundary = 0)
    ggplot2::ggplot(data, common.aes) + common.hist + ggplot2::labs(title = "Prices", x = "", y = "Count")
}

showMap = function(content) {
    tbl = content$results$data %>% mutate_each_(funs(as.numeric), c("listing.lng", "listing.lat", "listing.bedrooms", "pricing_quote.rate.amount"))
    map <- ggmap::get_map(location = c(left = min(tbl$listing.lng),
                                       bottom = min(tbl$listing.lat),
                                       right = max(tbl$listing.lng),
                                       top = max(tbl$listing.lat)),
                          maptype = "roadmap", source = "google", color = "bw", messaging = FALSE)
    ggmap::ggmap(map) +
        ggplot2::geom_point(data = tbl, ggplot2::aes(x = listing.lng, y = listing.lat, size = listing.bedrooms, color = pricing_quote.rate.amount), alpha = .75) +
        ggplot2::scale_color_gradient(low = "#99ccff", high = "#000099", guide = "colorbar")
}

describeMetadata(content)
describeResults(content)
showMap(content)
