library(httr)
library(plyr)
library(dplyr)
library(magrittr)
library(ggplot2)
library(grid)
library(gridExtra)
library(tibble)

describe = function(metadata, results) {
    # description
    print(paste0("Your search for Airbnb properties in '",
                 metadata$passed.location, "' returned ",
                 prettyNum(metadata$num.listings, big.mark = ","),
                " listings. Your search term was recognized as a ",
                metadata$geography$result.type, ", specifically a ",
                metadata$geography$precision,
                ", which Airbnb resolved to the following location: ",
                metadata$geography$city, ", ", metadata$geography$state, ", ",
                metadata$geography$country, ". ",
                "The center of your search area is here: ",
                metadata$geography$google.maps.loc, ". A graphic detailing ",
                "characteristics of the listings returned by your search is ",
                "being saved in your working directory."))

    # messy plotting
    common.aes = aes(x = value, y = count)
    common.bar = geom.bar(stat = "identity", fill = "lightblue")
    common.percent.lab = geom.text(aes(label = scales::percent(round(count/metadata$num.listings, 2))), vjust = -.2)
    p1 = ggplot(metadata$facets$bedrooms, common.aes) + common.bar + common.percent.lab + labs(title = "Bedrooms", x = "", y = "Count") + scale.x.continuous(breaks = metadata$facets$bedrooms$value)
    p2 = ggplot(metadata$facets$beds, common.aes) + common.bar + common.percent.lab + labs(title = "Beds", x = "", y = "Count") + scale.x.continuous(breaks = metadata$facets$beds$value)
    p3 = ggplot(metadata$facets$bathrooms, common.aes) + common.bar + common.percent.lab + labs(title = "Bathrooms", x = "", y = "Count") + scale.x.continuous(breaks = metadata$facets$bathrooms$value)
    p4 = ggplot(metadata$facets$room.type, common.aes) + common.bar + common.percent.lab + labs(title = "Room Type", x = "", y = "Count") + scale.x.discrete(limits = metadata$facets$room.type$value)
    p5 = ggplot(metadata$facets$top.amenities, common.aes) + common.bar + common.percent.lab + labs(title = "Top Amenities", x = "", y = "Count") + scale.x.discrete(limits = metadata$facets$top.amenities$value)
    p6 = ggplot(metadata$facets$hosting.amenity.ids, aes(x = value, weight = (count/metadata$num.listings)*100)) + geom.bar(fill = "lightblue") + labs(title = "All Amenities", x = "", y = "Percent") + scale.x.discrete(limits = metadata$facets$hosting.amenity.ids$value) + coord.flip()
    p = grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 2, ncol = 3, top = textGrob(paste0("Characteristics of the ", prettyNum(metadata$num.listings, big.mark = ","),  " Listings in '", content$passed.location, "'"), gp = gpar(fontsize = 20)))
    ggsave(file = paste0(metadata$passed.location, ".pdf"), p, width = 16, height = 9)
}

describe(metadata, results)
