##
## File written by noahwz
##

#' Describe characteristics of Airbnb listings.
#'
#' @description Given output from \code{\link{searchLocation}}, produces
#' summary charts based on metadata and results.
#' @rdname describe
#' @name describe
#' @param content The output of \code{\link{searchLocation}}.
#'
#' @importFrom ggplot2 ggplot aes geom_bar geom_text geom_histogram
#' geom_point labs scale_x_continuous scale_x_discrete scale_color_gradient
#' @importFrom ggmap ggmap
#' @importFrom scales percent
#' @importFrom gridExtra grid.arrange
#' @importFrom grid textGrob gpar
#'
NULL

#' @rdname describe
#' @export
#'
describeMetadata = function(content) {
    metadata = content$metadata

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
    gridExtra::grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 2, ncol = 3, top = grid::textGrob(paste0("Characteristics of the ", metadata$num.listings, " Listings in '", content$passed.location, "'"), gp = grid::gpar(fontsize = 20)))
    #ggplot2::ggsave(file = paste0(metadata$passed.location, ".pdf"), p, width = 16, height = 9)
}

#' @rdname describe
#' @export
#'
describeResults = function(content) {
    data = content$results$data

    # messy plotting
    common.aes = ggplot2::aes(x = pricing.quote.nightly.price, fill = room.type)
    common.hist = ggplot2::geom_histogram(binwidth = 25, boundary = 0)
    ggplot2::ggplot(data, common.aes) + common.hist + ggplot2::labs(title = "Prices", x = "", y = "Count")
}

#' @rdname describe
#' @export
#'
showMap = function(content) {
    map <- ggmap::get_map(location = c(left = min(content$results$data$lng),
                                       bottom = min(content$results$data$lat),
                                       right = max(content$results$data$lng),
                                       top = max(content$results$data$lat)),
                          maptype = "roadmap", source = "google", color = "bw", messaging = FALSE)
    ggmap::ggmap(map) +
        ggplot2::geom_point(data = content$results$data, ggplot2::aes(x = lng, y = lat, size = bedrooms, color = pricing.quote.nightly.price), alpha = .75) +
        ggplot2::scale_color_gradient(low = "#99ccff", high = "#000099", guide = "colorbar")
}

