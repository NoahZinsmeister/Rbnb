-   [Introduction](#introduction)
-   [Installation](#installation)
-   [Searching by Location](#searching-by-location)
    -   [Metadata](#metadata)
    -   [Results](#results)
-   [Listing Details](#listing-details)
-   [Price Prediction](#price-prediction)

Introduction
============

Rbnb is an experimental front end for the (unofficial) Airbnb API. In addition to pulling detailed data on Airbnb listings based on arbitrary user-defined locations, Rbnb also provides a variety of visual summaries of these listing.

Use cases include, but are not limited to:

-   Prospective Airbnb guests and hosts who want to get a sense of the Airbnb market in their target location.
-   Current Airbnb hosts who wish to see how the price of their existing listing compares to similiar listings in their area.
-   Researchers interesting in analyzing granual microdata on short-term rental markets.

We, the authors (Alexander Kroeger and Noah Zinsmeister), are grateful to [The Official Unofficial Airbnb API Docs](airbnbapi.org) for documenting much of the Airbnb API.

Installation
============

Before we get started, let's install Rbnb.

``` r
library(devtools)
install_github("NoahZinsmeister/Rbnb")
```

Searching by Location
=====================

The first thing you may want to do after installing Rbnb is to try searching for listings in your current zip code. For example, one of us lives in 10019, a zip that includes Hell's Kitchen and midtown Manhattan. Let's pull listings data for this area.

``` r
library(Rbnb)

location = 10019
content = Rbnb::searchLocation(location, verbose=FALSE)
```

Note: if you are unable to access the internet or the Airbnb API specifically, we've included a static pull of the 10019 data (`10019.RDS`) that you can load in like so (in this example we'll be using the static pull):

``` r
content = data(10019, package=Rbnb)
```

And here's what we have!

``` r
str(content, max.level = 1)
```

    ## List of 3
    ##  $ passed.location: chr "60611"
    ##  $ metadata       :List of 3
    ##  $ results        :List of 2

As you can see, `searchLocation` returns a list with three elements: -passed.location -metadata -results

Let's take a closer look at these last two elements.

Metadata
--------

Airbnb spits out some interesting metadata when given a location.

``` r
str(content$metadata, max.level = 1)
```

    ## List of 3
    ##  $ num.listings: int 373
    ##  $ geography   :List of 8
    ##  $ facets      :List of 7

In our example, Airbnb provided metadata on **373** listings for your search term '**60611**'. Your search term was recognized as a **postal\_code**, specifically a **zip**, which Airbnb resolved to the following location: **Chicago**, **Illinois**, **United States**. We can even pull up the [center of our search area](http://maps.google.com/maps?t=m&q=loc:41.8925085+-87.6161696) on Google Maps.

The facets contain various distributional statistics, e.g.:

``` r
content$metadata$facets$bedrooms
```

    ## # A tibble: 6 × 3
    ##   value cumulative.count count
    ##   <dbl>            <int> <dbl>
    ## 1     0              373    79
    ## 2     1              294   211
    ## 3     2               83    73
    ## 4     3               10     8
    ## 5     4                2     1
    ## 6     5                1     1

We can use `describeMetadata` for a better look at some of these facets.

``` r
describeMetadata(content)
```

![](Report_files/figure-markdown_github/unnamed-chunk-7-1.png)

If we only want to take a look at this metadata without going through the (sometimes lengthy) process of downloading individual listings, we can run the following:

``` r
Rbnb::searchLocation(location, metadata.only=TRUE, verbose=FALSE)
```

Results
-------

Here's a look at the actual dataframe we've pulled.

``` r
content$results$data
```

    ## # A tibble: 390 × 54
    ##    bathrooms bedrooms  beds    city       id instant.bookable
    ##        <dbl>    <dbl> <dbl>   <chr>    <chr>            <chr>
    ## 1          1        1     2 Chicago 17582836             TRUE
    ## 2          1        1     1 Chicago 17544023             TRUE
    ## 3          1        0     1 Chicago 17713990             TRUE
    ## 4          1        1     2 Chicago 17316903             TRUE
    ## 5          1        0     1 Chicago 16605075             TRUE
    ## 6          1        0     2 Chicago 17510572             TRUE
    ## 7          1        0     2 Chicago 15956773            FALSE
    ## 8          1        1     2 Chicago 16324985             TRUE
    ## 9          1        1     2 Chicago 17263099             TRUE
    ## 10         1        0     1 Chicago 16947779             TRUE
    ## # ... with 380 more rows, and 48 more variables:
    ## #   is.business.travel.ready <chr>, is.new.listing <chr>, lat <dbl>,
    ## #   lng <dbl>, localized.city <chr>, name <chr>, neighborhood <chr>,
    ## #   person.capacity <dbl>, picture.count <dbl>,
    ## #   primary.host.first.name <chr>, primary.host.has.profile.pic <chr>,
    ## #   primary.host.id <chr>, primary.host.smart.name <chr>,
    ## #   primary.host.is.superhost <chr>, property.type <chr>,
    ## #   property.type.id <chr>, public.address <chr>, reviews.count <dbl>,
    ## #   room.type <chr>, room.type.category <chr>, star.rating <dbl>,
    ## #   user.first.name <chr>, user.has.profile.pic <chr>,
    ## #   user.smart.name <chr>, user.is.superhost <chr>,
    ## #   pricing.quote.available <chr>, pricing.quote.guests <dbl>,
    ## #   pricing.quote.guest.details.number.of.adults <dbl>,
    ## #   pricing.quote.guest.details.number.of.children <chr>,
    ## #   pricing.quote.guest.details.number.of.infants <chr>,
    ## #   pricing.quote.listing.currency <chr>,
    ## #   pricing.quote.localized.currency <chr>,
    ## #   pricing.quote.localized.nightly.price <dbl>,
    ## #   pricing.quote.localized.service.fee <dbl>,
    ## #   pricing.quote.localized.total.price <dbl>,
    ## #   pricing.quote.long.term.discount.amount.as.guest <dbl>,
    ## #   pricing.quote.nightly.price <dbl>, pricing.quote.service.fee <dbl>,
    ## #   pricing.quote.total.price <dbl>, extra.host.languages1 <chr>,
    ## #   extra.host.languages2 <chr>, extra.host.languages <chr>,
    ## #   extra.host.languages3 <chr>, extra.host.languages4 <chr>,
    ## #   extra.host.languages5 <chr>, extra.host.languages6 <chr>,
    ## #   extra.host.languages7 <chr>, extra.host.languages8 <chr>

This dataframe contains **373** listings related to your search term '**60611**'.

We can use `describeResults` to plot some interesting aspects of the data.

``` r
describeResults(content)
```

![](Report_files/figure-markdown_github/unnamed-chunk-10-1.png)

And if we want to plot the location of listings on a map, we can use `showMap`.

``` r
showMap(content)
```

    ## Warning: bounding box given to google - spatial extent only approximate.

    ## converting bounding box to center/zoom specification. (experimental)

    ## Map from URL : http://maps.googleapis.com/maps/api/staticmap?center=41.903309,-87.658392&zoom=13&size=640x640&scale=2&maptype=roadmap&language=en-EN&sensor=false

    ## Warning: Removed 7 rows containing missing values (geom_point).

![](Report_files/figure-markdown_github/unnamed-chunk-11-1.png)

Listing Details
===============

To come.

Price Prediction
================

Let's say we're looking at an existing Airbnb listing, and are curious as to how its price compares with that of comparable listings in the same area. Using `getListingDetail`, we can extract a vector of features about this listing. We can also construct training/test sets using listings returned from the `searchLocation` function called with the (internally calculated) zip code. It's then fairly straightforward to predict the price of our input listing using xgboost!

``` r
predictPrice("17582836")
```

rmarkdown::render('Report.Rmd', output\_format = "all", output\_file=c("README.md", "Report.html"))
