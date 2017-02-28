## function to return Airbnb auth token
library(jsonlite)
library(httr)
library(magrittr)

api.token <- function(email,
                   password,
                   currency="USD",
                   locale="en-US",
                   grant_type="password",
                   client_id="3092nxybyb0otqw18e8nh5nty"
                   ) {
  params = list(client_id=client_id,
                locale=locale,
                currency=currency,
                grant_type=grant_type,
                password=password,
                username=email
  )
  r <- httr::POST(url="https://api.airbnb.com/v1/authorize",
                  body = params)
  api.json <- r$content  %>%
    paste(collapse="") %>%
    {sapply(seq(1, nchar(.), by=2), function(x) substr(., x, x+1))} %>%
    {rawToChar(as.raw(strtoi(.,16L)))} %>%
    fromJSON()
  api.json$access_token
}
