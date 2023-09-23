#inspired by https://themockup.blog/posts/2020-05-22-parsing-json-in-r-with-jsonlite/
library(tidyverse)
library(httr)
library(jsonlite)

url_json <- "https://www.nationaltrust.org.uk/api/search/places?query=&lat=52&lon=0&milesRadius=1000&maxPlaceResults=1000"
raw_json <- url_json %>% 
  httr::GET() %>% 
  httr::content()

raw_json1 <- raw_json$multiMatch$results


raw_json1[[1]]$type %>% str(max.level = 1)

raw_json$multiMatch %>% 
  # equivalent to raw_json[["athletes"]][[1]][["athlete"]]
  purrr::pluck("results", 1) %>% 
  str(max.level = 1)


category_names = c("type", "title", "lat", "long")
loc_name = c("lat", "long")

get_NT_data <- function(row_n) {
  prop_type <- raw_json$multiMatch %>% 
    purrr::pluck("results", row_n) %>% 
    keep(names(.) %in% c("type", "title", "town")) %>% 
    unlist()
  
  prop_loc <- raw_json$multiMatch %>% 
    purrr::pluck("results", row_n, "location") %>% 
    set_names(nm = loc_name)

  c(prop_type, prop_loc)
}

NT_df <- 1:length(raw_json$multiMatch$results) %>% 
  map_dfr(get_NT_data) %>% 
  mutate(across(lat:long, as.double))

write_excel_csv(NT_df, "data/NT prop loc.csv")
  

