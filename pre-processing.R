library(rjson)
library(tidyverse)


# business <- read.csv(file = '/Users/Patron/Documents/Yelp/yelp_Fall2023/business.csv')

# bakeries <- business[grepl("Bakeries", business$categories), ] %>% filter(state=="CA" & city=="Santa Barbara") %>% na.omit()
# write_csv(bakeries, "/Users/Patron/Documents/Yelp/yelp_Fall2023/bakeries.csv")
bakeries = read_csv("/Users/Patron/Documents/Yelp/yelp_Fall2023/bakeries.csv")

closed_bakeries <-  bakeries %>% filter(is_open==0) %>% pull(business_id)


# reviews = read.csv(file = '/Users/Patron/Documents/Yelp/yelp_Fall2023/review.csv') %>% %>% filter(business_id %in% bakeries$business_id)
# write_csv(bakery_reviews, "/Users/Patron/Documents/Yelp/yelp_Fall2023/bakery_reviews.csv")

bakery_reviews = read.csv(file = '/Users/Patron/Documents/Yelp/yelp_Fall2023/bakery_reviews.csv') 


# trips = read_csv("/Users/Patron/Documents/Yelp/yelp_Fall2023/Trips_by_Distance.csv")

# colnames(trips) <- tolower(gsub(" ", "_", tolower(colnames(trips))))

# county_trips = trips %>% filter(level =="County" & state_postal_code=="CA" & county_name=="Santa Barbara County")
# write_csv(county_trips, "/Users/Patron/Documents/Yelp/yelp_Fall2023/county_trips.csv")
county_trips = read_csv("/Users/Patron/Documents/Yelp/yelp_Fall2023/county_trips.csv")

open_business_reviews  =  bakery_reviews %>% filter(business_id == "IDtLPgUrqorrpqSLdfMhZQ")
write_csv(open_business_reviews, "/Users/Patron/Documents/Yelp/yelp_Fall2023/open_business_reviews.csv")

closed_bakeries_reviews = bakery_reviews %>% filter(business_id %in% closed_bakeries_summary$business_id)
# write_csv(closed_business_reviews, "/Users/Patron/Documents/Yelp/data/all_closed_bakeries_reviews.csv")

# write_csv(open_bakeries_reviews, "/Users/Patron/Documents/Yelp/data/open_bakeries_reviews.csv")

closed_bakeries_reviews <- closed_bakeries_reviews %>% mutate(is_positive = !(stars<3.5))
open_bakeries_reviews <- open_bakeries_reviews %>% mutate(is_positive = !(stars<3.5))

write_csv(open_bakeries_reviews %>% filter(is_positive==T) %>% select(-is_positive), 
          "/Users/Patron/Documents/Yelp/data/open_bakeries_positive_reviews.csv")



