library(tidyverse)
library(tmap)
library(sf)
library(jsonlite)
library(tigris)
library(tidycensus)
library(bslib)
library(shinydashboard)
library(leaflet)
library(shinyWidgets)

theme_set(theme_classic())

tmap_mode("view")
options(tigris_use_cache = TRUE)

# Read Files
bakeries = read_csv("https://github.com/NiharikaCNR/Yelp-Bakeries/raw/main/data/bakeries.csv") %>% 
  mutate(is_open = factor(is_open, levels = c(0,1)))
bakery_reviews = read.csv(file = 'https://github.com/NiharikaCNR/Yelp-Bakeries/raw/main/data/bakery_reviews.csv')
incomes <- read_csv("https://github.com/NiharikaCNR/Yelp-Bakeries/raw/main/data/ca-sb-mean-incomes.csv")

# Create plot data
## Bakeries map data
zipcodes <- incomes$zip
valid_bakeries <- bakeries %>% filter(postal_code %in% zipcodes) #%>% pull(business_id)
open_bakeries = valid_bakeries %>% filter(is_open==1)
closed_bakeries = valid_bakeries %>% filter(is_open==0)

bakeries_summary = bakery_reviews %>% group_by(business_id) %>% 
  summarise(num_reviews = n(), avg_rating = sum(stars)/num_reviews, first_review = min(date), last_review = max(date),
            operation_time = as.Date(last_review) - as.Date(first_review)) %>% 
  mutate(opening_year = as.numeric(str_extract(first_review, '[0-9]+')), closing_year = ifelse(business_id %in% closed_bakeries$business_id, as.numeric(str_extract(last_review, '[0-9]+')),.Machine$integer.max)) %>% 
  arrange(closing_year)

y <- 2022

bakeries_map_data <- inner_join(valid_bakeries, bakeries_summary, by='business_id') %>% 
  filter(num_reviews > 50) %>% 
  select(latitude, longitude, name, business_id, is_open, avg_rating, opening_year, closing_year) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

## Zip code Income Map data
zipcode_boundaries <- zctas(state="CA", year=2010) %>% 
  filter(ZCTA5CE10 %in% zipcodes) %>% 
  select(zip=ZCTA5CE10, lat=INTPTLAT10, long=INTPTLON10, geometry) %>% 
  mutate(zip=as.numeric(zip), lat=as.numeric(lat), long=as.numeric(long), geometry)

zipcode_map_data <- incomes %>% 
  inner_join(zipcode_boundaries) %>% 
  st_as_sf(sf_column_name='geometry')

#TODO:  Maps -- REMOVE
# bakeries_map <- tm_shape(bakeries_map_data) + 
#   tm_dots(col = 'is_open_now', size = 'avg_rating', palette="Set1", alpha=0.7)
#  bakeries_map
# zipcode_income_map <- tm_shape(zipcode_map_data) +
#   tm_polygons(col='2020', alpha = 0.7)
#  zipcode_income_map

##TODO: Final Map -- REMOVE
# zipcode_income_map + bakeries_map + tm_view(set.view = c(-122,34.44,14), set.zoom.limits = c(14,17))

# ------------------------------------------------------------------------------------------------------------------------


update_map <- function(y) {
  # Filtering data for Map-1
  bakeries_map_data <- bakeries_map_data %>% 
    mutate(was_open = (as.numeric(y)>=opening_year)) %>% 
    filter(was_open) %>% 
    mutate(is_open_now = ifelse((closing_year>=as.numeric(y)), T, F))
  
  # Map-1: A map showing locations of open and closed bakeries in Santa Barbara, CA in year 'y'
  bakeries_map <- tm_shape(bakeries_map_data) + 
  tm_bubbles(col='is_open_now', size='avg_rating', palette="Set1", alpha=0.7,scale=0.2, 
             popup.vars=c('Name'='name', 'Business ID'='business_id', 'Average Rating:'='avg_rating'))
  # tmap_leaflet(bakeries_map)
  
  # Map-2: A map showing avg. incomes of open and closed bakeries in Santa Barbara, CA in year 'y'
  zipcode_income_map <- tm_shape(zipcode_map_data) +
    tm_polygons(col=y, alpha = 0.6, palette="Greens")
  # tmap_leaflet(zipcode_income_map)
  
  ## Final Map: Map-1 overlayed over Map-2 
  tmap_leaflet(zipcode_income_map + bakeries_map + tm_view(set.view = c(-119.72,34.42,14), set.zoom.limits = c(13,17)))
  
}


home_page_body <- function() {
  fluidPage(
    style = "font-size: 2rem; text-align: justify;",
    h1("What do we do?"),
    p("[Insert Text here]"),
    tags$br(),
    h1("How to use this site?"),
    tags$ol(
      tags$li(""),
      tags$li(""),
      tags$li(""),
      tags$li(""),
    )
  )
}

map_page_body <-function() {
  fluidPage(
    prettyRadioButtons('year', "Select a year:", choices=rev(colnames(incomes)[-1]), selected='2019', inline=T),
    leafletOutput("income_bakeries_map", height = 700),
  )
}


ui <- dashboardPage(
  header = dashboardHeader(title = "Bakeries in Santa Barbara, CA", titleWidth = "calc(20%)"),
  sidebar = dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Home", tabName = "home_page"),
      menuItem("Map" , tabName = "map_page" )
      # menuItem("Method", tabName = "method_page"),
      # menuItem("Reporting Tool", tabName = "reporting_tool"),
      # menuItem("Result Summary", tabName = "result_summary") , 
      # menuItem("Team", tabName = "team_page")
      )
    ),
  body = dashboardBody( 
    includeCSS("styles/app.css"),
    tabItems( 
      tabItem( tabName = "home_page", home_page_body() ),
      tabItem( tabName = "map_page",  map_page_body()  )
      # tabItem(tabName = "methods_page", method_tab_body()),
      # tabItem(tabName = "reporting_tool", reporting_tool_body()), 
      # tabItem(tabName = "result_summary"), 
      # tabItem(tabName = "team_page", team_tab_body())
      )
    ),
  skin = 'purple'
)


server <- function(input, output, session) {
  y <- reactive({input$year})
  # message(year())
  output$selected_year <- renderText(y())
  output$income_bakeries_map <- renderLeaflet(update_map(y()))
  # observeEvent(input$year, {
  #   output$income_bakeries_map <- renderLeaflet({
  #     # data("World")
  #     # tmap_leaflet(tm_shape(World) + tm_borders())
  #     # browser()
  #     message("------------HELLOOOOOOO------------------")
  #     # browser()
  #     update_map(y())
  #   })
  # })
  
}

shinyApp(ui, server)






