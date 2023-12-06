# https://appsilon.com/r-shiny-dashboard-templates/?utm_source=template_marketplace&utm_campaign=templates#templates
# https://connect.appsilon.com/shiny-enterprise-demo/?utm_medium=referral&utm_source=template_marketplace&utm_campaign=templates
# Radar map for attributes
# Compare Bakery A vs. Bakery B.
# Radar map of sentiment scores - comparing scores of aspects for good and bad reviews. (open/close?)

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
library(fmsb)

theme_set(theme_classic())

tmap_mode('view')
options(tigris_use_cache = TRUE)

# Read Files
bakeries = read_csv('https://github.com/NiharikaCNR/Yelp-Bakeries/raw/main/data/bakeries.csv') %>% 
  mutate(is_open = factor(is_open, levels = c(0,1)), price_range=if_else(is.na(price_range), 0, as.numeric(price_range))) %>% 
  mutate(dollar_signs = strrep('$',price_range))
bakery_reviews = read.csv(file = 'https://github.com/NiharikaCNR/Yelp-Bakeries/raw/main/data/bakery_reviews.csv')
aspect_scores_overall <- read_csv('https://github.com/NiharikaCNR/Yelp-Bakeries/raw/main/data/aspect_scores_overall.csv')
incomes <- read_csv('https://github.com/NiharikaCNR/Yelp-Bakeries/raw/main/data/ca-sb-mean-incomes.csv')
sentiment_scores <- read_csv('https://github.com/NiharikaCNR/Yelp-Bakeries/raw/main/data/sentiment_scores.csv')
valid_bakeries <- read.table('https://github.com/NiharikaCNR/Yelp-Bakeries/raw/main/data/valid_bakeries.txt', header = FALSE)$V1

# Create plot data
## Bakeries map data
zipcodes <- incomes$zip
open_bakeries = bakeries %>% filter(business_id %in% valid_bakeries & is_open==1)
closed_bakeries = bakeries %>% filter(business_id %in% valid_bakeries & is_open==0)

bakeries_summary = bakery_reviews %>% group_by(business_id) %>% 
  summarise(num_reviews = n(), avg_rating = sum(stars)/num_reviews, first_review = min(date), last_review = max(date),
            operation_time = as.Date(last_review) - as.Date(first_review)) %>% 
  mutate(opening_year = as.numeric(str_extract(first_review, '[0-9]+')), closing_year = ifelse(business_id %in% closed_bakeries$business_id, as.numeric(str_extract(last_review, '[0-9]+')),.Machine$integer.max)) %>% 
  arrange(closing_year)

y <- 2022

bakeries_map_data <- inner_join(
  bakeries %>% filter(business_id %in% valid_bakeries), 
  bakeries_summary, by='business_id'
  ) %>% 
  filter(num_reviews > 50) %>% 
  inner_join(sentiment_scores) %>%
  select(latitude, longitude, name, business_id, is_open, avg_rating, opening_year, closing_year, sentiment_label, dollar_signs) %>% 
  st_as_sf(coords = c('longitude', 'latitude'), crs = 4326)

## Zip code Income Map data
zipcode_boundaries <- zctas(state='CA', year=2010) %>% 
  filter(ZCTA5CE10 %in% zipcodes) %>% 
  select(zip=ZCTA5CE10, lat=INTPTLAT10, long=INTPTLON10, geometry) %>% 
  mutate(zip=as.numeric(zip), lat=as.numeric(lat), long=as.numeric(long), geometry)

zipcode_map_data <- incomes %>% 
  inner_join(zipcode_boundaries) %>% 
  st_as_sf(sf_column_name='geometry')

beautiful_radarchart <- 
  function(data, color = '#00AFBB', vlabels = colnames(data), vlcex = 0.7, caxislabels = NULL, title = NULL, ...) {
    radarchart(
      data, axistype = 1,
      # Customize the polygon
      pcol = color, pfcol = c(scales::alpha(color[1],0.5),rep(NA,4)), plwd = 2, plty = 1,
      # Customize the grid
      cglcol = 'grey', cglty = 1, cglwd = 0.8,
      # Customize the axis
      axislabcol = 'white', 
      # Variable labels
      vlcex = 1, vlabels = vlabels,
      caxislabels = caxislabels, title = title, ...
    )
  }

update_map <- function(y) {
  # Filtering data for Map-1
  bakeries_map_data <- bakeries_map_data %>% 
    mutate(was_open = (as.numeric(y)>=opening_year)) %>% 
    filter(was_open) %>% 
    mutate(is_open_now = ifelse((closing_year>=as.numeric(y)), T, F))
  
  # Map-1: A map showing locations of open and closed bakeries in Santa Barbara, CA in year 'y'
  bakeries_map <- tm_shape(bakeries_map_data) + 
  tm_bubbles(
    col='is_open_now', size='avg_rating', palette='Set1', alpha=0.7,scale=0.2, 
    popup.vars=c('Name'='name', 
                 'Average Rating:'='avg_rating', 
                 'Price Category:' = 'dollar_signs',
                 'Review Verdict:'='sentiment_label', 
                 'Business ID'='business_id'
                )
     )
  # tmap_leaflet(bakeries_map)
  
  # Map-2: A map showing avg. incomes of open and closed bakeries in Santa Barbara, CA in year 'y'
  zipcode_income_map <- tm_shape(zipcode_map_data) +
    tm_polygons(col=y, alpha = 0.6, palette='Greens', popup.vars=c('Avg. Income: '=y))
  # tmap_leaflet(zipcode_income_map)
  
  ## Final Map: Map-1 overlayed over Map-2 
  tmap_leaflet(zipcode_income_map + bakeries_map + tm_view(set.view = c(-119.72,34.42,14), set.zoom.limits = c(13,17)))
  
}

update_radar_plot <- function(bakery_name, open_status) {
  limit_rows <- aspect_scores_overall %>% 
    head(3) %>%
    select(-c(2,3,4))
  
  if(bakery_name=='All') {
    name <- paste('All',open_status,'Bakeries Scores')
    bakeries_list <- if(open_status=='Open') open_bakeries else closed_bakeries
    selected_bakery <- aspect_scores_overall %>% 
      filter(business_id %in% bakeries_list$business_id) %>% 
      summarize_at(vars(colnames(aspect_scores_overall)[5:10]), ~round(mean(.),2), .names = '{.col}') %>% 
      mutate(business_id = name, .before = stars)
    bakery_name <- selected_bakery$business_id[1]
  }
  else {
    selected_bakery <- aspect_scores_overall %>% 
      filter(name==bakery_name) %>% 
      head(1) %>% 
      select(-c(2,3,4))
  }
    
  radar_plot_data <- rbind(limit_rows,selected_bakery)
  sample <- as.data.frame(radar_plot_data)
  rownames(sample) <- c(sample$business_id[1:3],bakery_name)
  sample <- sample %>% select(c('Ambiance','Drinks','Food','Price','Service'))
  
  # CONSTRUCTING RADAR PLOT
  op <- par(mar = c(1, 2, 2, 2))
  beautiful_radarchart(sample, caxislabels=c(), c('#d9d9d9', '#605ca8'))
  legend(
    x = 'bottom', legend = rownames(sample[-c(1,2),]), horiz = T,
    bty = 'n', pch = 20 , col = c('#d9d9d9', '#605ca8'),
    text.col = 'black', cex = 1, pt.cex = 1.75
  )
  # par(op)
}

generate_summary <- function(bakery_name) {
  if (bakery_name=="All") {
    return (paste0('<h4>Please select a bakery to see summary</h4>',
                   '<span>Reading the plot:</span><br>',
                   '<ul>',
                   '<li>Grey area represents the performance scores of all bakeries in Santa Barbara, CA.</li>',
                   '<li>The purple plot represents the performance scores of the selected bakery. Choose one from the above drop-down.</li>',
                   '<li>If a node of the purple polygon lie inside the grey region, it means that the selected bakery is performing poorly in that aspect as compared to the average score. If the node lies outside, then it is performing better than the others. (Note that this rule is to be read in reverse for the Price category)</li>',
                   '</ul>'))
  }
  # bakery_name <- 'Helena Avenue Bakery'
  avg <- aspect_scores_overall[3,]
  selected <- aspect_scores_overall %>% filter(name==bakery_name) %>% head(1)
  
  amb_text <- "Select a bakery to get summary"
  if((avg$Ambiance-selected$Ambiance)>0) {
    amb_text <- "You need to make improvements to your bakery ambiance."
  }else {
    amb_text <- "You are already outdoing other bakeries.<br>Keep it up!"
  }
  
  drinks_text <- "Select a bakery to get summary"
  if((avg$Drinks-selected$Drinks)>0) {
    drinks_text <- "You need to updates the quality and menu of the drinks you offer."
  }else {
    drinks_text <- "You are already outdoing other bakeries.<br>Keep it up!"
  }
  
  food_text <- "Select a bakery to get summary"
  if((avg$Food-selected$Food)>0) {
    food_text <- "You need to revisit the quality of your food."
  }else {
    food_text <- "You are already outdoing other bakeries.<br>Keep it up!"
  }
  
  price_text <- "Select a bakery to get summary"
  if((avg$Price-selected$Price)>0) {
    price_text <- "Your menu prices are competitive and affordable. Keep it up!"
  }else {
    price_text <- "You need to revamp the pricing of your menu items"
  }
  
  service_text <- "Select a bakery to get summary"
  if((avg$Service-selected$Service)>0) {
    service_text <- "Your staff and service needs to be more approachable."
  }else {
    service_text <- "You are already outdoing other bakeries.<br>Keep it up!"
  }
  
  tags <- paste0('<font size="3"><span><b><u>Ambiance Summary:</u></b></span></font>',
                 '<font size="3"><br>',amb_text,'</font><br>',
                 '<font size="3"><span><b><u>Drinks Summary:</u></b></span></font>',
                 '<font size="3"><br>',drinks_text,'</font><br>',
                 '<font size="3"><span><b><u>Food Summary:</u></b></span></font>',
                 '<font size="3"><br>',food_text,'</font><br>',
                 '<font size="3"><span><b><u>Price Summary:</u></b></span></font>',
                 '<font size="3"><br>',price_text,'</font><br>',
                 '<font size="3"><span><b><u>Service Summary:</u></b></span></font>',
                 '<font size="3"><br>',service_text,'</font>')
  return (tags)
}


home_page_body <- function() {
  fluidPage(
    style = 'font-size: 2rem; text-align: justify;',
    h1('What is this app?'),
    p('The overall goal of this application is to provide useful, analytical insights to bakery store business owners on
Yelp, making it easy to be understood.'),
    p('Furthermore, we focus on this specific goal: We aim to provide advice for bakery shop owners following the key impact factors extracted from reviews and the correlation with regional income.'), 
    p('We are going to answer: What factors contribute to the opening and close of bakery shops in Santa Barbara?'),
    tags$br(),
    h1('How do I navigate?'),
    tags$ol(
      tags$li('Map: A map of the open and closed bakeries in Santa Barbara, CA, overlaid with a choropleth map of income across zip codes. Click on each point to view more information on each bakery. Choose years to see what bakeries were open and clossed each year.'),
      tags$li('Radar Plots: Graphs of open and closed bakeries based on five aspects of comparison'),
    )
  )
}

map_page_body <- function() {
  fluidPage(
    prettyRadioButtons('year', 'Select a year:', choices=rev(colnames(incomes)[-1]), selected='2019', inline=T),
    leafletOutput('income_bakeries_map', height = 700)
  )
}

radar_page_body <- function() {
  fluidPage(
    fluidRow(
      sidebarLayout(
        sidebarPanel(
          selectizeInput( 'open_bakery', 'Select an open bakery:', c('All', unique(open_bakeries$name)) ),
          htmlOutput('open_summary')
        ),
        mainPanel(
          plotOutput('open_radar_plot', height = '430px')
        )
      )
    ),
    tags$hr(), 
    fluidRow(
      sidebarLayout(
        sidebarPanel(
          selectizeInput( 'closed_bakery', 'Select a closed bakery:', c('All', unique(closed_bakeries$name)) ),
          htmlOutput('closed_summary')
        ),
        mainPanel(
          plotOutput('closed_radar_plot', height = '430px')
        )
      )
    )
  )
}

ui <- dashboardPage(
  header = dashboardHeader(title = 'Bakeries in Santa Barbara, CA', titleWidth = 'calc(20%)'),
  sidebar = dashboardSidebar(
    sidebarMenu(
      id = 'tabs',
      menuItem('Home', tabName = 'home_page'),
      menuItem('Map' , tabName = 'map_page' ),
      menuItem('Radar Plots', tabName = 'radar_page')
      )
    ),
  body = dashboardBody( 
    includeCSS('https://github.com/NiharikaCNR/Yelp-Bakeries/raw/main/styles/app.css'),
    tabItems( 
      tabItem( tabName = 'home_page', home_page_body() ),
      tabItem( tabName = 'map_page',  map_page_body()  ),
      tabItem(tabName = 'radar_page', radar_page_body())
      )
    ),
  skin = 'purple'
)


server <- function(input, output, session) {
  y <- reactive({input$year})
  open_bakery <- reactive({input$open_bakery})
  closed_bakery <- reactive({input$closed_bakery})
  
  output$selected_year <- renderText(y())
  output$income_bakeries_map <- renderLeaflet(update_map(y()))
  
  output$open_radar_plot <- renderPlot(update_radar_plot(open_bakery(),'Open'))
  output$open_summary <- renderText(generate_summary(open_bakery()))
  
  output$closed_radar_plot <- renderPlot(update_radar_plot(closed_bakery(),'Closed'))
  output$closed_summary <- renderText(generate_summary(closed_bakery()))
}

shinyApp(ui, server)






