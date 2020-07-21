## read_covid_data
read_covid_data <- function() {
  
  confirmed <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"
  
  deaths <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"
  
  # Read deaths
  deaths_df <-
    readr::read_csv(deaths) %>%
    mutate(Type = "Deaths")
  
  # Get population
  population_df <-
    deaths_df %>%
    select(UID, iso2, iso3, code3, FIPS, Admin2, Province_State, Country_Region,
           Lat, Long_, Combined_Key, Population)
  
  # Read confirmed cases
  confirmed_df <- 
    readr::read_csv(confirmed) %>% 
    mutate(Type = "Confirmed") %>%
    left_join(population_df, by = c("UID", "iso2", "iso3", "code3", "FIPS",
                                    "Admin2", "Province_State", "Country_Region",
                                    "Lat", "Long_", "Combined_Key")) %>%
    select(UID, iso2, iso3, code3, FIPS, Admin2, Province_State, Country_Region,
           Lat, Long_, Combined_Key, Population, everything())
  
  # Union it together
  union(confirmed_df, deaths_df) %>% 
    select(UID, iso2, iso3, code3, FIPS, Admin2, Province_State, Country_Region,
           Lat, Long_, Combined_Key, Type, Population, everything())
  
  # return(list(confirmed = confirmed_df,
  #             deaths = deaths_df))
  
}

## get_county
get_county <- function(df,
                       County = "New Castle",
                       State = "Delaware"){
  
  # Check if the county is NULL
  if (is.null(County)) {
    
    # If it is, get the whole state
    df %>% 
      filter(Province_State %in% State)
    
  } else {
    
    # If it isn't get specific counties in the state
    df %>% 
      filter(Admin2 %in% County,
             Province_State %in% State)
    
  }
  
}




## tidy_covid
tidy_covid <- function(df){
  df %>% 
    
    # Turn date fields long
    pivot_longer(cols = matches("^[0-9]+\\/[0-9]+\\/[0-9]+"),
                 names_to = "Date",
                 values_to = "Count") %>% 
    
    # Widen based upon type
    pivot_wider(names_from = Type,
                values_from = Count) %>% 
    
    # Force Date field to be type Date
    mutate(Date = as.Date(Date, format = "%m/%d/%y")) %>% 
    
    # Move population to the end of the data frame
    select(-Population, everything(), Population)
}



## graph_covid
graph_covid <- function(df,
                        field = Confirmed,
                        ...) {
  
  field <- enquo(field)
  
  number_of_counties <- length(unique(df$Admin2))
  
  # Check how many counties are being used
  if(number_of_counties > 1) {
    
    # If more than 1, use a color and group aes
    plot <- 
      df %>% 
      ggplot(aes(x = Date,
                 y = !!field,
                 color = Admin2,
                 group = Admin2,
                 ...)) +
      geom_line()
    
  } else {
    
    # Otherwise, don't
    plot <-
      df %>% 
      ggplot(aes(x = Date,
                 y = !!field)) +
      geom_line()
    
  }
  
  # Add in some details to the plot
  plot +
    scale_y_continuous(labels = scales::comma,
                       ...) +
    theme_minimal() +
    theme(legend.position = "top",
          axis.title = element_text(face = "bold"),
          axis.text = element_text(face = "italic")) +
    labs(color = "")
  
}
