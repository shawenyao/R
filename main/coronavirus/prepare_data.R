if(refresh_data){
  
  # raw data of confirmed cases
  coronavirus_raw_nonus <- import("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv") %>% 
    filter(`Country/Region` != "US")
  coronavirus_raw_us <- import("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv", colClasses = c("fips" = "numeric"))
  
  # us coordinates
  data(state.map)
  us_state_coordinates <- state.map %>% 
    group_by(region) %>% 
    summarise(
      long_state = median(long),
      lat_state = median(lat)
    ) %>% 
    # add back other regions
    bind_rows(
      tibble(
        region = c("puerto rico", "virgin islands", "guam"),
        long_state = c(-66.5901, -64.8963, 144.7937),
        lat_state = c(18.2208, 18.3358, 13.4443)
      )
    )
  # us county coordinates
  data(county.map)
  us_county_coordinates <- county.map %>% 
    group_by(region) %>% 
    summarise(
      long_county = mean(long),
      lat_county = mean(lat)
    )
  
  # format data: non-US
  coronavirus_input_nonus <- coronavirus_raw_nonus %>% 
    mutate(
      id = if_else(
        `Province/State` == "",
        `Country/Region`,
        paste0(`Province/State`, " @ ", `Country/Region`)
      ),
      display_name = if_else(
        `Province/State` == "",
        `Country/Region`,
        `Province/State`
      )
    ) %>% 
    select(-`Province/State`, -`Country/Region`) %>% 
    gather(date, cases, -id, -Lat, -Long, -display_name) %>% 
    mutate(
      date = as.Date(date, format = "%m/%d/%y")
    ) %>% 
    arrange(id, date) %>% 
    # forward filling
    group_by(id) %>% 
    mutate(
      cases = na.locf(cases)
    ) %>% 
    ungroup() %>% 
    select(lat = Lat, long = Long, id, display_name, date, cases)
  
  # format data: US
  coronavirus_input_us <- coronavirus_raw_us %>% 
    transmute(
      id = paste0(county, " @ ", state),
      display_name = id,
      date = as.Date(date),
      cases = cases,
      fips = fips,
      county = tolower(county),
      state = tolower(state)
    ) %>% 
    left_join(us_county_coordinates, by = c("fips" = "region")) %>% 
    left_join(us_state_coordinates, by = c("state" = "region")) %>% 
    mutate(
      lat = if_else(!is.na(lat_county), lat_county, lat_state),
      long = if_else(!is.na(long_county), long_county, long_state)
    ) %>% 
    select(lat, long, id, display_name, date, cases)
  
  # remove extra dates in one of the datasets
  max_date <- min(max(coronavirus_input_nonus$date), max(coronavirus_input_us$date))
  coronavirus_input <- bind_rows(coronavirus_input_nonus, coronavirus_input_us) %>% 
    filter(date <= max_date) %>% 
    arrange(date, id) %>% 
    # overide NYC coordinates
    mutate(
      lat = if_else(id == "New York City @ New York", 40.7831, lat),
      long = if_else(id == "New York City @ New York", -73.9712, long)
    )
    
  export(coronavirus_input, "../../input/coronavirus/coronavirus.csv")
}
