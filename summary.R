# Import and create dataset
url <- "https://raw.githubusercontent.com/melaniewalsh/Neat-Datasets/main/us-prison-jail-rates-1990-WA.csv"
prison_dataset <- read.csv(url)

# Latest year in dataset.
latest_year <- max(prison_dataset$year)

# Average jail population rate for Black individuals in the latest year.
average_black_rate_latest_year <- prison_dataset %>%
  filter(year == latest_year) %>%
  summarise(mean_black_rate = mean(black_jail_pop_rate, na.rm = TRUE))

# Average jail population rate for White individuals in the latest year.
average_white_rate_latest_year <- prison_dataset %>%
  filter(year == latest_year) %>%
  summarise(mean_white_rate = mean(white_jail_pop_rate, na.rm = TRUE))

# County with highest Black jail population rate.
highest_black_rate_county <- prison_dataset %>%
  filter(year == latest_year) %>%
  arrange(desc(black_jail_pop_rate)) %>%
  top_n(1, black_jail_pop_rate) %>%
  select(county_name, black_jail_pop_rate)

# County with lowest Black jail population rate (excl. NAs/zeros).
lowest_black_rate_county <- prison_dataset %>%
  filter(year == latest_year & black_jail_pop_rate > 0) %>%
  arrange(black_jail_pop_rate) %>%
  top_n(-1, black_jail_pop_rate) %>%
  select(county_name, black_jail_pop_rate)

# Change in avg jail population rate for Black individuals from 1990 to the latest year (2018).
change_black_rate <- prison_dataset %>%
  filter(year %in% c(1990, latest_year)) %>%
  group_by(year) %>%
  summarise(mean_black_rate = mean(black_jail_pop_rate, na.rm = TRUE)) %>%
  summarise(change = mean_black_rate[2] - mean_black_rate[1])

# Chart 1: Jail Population Rate Over Time for Black and White Inmates in Washington
ggplot(prison_dataset, aes(x = year)) +
  geom_line(aes(y = black_jail_pop_rate, color = "Black Jail Pop Rate"), na.rm = TRUE) +
  geom_line(aes(y = white_jail_pop_rate, color = "White Jail Pop Rate"), na.rm = TRUE) +
  labs(
    title = "Jail Population Rate Over Time for Black and White Inmates in Washington",
    x = "Year",
    y = "Jail Population Rate per 100,000",
    color = "Rate Type"
  ) +
  theme_minimal()

# Chart 2: Comparison between Male and Female Jail Population Rate
ggplot(prison_dataset, aes(x = male_jail_pop_rate, y = female_jail_pop_rate)) +
  geom_point(aes(color = year), alpha = 0.6) +
  labs(
    title = "Comparison between Male and Female Jail Population Rate",
    x = "Male Jail Population Rate per 100,000",
    y = "Female Jail Population Rate per 100,000",
    color = "Year"
  ) +
  theme_minimal()

us_county_map <- map_data("county")
washington_2018 <- subset(prison_dataset, year == 2018 & state == "WA")
washington_2018$cleaned_county_name <- gsub(" County", "", washington_2018$county_name)
washington_map <- subset(us_county_map, region == "washington")
washington_2018$cleaned_county_name <- tolower(gsub(" County", "", washington_2018$county_name))
washington_map$subregion <- tolower(washington_map$subregion)
combined_wa_data <- left_join(washington_map, washington_2018, by = c("subregion" = "cleaned_county_name"))

ggplot(data = combined_wa_data, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = total_jail_pop_rate), color = "white") +
  coord_fixed(1.3) + 
  scale_fill_viridis_c(name = "Total Jail\nPop Rate", 
                       na.value = "grey50",
                       guide = guide_colorbar(direction = "horizontal")) +
  labs(title = "Total Jail Population Rate by County in Washington (2018)",
       subtitle = "Grey areas represent missing data or non-matching county names") +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, size = 10))




