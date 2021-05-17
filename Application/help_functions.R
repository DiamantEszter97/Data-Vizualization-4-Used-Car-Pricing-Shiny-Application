
library(dplyr)
library(ggplot2)
library(data.table)
library(tidyr)
library(DT)
library(wesanderson)

# load data and drop url
cars <- read.csv('cars.csv')
cars <- cars %>% filter((price >= 500) & (price <= 3000000)) %>%  select(-url)


# see the prices to investigate the outliners
# summary(cars$price)

# save the min and max price for slider range
min_price <- min(cars$price)
max_price <- max(cars$price)

# save loldest and latest posting dates:
cars$posting_date <- as.Date(cars$posting_date)
min_date <- min(cars$posting_date)
max_date <- max(cars$posting_date)

# get unique values for selectors
cars_full_price <- cars %>% filter(manufacturer != "")
manu_list <- unique(cars_full_price$manufacturer)
region_list <- sort(unique(cars$region))
type_list <- sort(unique(cars$type))[2:14]
condition_list <- sort(unique(cars$condition))[2:7]
cyclinder_list <- sort(unique(cars$cylinders))[2:9]
fuel_list <- sort(unique(cars$fuel))[2:6]
transmission_list <- sort(unique(cars$transmission))[2:4]


# get the average price per menufacturer
get_avg_price_per_manufacturer <- function(df, x) {
  cars_full_price <- df %>% filter(manufacturer != "")
  df <- as.data.table(cars_full_price %>% group_by(manufacturer) %>%
    summarise(avg_price = mean(price, na.rm = T)))
  df <- df[manufacturer %in% x]
  return(df)
}


# plot manufacturer df
plot_avg_price_per_manufacturer <- function(df) {
  p <- ggplot(df, aes(reorder(manufacturer, avg_price), avg_price)) +
    geom_bar(stat = 'identity', fill = "brown1") +
    coord_flip() +
    theme_bw()

  print(p)
}


# get price summary for different car manufacturer
get_price_summary_per_manufacturer <- function(df, x) {
    df <- df %>% filter(manufacturer != "")
    df_summary <- df %>%  filter(manufacturer == x) %>%
      summarise(manufacturer = x,
                average = round(mean(price, na.rm = T), 0),
                median = round(median(price, na.rm = T), 0),
                minimum = round(min(price, na.rm = T), 0),
                maximum = round(max(price, na.rm = T), 0),
                standard_deviation = round(sd(price, na.rm = T), 0))
    return(df_summary)
}

# print results for price summary
summary_list <- function(df, x) {
  df <- df %>% filter(manufacturer != "")
  df_summary <- df %>%  filter(manufacturer == x) %>%
    summarise(manufacturer = x,
              average = round(mean(price, na.rm = T), 0),
              median = round(median(price, na.rm = T), 0),
              minimum = round(min(price, na.rm = T), 0),
              maximum = round(max(price, na.rm = T), 0),
              standard_deviation = round(sd(price, na.rm = T), 0))
    sum_list <- list(paste("The average price of", df_summary$manufacturer, "is", df_summary$average),
    paste("The median price of", df_summary$manufacturer, "is", df_summary$median),
    paste("The minimum price of", df_summary$manufacturer, "is", df_summary$minimum),
    paste("The maximum price of", df_summary$manufacturer, "is", df_summary$maximum),
    paste("The standard devition of the price of", df_summary$manufacturer, "is", df_summary$standard_deviation))
  return(sum_list)
}

# print price summary
print_summary <- function(list) {
  for(i in 1:length(list)) {
    print(list[i])
  }
}

# simple line plot to see correlation between year of manufacturing and price
df_price_changes_through_years <- function(df, x) {

      df <- df %>% filter(manufacturer != "") %>% select(c(manufacturer, year, price, fuel, transmission))

      df <- df %>%  filter((manufacturer == x) & (transmission != ""))

      df <- sample_n(df, round((nrow(df)/10), 0))

      return(df)


}

plot_price_changes_through_years <- function(df) {

  p <- ggplot(df, aes(year, price)) +
    geom_point(color = "brown1") +
    geom_smooth(se = F, color = 'black') +
    ylim(500, 100000) +
    theme_bw()

  print(p)
}


# The great filtering for Details Sidebars

# df_price_histogram <- function(df, regi, yr, manu, con, cyl, fu, odo, transm, ty) {
#
#   # df <- df %>% filter(region == regi)
#   # df <- df %>% filter(year == yr)
#   # df <- df %>% filter(manufacturer == manu)
#   # df <- df %>% filter(condition == con)
#   # df <- df %>% filter(cylinders == cyl)
#   # df <- df %>% filter(fuel == fu)
#   # df <- df %>% filter((odometer >= odo[1]) & (odometer <= odo[2]))
#   # df <- df %>% filter(transmission == transm)
#   # df <- df %>% filter(type == ty)
#
#   # df <- filter(df, region %in% regi)
#   df <- filter(df, year %in% yr)
#   df <- filter(df, manufacturer %in% manu)
#   df <- filter(df, condition %in% con)
#   df <- filter(df, cylinders %in% cyl)
#   df <- filter(df, odometer >= odo[1], odometer <= odo[2])
#   df <- filter(df, transmission %in% transm)
#   df <- filter(df, type %in% ty)
#
#   return(df)
#
# }

# plot histogramm

plot_price_hist <- function(df) {

  p <- ggplot(df, aes(price)) +
    geom_histogram(aes(y=..density..), colour="brown1", fill="white")+
    geom_density(alpha=.2, fill="#FF6666") +
    theme_bw()
  print(p)
}

# filter(cars, manufacturer %in% manu_list,
#             condition %in% condition_list,
#             cylinders %in% cyclinder_list,
#             fuel %in% fuel_list,
#             region %in% region_list,
#             transmission %in% transmission_list,
#             type %in% type_list)
#

# widget object
cars_sel <- cars %>% select(-c(X, entry_id))

cars_DT <- DT::datatable(cars_sel, filter = 'top', options = list(
  pageLength = 20, autoWidth = TRUE
))
