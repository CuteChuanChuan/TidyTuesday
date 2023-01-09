
# Library -----------------------------------------------------------------


library(tidyverse)
library(here)
library(sjmisc)
library(sjlabelled)
library(sjPlot)
library(rio)
library(magrittr)
library(naniar)
library(sp)
library(spdplyr)
library(geojsonsf)
library(geojsonio)
library(showtext)
library(sf)
library(usefun)
library(rcartocolor)
library(RColorBrewer)
library(rgdal)
library(broom)
library(rgeos)
library(stringr)
library(viridis)



# Font --------------------------------------------------------------------


font_add_google("Cabin Sketch", "cabin")
font_add_google("Lato", "lato")
showtext_auto()


# Data --------------------------------------------------------------------

# tuesdata <- tidytuesdayR::tt_load('2022-09-27')
# artists <- tuesdata$artists
# export(artists, here("2022-09-27/2022.09.27.Rds"))

artists <- import(here("2022-09-27/2022.09.27.Rds"))

# Wrangle -----------------------------------------------------------------

## Compute: 
### Total workers of each state
### Total artists of each state
### Total numbers of each type artist in each state

artists.short <- artists |> 
    select(-location_quotient, -race, -artists_share) |> 
    group_by(state, type) |> 
    summarise(across(everything(), ~ sum(.x, na.rm = TRUE))) |> 
    rename(total_workers_n = all_workers_n) |> 
    ungroup() |> 
    group_by(state) |> 
    mutate(total_artists_n = sum(artists_n, na.rm = TRUE)) |> 
    ungroup()

# Compute: 
## Percent of artists of each state
## Percent of each type of artists of each state
artists.short <- artists.short |> 
    mutate(artists_prop = artists_n / total_workers_n,
           total_artists_prop = total_artists_n / total_workers_n)

artists.short <- artists.short |> 
    group_by(state) |> 
    slice_max(order_by = artists_n, n = 1) |> 
    select(state, type) |> 
    ungroup() |> 
    rename(most_artists = type) |> 
    right_join(y = artists.short, by = "state") |> 
    relocate(most_artists, .after = type) |> 
    mutate(most_artists = as_factor(most_artists))

# Create map --------------------------------------------------------------


spdf <- geojson_read(here("2022-09-27/us_states_hexgrid.geojson"), 
                     what = "sp")

spdf@data <- spdf@data |> 
    mutate(google_name = gsub(" \\(United States\\)", "", google_name))

temp <- artists.short |> 
    select(state, most_artists) |> 
    distinct()

spdf@data <- spdf@data |> 
    left_join(y = temp, by = c("google_name" = "state"))

spdf_fortified <- tidy(spdf, region = "google_name")

centers <- 
    cbind.data.frame(data.frame(gCentroid(spdf, byid = TRUE), 
                                id = spdf@data$iso3166_2))

labels <- 
    cbind.data.frame(data.frame(gCentroid(spdf, byid = TRUE), 
                                label = spdf@data$most_artists))

ggplot() +
    geom_polygon(data = spdf_fortified, 
                 aes(x = long, y = lat, group = group), 
                 fill="skyblue", color="white") +
    geom_text(data = centers, aes(x = x, y = y, label = id)) +
    theme_void() +
    coord_map()


# Map + Data --------------------------------------------------------------

spdf_fortified <- spdf_fortified |> 
    left_join(., y = artists.short, by = c("id" = "state")) 

spdf_fortified |> 
    ggplot() +
    geom_polygon(aes(x = long, y = lat, group = group, fill = as_numeric(most_artists))) +
    scale_fill_gradient() +
    theme_void() +
    coord_map()

spdf_fortified |> 
    ggplot() +
    geom_histogram(aes(x = total_artists_prop), bins = 50)

spdf_fortified$bin <- 
    cut(spdf_fortified$total_artists_prop,
        breaks = c(0, 0.003, 0.006, 0.009, 0.012, 0.015, 0.018, 0.021, 0.024, Inf),
        labels = c("0 - 0.3", "0.3 - 0.6", "0.6 - 0.9", "0.9 - 1.2", "1.2 - 1.5", "1.5 - 1.8", "1.8 - 2.1", "2.1 - 2.4", "> 2.4"),
        include.lowest = TRUE)

my_palette <- rev(mako(8))[c(-1,-8)]

st <- str_wrap("The word below each state name indicates the highest proportion of artist type in that state.", 100)

spdf_fortified |> 
    ggplot() +
    geom_polygon(aes(fill = bin, x = long, y = lat, group = group) , size = 0, alpha = 0.9) +
    geom_text(data = centers, aes(x = x, y = y, label = id), color = "black", size = 5, alpha = 0.6) +
    geom_text(data = labels , aes(x = x, y = y - 1.0, label = label), color = "white", size = 4, alpha = 0.6) +
    theme_void() +
    scale_fill_manual( 
        values = my_palette, 
        name = "Percent of workers in the arts area (%)", 
        guide = guide_legend(keyheight = unit(3, units = "mm"), 
                             keywidth = unit(12, units = "mm"), 
                             label.position = "bottom", title.position = 'top', nrow = 1) 
    ) +
    ggtitle("The Situation of Artists in each state of the USA") +
    labs(subtitle = st,
         caption = "Raymond Hung (Data: arts.gov) and Inspired by Nicola Rennie's tidytuesday repository under CC-BY-4.0 license") +
    theme(
        legend.position = c(0.5, 0.9),
        legend.background = element_rect(fill = "#f5f5f2", color = NA),
        legend.text = element_text(size = 13),
        text = element_text(color = "#22211d"),
        panel.background = element_rect(fill = "#f5f5f2", color = NA), 
        plot.background = element_rect(fill = "#f5f5f2", color = NA), 
        plot.title = element_text(size = 40, hjust = 0.5, color = "#4e4d47",
                                  margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
        plot.subtitle = element_text(hjust = 0.5,
                                     vjust = -1.5,
                                     family = "lato",
                                     size = 20,
                                     lineheight = 1.0),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), unit = "cm"),
    )


