#' @description  reading in the dataset that might be a text file or csv file
#' @param file it represents the path to the file that contains the dataset
#' @param ext_tracks_widths it specifies the width for each column in the dataset
#' @param ext_tracks_colnames it specifies the name for each column in the dataset
#' @importFrom readr read_fwf
#' @importFrom readr fwf_widths
#' @return ext_tracks a dataframe that will be passed and manipulated to get a long format observation to be plotted
#' @export
ext_tracks_widths <- c(7, 10, 2, 2, 3, 5, 5, 6, 4, 5, 4, 4, 5, 3, 4, 3, 3, 3,
                       4, 3, 3, 3, 4, 3, 3, 3, 2, 6, 1)
ext_tracks_colnames <- c("storm_id", "storm_name", "month", "day",
                         "hour", "year", "latitude", "longitude",
                         "max_wind", "min_pressure", "rad_max_wind",
                         "eye_diameter", "pressure_1", "pressure_2",
                         paste("radius_34", c("ne", "se", "sw", "nw"), sep = "_"),
                         paste("radius_50", c("ne", "se", "sw", "nw"), sep = "_"),
                         paste("radius_64", c("ne", "se", "sw", "nw"), sep = "_"),
                         "storm_type", "distance_to_land", "final")

ext_tracks <- readr::read_fwf("ebtrk_atlc_1988_2015.txt",
                              readr::fwf_widths(ext_tracks_widths, ext_tracks_colnames),
                              na = "-99")
#' @description transforming the data to be in a tidy format using dplyr functions
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom lubridate make_datetime
#' @importFrom magrittr %>%
#' @return final_hurricane_df a tidy dataframe for all storms in the state
#' @export
hurricane_df <- ext_tracks %>%
  dplyr::select(storm_id,storm_name,month,day,hour,year,longitude,latitude, dplyr::starts_with("radius_")) %>%
  dplyr::mutate(storm_id = paste(storm_name, year, sep = "-"),
                date = lubridate::make_datetime(year = as.integer(year),
                                                month = as.integer(month),
                                                day = as.integer(day),
                                                hour = as.integer(hour),
                                                tz = "UTC"),
                longitude = -longitude)
final_hurricane_df <- hurricane_df %>%
  dplyr::select(storm_id,date,latitude,longitude, dplyr::starts_with("radius_"))
#' @description transforming the returning tidy dataframe into a long format so you can get a single observation time for a single storm
#' @importFrom dplyr select
#' @importFrom dplyr rename
#' @importFrom dplyr mutate
#' @importFrom dplyr starts_with
#' @importFrom plyr arrange
#' @return wind_speed_df dataframe in a long format that specifies the storm id and the time when this storm occured and the wind speed in all directions
#' @export
wind_speed_34 <- final_hurricane_df %>%
  dplyr::select(storm_id,date,latitude,longitude, dplyr::starts_with("radius_34_")) %>%
  dplyr::rename(ne = radius_34_ne ,se = radius_34_se ,sw = radius_34_sw , nw = radius_34_nw )%>%
  dplyr::mutate(wind_speed = 34)

wind_speed_50 <- final_hurricane_df %>%
  dplyr::select(storm_id,date,latitude,longitude,dplyr::starts_with("radius_50_")) %>%
  dplyr::rename(ne = radius_50_ne ,se = radius_50_se ,sw = radius_50_sw , nw = radius_50_nw )%>%
  dplyr::mutate(wind_speed = 50)

wind_speed_64 <- final_hurricane_df %>%
  dplyr::select(storm_id,date,latitude,longitude,dplyr::starts_with("radius_64_")) %>%
  dplyr::rename(ne = radius_64_ne ,se = radius_64_se ,sw = radius_64_sw , nw = radius_64_nw )%>%
  dplyr::mutate(wind_speed = 64)
wind_speed_df <- rbind(wind_speed_34,wind_speed_50,wind_speed_64) %>%
  plyr::arrange(storm_id,date,wind_speed)

#' @description the class to create a hurricane wind radii chart for a single storm observation
#' @title GeomHurricane
#' @importFrom ggplot2 ggproto
#' @importFrom ggplot2 draw_key_polygon
#' @importFrom dplyr as_tibble
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr rename
#' @importFrom magrittr %>%
#' @importFrom geosphere destPoint
#' @importFrom grid polygonGrob
#' @importFrom grid gpar
#' @importFrom ggplot2 Geom
#' @export
GeomHurricane <- ggplot2::ggproto("GeomHurricane",
                                  ggplot2::Geom,
                                  required_aes = c("x", "y",
                                                   "r_ne","r_se","r_sw",  "r_nw"),
                                  default_aes = ggplot2::aes(colour  = "black",
                                                             fill        = "black",
                                                             linetype    = 0,
                                                             alpha       = 0.65,
                                                             scale_radii = 1.0),
                                  draw_key = ggplot2::draw_key_polygon,
                                  draw_group = function(data,
                                                        panel_scales,
                                                        coord) {
                                    hurricane_data <- dplyr::as_tibble()
                                    center_hurricane       <- dplyr::as_tibble()
                                    data %>% dplyr::mutate(fill = fill,
                                                           colour = colour)
                                    center_hurricane <- data %>%
                                      dplyr::select(lon = x, lat = y)
                                    radius_hurricane <- data %>% dplyr::select(r_ne,r_se,
                                                                               r_sw,r_nw) %>%
                                      dplyr::mutate(r_ne = data$scale_radii * r_ne * 1852,
                                                    r_se = data$scale_radii * r_se * 1852,
                                                    r_sw = data$scale_radii * r_sw * 1852,
                                                    r_nw = data$scale_radii * r_nw * 1852)
                                    for (counter1 in 1:4)
                                    {
                                      for (counter2 in 1:nrow(data))
                                      {
                                        hurricane_data <- geosphere::destPoint(c(x = center_hurricane[counter2,1],
                                                                                 y = center_hurricane[counter2,2]),
                                                                               b = ((counter1-1)*90):(90*counter1),
                                                                               d = radius_hurricane[counter2,counter1]) %>%
                                          rbind(c(x = center_hurricane[counter2,1],
                                                  y = center_hurricane[counter2,2])) %>%
                                          rbind(hurricane_data)
                                      }
                                      coords <- hurricane_data %>%
                                        dplyr::as_tibble() %>%
                                        dplyr::rename(x = lon,y = lat) %>%
                                        coord$transform(panel_scales)
                                    }
                                    grid::polygonGrob(x = coords$x,
                                                      y = coords$y,
                                                      default.units = "native",
                                                      gp = grid::gpar(col = data$colour, fill = data$fill,
                                                                      alpha = data$alpha, lty = 1,
                                                                      scale_radii = data$scale_radii))
                                  }
)
#' @description a wind radii geom that shows a polygon for the three levels of the wind according to wind radii provided in the storm observation
#' @title geom_hurricane
#' @section aesthetics :
#' \itemize{
#'   \item \strong{x} : the longitude of the center of the storm
#'   \item \strong{y} : the latitude of the center of the storm
#'   \item \strong{r_ne} : the wind radii in the northeast
#'   \item \strong{r_se} : the wind radii in the southeast
#'   \item \strong{r_nw} : the wind radii in the northwest
#'   \item \strong{r_sw} : the wind radii in the southwest
#'   \item colour
#'   \item fill
#'   \item size
#'   \item linetype
#'   \item alpha
#'   \item \strong{scale_radii} the maximum radii value
#' }
#' @usage
#' \dontrun {
#' map_plot <- ggmap::get_map("Lousiana", zoom = 5, maptype = "toner-background")
#' map_plot %>%
#'   ggmap::ggmap(extent = "device") +
#'   geom_hurricane(data = storm_observation_ike,
#'                  ggplot2::aes(x = longitude, y = latitude,
#'                               r_ne = ne, r_se = se, r_nw = nw, r_sw = sw,
#'                               color = wind_speed, fill = wind_speed)) +
#'   ggplot2::scale_color_manual(name = "Wind speed (kts)",
#'                               values = c("red", "orange", "yellow")) +
#'   ggplot2::scale_fill_manual(name = "Wind speed (kts)",
#'                              values = c("red", "orange", "yellow"))
#' }
#' @importFrom ggplot2 layer
#' @export
geom_hurricane <- function(mapping = NULL,
                           data = NULL,
                           stat = "identity",
                           position = "identity",
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE, ...){
  ggplot2::layer(geom = GeomHurricane,
                 mapping = mapping,
                 data = data,
                 stat = stat,
                 position = position,
                 show.legend = show.legend,
                 inherit.aes = inherit.aes,
                 params = list(na.rm = na.rm,...)
  )
}

storm_observation <- wind_speed_df[wind_speed_df$storm_id == "KATRINA-2005" &
                                      wind_speed_df$date == ymd_hms("2005-08-29 12:00:00"),
storm_observation$wind_speed <- as.factor(storm_observation$wind_speed)
get_map("Louisiana", zoom = 6, maptype = "toner-background") %>%
  ggmap(extent = "device") +
  geom_hurricane(data = storm_observation,
                 aes(x = longitude, y = latitude,
                     r_ne = ne, r_se = se, r_nw = nw, r_sw = sw,
                     fill = wind_speed, color = wind_speed)) +
  scale_color_manual(name = "Wind speed (kts)",
                     values = c("red", "orange", "yellow")) +
  scale_fill_manual(name = "Wind speed (kts)",
                    values = c("red", "orange", "yellow"))


