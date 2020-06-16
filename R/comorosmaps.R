comorosmaps_data <- function(x="country",region=FALSE) {
  if (x=="states" & region==FALSE) {
    state <- km.map_data %>%
      select(NAME, geometry) %>%
      filter (NAME %in% c("anjouan_state","moheli_state","grande_comore_state"))

    } else if (x=="anjouan" & region ==TRUE) {
    state <- km.map_data %>%
      select(NAME, geometry) %>%
      filter (NAME %in% c("Ouani","Domoni","Sima","Mutsamudu"))
  } else if (x=="anjouan" & region == FALSE) {
    state <- km.map_data %>%
      select(NAME, geometry) %>%
      filter (NAME %in% c("anjouan_state"))

  } else if (x=="moheli" & region ==TRUE) {
    state <- km.map_data %>%
      select(NAME, geometry) %>%
      filter (NAME %in% c("Fomboni","Djando","Nioumachioi"))
  } else if (x=="moheli" & region == FALSE) {
    state <- km.map_data %>%
      select(NAME, geometry) %>%
      filter (NAME %in% c("moheli_state"))
  } else

    state <- print("error")
  return (plot(sf::st_geometry(state)))
}


hello <- function(){
  print("sa marche")
}
