km.data <- function(data=km.map_data, x="country",region=FALSE) {
  if (x=="country" & region==TRUE) {
    state <- km.map_data %>%
      select(NAME, geometry) %>%
      filter (NAME %in% c("Ouani","Domoni","Mrémani","Sima","Mutsamudu","Mitsamiouli.Mboudé",
                          "Hamahamet.Mboinkou","Itsandra.Hamanvou","Oichili.Dimani","Mbadjini.Est",
                          "Mbadjini.Ouest","Hambou","Kartala","Moroni.Bambao",
                          "Fomboni","Djando","Nioumachioi"))

  } else if (x=="country" & region==FALSE) {
    state <- km.map_data %>%
      select(NAME, geometry) %>%
      filter (NAME %in% c("comoros"))

  } else if (x=="states" & region==FALSE) {
    state <- km.map_data %>%
      select(NAME, geometry) %>%
      filter (NAME %in% c("anjouan_state","moheli_state","grande_comore_state"))

  } else if (x=="grande comore" & region ==TRUE) {
    state <- km.map_data %>%
      select(NAME, geometry) %>%
      filter (NAME %in% c("Mitsamiouli.Mboudé","Hamahamet.Mboinkou","Itsandra.Hamanvou","Oichili.Dimani",
                          "Mbadjini.Est","Mbadjini.Ouest","Hambou","Kartala","Moroni.Bambao"))
  } else if (x=="grande comore" & region ==FALSE) {
    state <- km.map_data %>%
      select(NAME, geometry) %>%
      filter (NAME %in% c("grande_comore_state"))

  } else if (x=="anjouan" & region ==TRUE) {
    state <- km.map_data %>%
      select(NAME, geometry) %>%
      filter (NAME %in% c("Ouani","Domoni","Mrémani","Sima","Mutsamudu"))
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
  plot(sf::st_geometry(state))
}
