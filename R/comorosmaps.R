if (!require("pacman")) install.packages("pacman")
  pacman::p_load( dplyr, magrittr, here, sp, sf)

  comorosmap <- function(x="country", region=FALSE, data) {
    if (x=="country" & region==TRUE) {
      state <- comorosmaps_data %>%
        select(NAME, geometry) %>%
        filter (NAME %in% c("Ouani","Domoni","Mremani","Sima","Mutsamudu","Mitsamiouli.Mboude",
                            "Hamahamet.Mboinkou","Itsandra.Hamanvou","Oichili.Dimani","Mbadjini.Est",
                            "Mbadjini.Ouest","Hambou","Kartala","Moroni.Bambao",
                            "Fomboni","Djando","Nioumachioi"))
    } else if (x=="country" & region==FALSE) {
      state <- comorosmaps_data %>%
        select(NAME, geometry) %>%
        filter (NAME %in% c("comoros"))
    } else if (x=="states" & region==FALSE) {
      state <- comorosmaps_data %>%
        select(NAME, geometry) %>%
        filter (NAME %in% c("anjouan_state","moheli_state","grande_comore_state"))
    } else if (x=="grande comore" & region ==TRUE) {
      state <- comorosmaps_data %>%
        select(NAME, geometry) %>%
        filter (NAME %in% c("Mitsamiouli.Mboude","Hamahamet.Mboinkou","Itsandra.Hamanvou","Oichili.Dimani",
                            "Mbadjini.Est","Mbadjini.Ouest","Hambou","Kartala","Moroni.Bambao"))
    } else if (x=="grande comore" & region ==FALSE) {
      state <- comorosmaps_data %>%
        select(NAME, geometry) %>%
        filter (NAME %in% c("grande_comore_state"))
    } else if (x=="anjouan" & region ==TRUE) {
      state <- comorosmaps_data %>%
        select(NAME, geometry) %>%
        filter (NAME %in% c("Ouani","Domoni","Mremani","Sima","Mutsamudu"))
    } else if (x=="anjouan" & region == FALSE) {
      state <- comorosmaps_data %>%
        select(NAME, geometry) %>%
        filter (NAME %in% c("anjouan_state"))
    } else if (x=="moheli" & region ==TRUE) {
      state <- comorosmaps_data %>%
        select(NAME, geometry) %>%
        filter (NAME %in% c("Fomboni","Djando","Nioumachioi"))
    } else if (x=="moheli" & region == FALSE) {
      state <- comorosmaps_data %>%
        select(NAME, geometry) %>%
        filter (NAME %in% c("moheli_state"))
    } else

      state <- print("error")

    plot(sf::st_geometry(state))
  }

  comorosmaps.data <- function(x="country", region=FALSE, data) {
    if (x=="country" & region==TRUE) {
      state <- comorosmaps_data %>%
        select(NAME, geometry) %>%
        filter (NAME %in% c("Ouani","Domoni","Mremani","Sima","Mutsamudu","Mitsamiouli.Mboude",
                            "Hamahamet.Mboinkou","Itsandra.Hamanvou","Oichili.Dimani","Mbadjini.Est",
                            "Mbadjini.Ouest","Hambou","Kartala","Moroni.Bambao",
                            "Fomboni","Djando","Nioumachioi"))
    } else if (x=="country" & region==FALSE) {
      state <- comorosmaps_data %>%
        select(NAME, geometry) %>%
        filter (NAME %in% c("comoros"))
    } else if (x=="states" & region==FALSE) {
      state <- comorosmaps_data %>%
        select(NAME, geometry) %>%
        filter (NAME %in% c("anjouan_state","moheli_state","grande_comore_state"))
    } else if (x=="grande comore" & region ==TRUE) {
      state <- comorosmaps_data %>%
        select(NAME, geometry) %>%
        filter (NAME %in% c("Mitsamiouli.Mboude","Hamahamet.Mboinkou","Itsandra.Hamanvou","Oichili.Dimani",
                            "Mbadjini.Est","Mbadjini.Ouest","Hambou","Kartala","Moroni.Bambao"))
    } else if (x=="grande comore" & region ==FALSE) {
      state <- comorosmaps_data %>%
        select(NAME, geometry) %>%
        filter (NAME %in% c("grande_comore_state"))
    } else if (x=="anjouan" & region ==TRUE) {
      state <- comorosmaps_data %>%
        select(NAME, geometry) %>%
        filter (NAME %in% c("Ouani","Domoni","Mremani","Sima","Mutsamudu"))
    } else if (x=="anjouan" & region == FALSE) {
      state <- comorosmaps_data %>%
        select(NAME, geometry) %>%
        filter (NAME %in% c("anjouan_state"))
    } else if (x=="moheli" & region ==TRUE) {
      state <- comorosmaps_data %>%
        select(NAME, geometry) %>%
        filter (NAME %in% c("Fomboni","Djando","Nioumachioi"))
    } else if (x=="moheli" & region == FALSE) {
      state <- comorosmaps_data %>%
        select(NAME, geometry) %>%
        filter (NAME %in% c("moheli_state"))
    } else

      state <- print("error")

    return(state)
  }
