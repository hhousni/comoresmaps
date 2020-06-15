if (!require("pacman")) install.packages("pacman")
pacman::p_load( dplyr, usethis, here, sp, sf)

# clean data

source(here::here("data-raw","raw_data.r"))
# this line allow us to read the script with the row-data


# fuction to convert a Data frame to a sf object
df_to_sf <- function(x) {
  x_sf <- st_as_sf(x, coords = c("long","lat"), sf_column_name = "geometry")
  x_sf_cast <- st_cast (st_combine(x_sf$geometry),"POLYGON")
  st_crs(x_sf_cast) <- "+proj=longlat +datum=WGS84 +no_defs"
  return(x_sf_cast)
}

##### apply the sf files ####
Djando_sf <- df_to_sf(Djando)
Domoni_sf <- df_to_sf(Domoni)
Fomboni_sf <- df_to_sf(Fomboni)
Hamahamet.Mboinkou_sf <- df_to_sf(Hamahamet.Mboinkou)
Hambou_sf <- df_to_sf(Hambou)
Itsandra.Hamanvou_sf <- df_to_sf(Itsandra.Hamanvou)
Kartala_sf <- df_to_sf(Kartala)
Mbadjini.Est_sf <- df_to_sf(Mbadjini.Est)
Mbadjini.Ouest_sf <- df_to_sf(Mbadjini.Ouest)
Mitsamiouli.Mboudé_sf <- df_to_sf(Mitsamiouli.Mboudé)
Moroni.Bambao_sf <- df_to_sf(Moroni.Bambao)
Mrémani_sf <- df_to_sf(Mrémani)
Mutsamudu_sf <- df_to_sf(Mutsamudu)
Nioumachioi_sf <- df_to_sf(Nioumachioi)
Oichili.Dimani_sf <- df_to_sf(Oichili.Dimani)
Ouani_sf <- df_to_sf(Ouani)
Sima_sf <- df_to_sf(Sima)
anjouan_state_sf <- df_to_sf(anjouan_state)
grande_comores_state_sf <- df_to_sf(grande_comore_state)
moheli_state_sf <- df_to_sf(moheli_state)



##### Create Data Frame for each region ####
Djando_df <- data.frame(NAME=c("Djando"))
Domoni_df <- data.frame(NAME=c("Domoni"))
Fomboni_df <- data.frame(NAME=c("Fomboni"))
Hamahamet.Mboinkou_df <- data.frame(NAME=c("Hamahamet.Mboinkou"))
Hambou_df <- data.frame(NAME=c("Hambou"))
Itsandra.Hamanvou_df <- data.frame(NAME=c("Itsandra.Hamanvou"))
Kartala_df <- data.frame(NAME=c("Kartala"))
Mbadjini.Est_df <- data.frame(NAME=c("Mbadjini.Est"))
Mbadjini.Ouest_df <- data.frame(NAME=c("Mbadjini.Ouest"))
Mitsamiouli.Mboudé_df <- data.frame(NAME=c("Mitsamiouli.Mboudé"))
Moroni.Bambao_df <- data.frame(NAME=c("Moroni.Bambao"))
Mrémani_df <- data.frame(NAME=c("Mrémani"))
Mutsamudu_df <- data.frame(NAME=c("Mutsamudu"))
Oichili.Dimani_df <- data.frame(NAME=c("Oichili.Dimani"))
Nioumachioi_df <- data.frame(NAME="Nioumachioi")
Ouani_df <- data.frame(NAME=c("Ouani"))
Sima_df <- data.frame(NAME=c("Sima"))
anjouan_state_df <- data.frame(NAME ="anjouan_state")
grande_comores_state_df <- data.frame(NAME ="grande_comore_state")
moheli_state_df <- data.frame(NAME ="moheli_state")

##### join the sf objects to data frames ####

st_geometry(Djando_df) <-Djando_sf
st_geometry(Domoni_df) <- Domoni_sf
st_geometry(Fomboni_df) <- Fomboni_sf
st_geometry(Hamahamet.Mboinkou_df) <- Hamahamet.Mboinkou_sf
st_geometry(Hambou_df) <- Hambou_sf
st_geometry(Itsandra.Hamanvou_df) <- Itsandra.Hamanvou_sf
st_geometry(Kartala_df) <- Kartala_sf
st_geometry(Mbadjini.Est_df) <- Mbadjini.Est_sf
st_geometry(Mbadjini.Ouest_df) <- Mbadjini.Ouest_sf
st_geometry(Mitsamiouli.Mboudé_df) <- Mitsamiouli.Mboudé_sf
st_geometry(Moroni.Bambao_df) <- Moroni.Bambao_sf
st_geometry(Mrémani_df) <- Mrémani_sf
st_geometry(Mutsamudu_df) <- Mutsamudu_sf
st_geometry(Oichili.Dimani_df) <- Oichili.Dimani_sf
st_geometry(Nioumachioi_df) <- Nioumachioi_sf
st_geometry(Ouani_df) <- Ouani_sf
st_geometry(Sima_df) <- Sima_sf
st_geometry(anjouan_state_df) <- anjouan_state_sf
st_geometry(grande_comores_state_df) <- grande_comores_state_sf
st_geometry(moheli_state_df) <- moheli_state_sf




##### Final Data ####

kmmap_data_v0 <- rbind(Djando_df,Domoni_df,Fomboni_df,Hamahamet.Mboinkou_df,Hambou_df,Itsandra.Hamanvou_df,Kartala_df,
                       Mbadjini.Est_df,Mbadjini.Ouest_df,Mitsamiouli.Mboudé_df,Moroni.Bambao_df,Mrémani_df,
                       Mutsamudu_df,Nioumachioi_df,Oichili.Dimani_df,Ouani_df,Sima_df,anjouan_state_df,grande_comores_state_df,moheli_state_df)

# Merging by Island
########
# anjouan_region <- kmmap_data_v0 %>% filter(NAME %in% c("Ouani","Domoni","Mrémani","Sima","Mutsamudu"))

# moheli_region <- kmmap_data_v0  %>% filter (NAME %in% c("Fomboni","Djando","Nioumachioi"))

# grande_comore_region <- kmmap_data_v0  %>% filter(NAME %in% c("Mitsamiouli.Mboudé","Hamahamet.Mboinkou","Itsandra.Hamanvou","Oichili.Dimani","Mbadjini.Est","Mbadjini.Ouest","Hambou","Kartala","Moroni.Bambao"))


comoros_sf <- kmmap_data_v0 %>%
  filter (NAME %in% c("anjouan_state","grande_comore_state","moheli_state")) %>%
  mutate(ID ="comoros") %>%
  group_by(ID) %>%
  summarise() %>%
  rename(NAME=ID)

km.map_data <- rbind(kmmap_data_v0,comoros_sf)


usethis::use_data(km.map_data, overwrite = TRUE)
