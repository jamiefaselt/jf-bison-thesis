library(googledrive)
options(
  gargle_oauth_cache = ".secrets",
  gargle_oauth_email = TRUE
)


# Download Wyoming Parcel Data --------------------------------------------
folder_url <- "https://drive.google.com/drive/folders/1j4paghd9NM2wmXo8BSSr_LrTPVrXbWWm" # wyoming parcels from the states gis site
folder <- drive_get(as_id(folder_url))
gdrive_files <- drive_ls(folder)
lapply(gdrive_files$id, function(x) drive_download(as_id(x),
                                                   path = paste0(here::here("data/original/Wyoming_Parcels/"), gdrive_files[gdrive_files$id==x,]$name), overwrite = TRUE))


# Download HSI ------------------------------------------------------------
folder_url <- "https://drive.google.com/drive/folders/15JnhlU5WDH26fir0vXCoLdCXyWmeOJms" # Brent sent to me clipped
folder <- drive_get(as_id(folder_url))
gdrive_files <- drive_ls(folder)
lapply(gdrive_files$id, function(x) drive_download(as_id(x),
                                                   path = paste0(here::here("data/original/SUMMER_HSI_clip/"), gdrive_files[gdrive_files$id==x,]$name), overwrite = TRUE))


# Download WY Padus -------------------------------------------------------
folder_url <- "https://drive.google.com/drive/folders/1zxMAZvWxL_1_kMB6oyuo6ZHlQOicQuJl" #PADUS by state
folder <- drive_get(as_id(folder_url))
gdrive_files <- drive_ls(folder)
lapply(gdrive_files$id, function(x) drive_download(as_id(x),
                                                   path = paste0(here::here("data/original/PADUS2_1_StateWY_Shapefile/"), gdrive_files[gdrive_files$id==x,]$name), overwrite = TRUE))


# Download MT Padus -------------------------------------------------------
folder_url <- "https://drive.google.com/drive/u/0/folders/1sMMT4w3xu-89iDsIAbhhoyHF8NtTdt-R" #PADUS by state
folder <- drive_get(as_id(folder_url))
gdrive_files <- drive_ls(folder)
lapply(gdrive_files$id, function(x) drive_download(as_id(x),
                                                   path = paste0(here::here("data/original/PADUS2_1_StateMT_Shapefile/"), gdrive_files[gdrive_files$id==x,]$name), overwrite = TRUE))


# Download nps_boundaries -------------------------------------------------
folder_url <- "https://drive.google.com/drive/u/0/folders/1UqhtCHGwPg_hA6N_CRa39Sey3GsAJ1Nl" # boundaries for mt nps units
folder <- drive_get(as_id(folder_url))
gdrive_files <- drive_ls(folder)
lapply(gdrive_files$id, function(x) drive_download(as_id(x),
                                                   path = paste0(here::here("data/original/nps_boundaries/"), gdrive_files[gdrive_files$id==x,]$name), overwrite = TRUE))

# Download NASS Data ------------------------------------------------------
folder_url <- "https://drive.google.com/drive/u/0/folders/1zSSfIQP014m4VBQGr61wlBflYen_KN8V" # NASS data
folder <- drive_get(as_id(folder_url))
gdrive_files <- drive_ls(folder)
lapply(gdrive_files$id, function(x) drive_download(as_id(x),
                                                   path = paste0(here::here("data/original/NASS_data/"), gdrive_files[gdrive_files$id==x,]$name), overwrite = TRUE))

# Download Montana Cadastral Data ------------------------------------------------------
folder_url <- "https://drive.google.com/drive/u/0/folders/1eS5oLPjdjak6cag1BixNknOx5Th96JK0" # Parcel data
folder <- drive_get(as_id(folder_url))
gdrive_files <- drive_ls(folder)
lapply(gdrive_files$id, function(x) drive_download(as_id(x),
                                                   path = paste0(here::here("data/original/Montana_Cadastral/"), gdrive_files[gdrive_files$id==x,]$name), overwrite = TRUE))

# Download Montana Reservation Data ------------------------------------------------------
folder_url <- "https://drive.google.com/drive/u/0/folders/1lpinf7nttBux4Zz2h2VM6C-J7POrCdvr" # Reservatio shapefiles
folder <- drive_get(as_id(folder_url))
gdrive_files <- drive_ls(folder)
lapply(gdrive_files$id, function(x) drive_download(as_id(x),
                                                   path = paste0(here::here("data/original/mt_reservations/"), gdrive_files[gdrive_files$id==x,]$name), overwrite = TRUE))


# Download HMI for US Data ------------------------------------------------------
folder_url <- "https://drive.google.com/drive/folders/1uBKCAeZ4VStT7N0HYy-yd0IbEyrtEdtE" # HMI from Theobold
folder <- drive_get(as_id(folder_url))
gdrive_files <- drive_ls(folder)
lapply(gdrive_files$id, function(x) drive_download(as_id(x),
                                                   path = paste0(here::here("data/original/Human_Modification_Index/"), gdrive_files[gdrive_files$id==x,]$name), overwrite = TRUE))

# Download zillow land value Data  ------------------------------------------------------
folder_url <- "https://drive.google.com/drive/u/0/folders/1iNtwL0zchxn5KJ3FByJmj3RW9iR6Jevs" # Zillow land value from pnas/dryad
folder <- drive_get(as_id(folder_url))
gdrive_files <- drive_ls(folder)
lapply(gdrive_files$id, function(x) drive_download(as_id(x),
                                                   path = paste0(here::here("data/original/places_fmv_pnas_dryad/"), gdrive_files[gdrive_files$id==x,]$name), overwrite = TRUE))

# APR Shapefile -----------------------------------------------------------
folder_url <- "https://drive.google.com/drive/u/0/folders/13QUtUL7MKKQ_ap9kD2193ePyXDBBnVLA" # APR shapefiles from APR staff
folder <- drive_get(as_id(folder_url))
gdrive_files <- drive_ls(folder)
lapply(gdrive_files$id, function(x) drive_download(as_id(x),
                                                   path = paste0(here::here("data/original/AP_Property_Boundaries_011022/"), gdrive_files[gdrive_files$id==x,]$name), overwrite = TRUE))


# Kate's model outputs tifs ----------------------------------------------------
folder_url <- "https://drive.google.com/drive/u/0/folders/1kcbX5z58e4cPyN2USv6UryjUju2uPv1T" # raster layers for bison increase and intervention scenarios
folder <- drive_get(as_id(folder_url))
gdrive_files <- drive_ls(folder)
lapply(gdrive_files$id, function(x) drive_download(as_id(x),
                                                   path = paste0(here::here("data/wildlife_model_tifs/"), gdrive_files[gdrive_files$id==x,]$name), overwrite = TRUE))

# Voting data -----------------------------------------------------------
folder_url <- "https://drive.google.com/drive/u/0/folders/1makcSx5Y9LJMTXCQH3aaFTN2royioTtM" # voting stats from harvard voting lab
folder <- drive_get(as_id(folder_url))
gdrive_files <- drive_ls(folder)
lapply(gdrive_files$id, function(x) drive_download(as_id(x),
                                                   path = paste0(here::here("data/original/pres_voting/"), gdrive_files[gdrive_files$id==x,]$name), overwrite = TRUE))

################### Old / Temp Data ################
# Temp CS Output Data -----------------------------------------------------
# will eventually run these all on the remote computer but for workflow purposes doing this way now
#folder_url <- "https://drive.google.com/drive/u/0/folders/1Fn2EbSNZoJgFBMHqmEaCNxVd4kEkYGZL" # cs outputs temp solution
#folder <- drive_get(as_id(folder_url))
#gdrive_files <- drive_ls(folder)
#lapply(gdrive_files$id, function(x) drive_download(as_id(x),
                                                  # path = paste0(here::here("data/circuitscape_outputs/"), gdrive_files[gdrive_files$id==x,]$name), overwrite = TRUE))

# above wasn't working and i just neeeded the updated biophys layer for now
#folder_url <- "https://drive.google.com/drive/u/0/folders/1FsWGSTeabEgbo7IVkkVk_TeuZgxLy0xb" # cs outputs temp solution
#folder <- drive_get(as_id(folder_url))
#gdrive_files <- drive_ls(folder)
#lapply(gdrive_files$id, function(x) drive_download(as_id(x),
 #                                                  path = paste0(here::here("data/circuitscape_outputs/biophys_resistance_layer/"), gdrive_files[gdrive_files$id==x,]$name), overwrite = TRUE))


# Download Metadata for Montana Private Conservation Lands Data ------------------------------------------------------
#folder_url <- "https://drive.google.com/drive/u/0/folders/1-2qhFff5eDlY7sOyhcRC9xK_LDPBiqoD" # Metadata for Montana Private Conservation Lands
#folder <- drive_get(as_id(folder_url))
#gdrive_files <- drive_ls(folder)
#lapply(gdrive_files$id, function(x) drive_download(as_id(x),
 #                                                  path = paste0(here::here("data/original/LandMan_PrvtCons/"), gdrive_files[gdrive_files$id==x,]$name), overwrite = TRUE))


