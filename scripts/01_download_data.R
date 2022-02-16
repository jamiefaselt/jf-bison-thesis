library(googledrive)
options(
  gargle_oauth_cache = ".secrets",
  gargle_oauth_email = TRUE
)


# Download Wyoming Parcel Data --------------------------------------------


folder_url <- "https://drive.google.com/drive/folders/1j4paghd9NM2wmXo8BSSr_LrTPVrXbWWm" #link to folder not the sharing link
folder <- drive_get(as_id(folder_url))
gdrive_files <- drive_ls(folder)
#have to treat the gdb as a folder and download it into a gdb directory in order to deal with the fact that gdb is multiple, linked files
lapply(gdrive_files$id, function(x) drive_download(as_id(x),
                                                   path = paste0(here::here("data/original/Wyoming_Parcels/"), gdrive_files[gdrive_files$id==x,]$name), overwrite = TRUE))


# Download HSI ------------------------------------------------------------
folder_url <- "https://drive.google.com/drive/folders/15JnhlU5WDH26fir0vXCoLdCXyWmeOJms" #link to folder not the sharing link
folder <- drive_get(as_id(folder_url))
gdrive_files <- drive_ls(folder)
#have to treat the gdb as a folder and download it into a gdb directory in order to deal with the fact that gdb is multiple, linked files
lapply(gdrive_files$id, function(x) drive_download(as_id(x),
                                                   path = paste0(here::here("data/original/SUMMER_HSI_clip/"), gdrive_files[gdrive_files$id==x,]$name), overwrite = TRUE))


# Download WY Padus -------------------------------------------------------

folder_url <- "https://drive.google.com/drive/folders/1zxMAZvWxL_1_kMB6oyuo6ZHlQOicQuJl" #describe where you downloaded
folder <- drive_get(as_id(folder_url))
gdrive_files <- drive_ls(folder)
#have to treat the gdb as a folder and download it into a gdb directory in order to deal with the fact that gdb is multiple, linked files
lapply(gdrive_files$id, function(x) drive_download(as_id(x),
                                                   path = paste0(here::here("data/original/PADUS2_1_StateWY_Shapefile/"), gdrive_files[gdrive_files$id==x,]$name), overwrite = TRUE))

