suppressPackageStartupMessages(library(sf)) # for spatial things
suppressPackageStartupMessages(library(dplyr)) # for manipulating data

# Greater Melbourne
study_region <- st_read('/home/alan/DATA/Datasets/ABS/2016/ASGS_volume_1.sqlite',
                        layer='GCCSA_2016_AUST') %>%
  # st_drop_geometry() %>%
  filter(GCCSA_NAME_2016=='Greater Melbourne') %>%
  st_geometry()

# Just the parts of LGAs that are inside the study region
lga <- st_read('/home/alan/DATA/Datasets/ABS/2020/ASGS_volume_3.sqlite',
               layer='LGA_2020_AUST') %>%
  filter(STATE_NAME_2016=='Victoria') %>%
  st_intersection(study_region) %>%
  mutate(area=as.numeric(st_area(.))) %>%
  # st_drop_geometry() %>%
  filter(area>1000) %>%
  dplyr::select(lga_name=LGA_NAME_2020,lga_code=LGA_CODE_2020) %>%
  mutate(lga_code=as.integer(lga_code)) %>%
  st_transform(28355)
st_write(lga, './Data/vitm/lga.sqlite', delete_dsn=T)


# vitm regions
tzn <- st_read('./Data/vitm/vitm_regions.sqlite') %>%
  st_set_crs(28355) %>%
  dplyr::select(tzn) %>%
  st_filter(st_transform(study_region,28355), st_intersects=T)

# intersecting LGAs with vitm regions and calculating the area
lga_tzn <- st_intersection(tzn,lga) %>%
  st_make_valid() %>%
  mutate(area=as.numeric(st_area(.))) %>%
  dplyr::select(lga_name,lga_code,tzn,area)

# writing the output
st_write(lga_tzn, './Data/vitm/lga_intersect_vitm_regions.sqlite', delete_dsn=T)

