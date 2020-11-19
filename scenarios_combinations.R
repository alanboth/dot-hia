
                            
                            
age=c("18 to 40", "41 to 65", "66 plus", "all")
sex=c('male', 'female', 'all')
walking=c("0km",">1km", "1-2km", "3-5km", "6-10km", ">10km")
cycling=c("0km",">1km", "1-2km", "3-5km", "6-10km", ">10km")
trip_purpose=c("Leisure", "Shopping", "Work related", "Education", "Other")

# "Pick-up or drop-off someone/something", "personal business", "Other", "accompany someone", "education","at or go home")



scenarios <- expand.grid(age = age, sex = sex, walking = walking, cycling = cycling, trip_purpose = trip_purpose) %>%
                            dplyr::filter(walking!=cycling)