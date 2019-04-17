
library(dplyr)
drillhole = c('DH1','DH1','DH1','DH1','DH2','DH2')
from = c(365,366,367,368,401,402)
to = c(366,367,368.5,369,402.7,403)
east = c(682582.864,
         682582.868,
         682582.872,
         682582.876,
         682582.974,
         682582.978)

north = c(6629844.831,
          6629844.841,
          6629844.851,
          6629844.860,
          6629845.080,
          6629845.090)

StartRL = c(-239.64, -240.64, -241.64, 
            -242.64, -331.63, -332.63)

df <- data.frame(drillhole, from, to, east, north, StartRL)
df

intervals <- df %>% 
    group_by(drillhole) %>%
    mutate(to1 = cumsum(to-from),
           temp = lag(to1),
           from1 = ifelse(is.na(lag(to1)),0,temp),
           dip = asin(lead(StartRL)-StartRL)/(((lead(east)-east)^2 + 
                                                   (lead(north)-north)^2 +
                                                   (lead(StartRL)-StartRL)^2)^0.5)*180/pi,
           dip = ifelse(is.na(dip),lag(dip),dip),
           azimuth = atan((lead(east)-east)/(lead(north)-north))*180/pi,
           azimuth = ifelse(is.na(azimuth),lag(azimuth),azimuth))

intervals
