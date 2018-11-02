library(data.table)
load('./data/temp_month.rdata')

temp_oslo_tidy <- melt(temp_oslo, id.vars = 'year', value.factor = TRUE)
colnames(temp_oslo_tidy)[2:3] <- c('month', 'temp')

temp_prague_tidy <- melt(temp_prague, id.vars = 'year', value.factor = TRUE)
colnames(temp_prague_tidy)[2:3] <- c('month', 'temp')

temp_athens_tidy <- melt(temp_athens, id.vars = 'year', value.factor = TRUE)
colnames(temp_athens_tidy)[2:3] <- c('month', 'temp')

temp_oslo_tidy[, station := factor('oslo')]
temp_athens_tidy[, station := factor('athens')]
temp_prague_tidy[, station := factor('prague')]

temp_stations <- rbind(temp_prague_tidy, temp_athens_tidy, temp_oslo_tidy)

temp_stations[temp < -100, temp := NA] # Set values -999.9 to NA (missing values)
setorder(temp_stations, station, year, month)

save(temp_stations, file = './data/temp_stations.rdata')

colnames(temp_stations)[3] <- 'values'
colnames(prcp_stations)[3] <- 'values'

temp_stations$variable <- "temp"

prcp_stations$variable <- "prcp"
prcp_stations

clim_stations <- rbind(temp_stations, prcp_stations)
clim_stations <- clim_stations[, c(4, 1, 2, 5, 3)]

save(clim_stations, file = './data/clim_stations.rdata')

clim_stations[, date := ymd(paste0(year, '-', month, '-01'), format = '%Y-%b-%d', locale = 'czech')]
monthly_means <- clim_stations[, .(mean(values, na.rm = T)), .(station, variable, month)]
annual_means <- clim_stations[, .(mean(values, na.rm = T)), .(station, variable, year)]

dt[, column_to_delete := NULL]


clim_stations[, date := ymd(paste0(year, '-', month, '-01'), format = '%Y-%b-%d', locale = 'czech')]

ggplot(data = clim_stations[is.na(values)], aes(x = year)) +
  geom_bar() + 
  facet_wrap(variable ~ station) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme_bw()

ggplot(data = clim_stations[is.na(values)], aes(x = month)) +
  geom_bar() + 
  facet_wrap(variable ~ station) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme_bw()
