library(data.table); library(ggplot2); 

#Download data
download.file('http://climexp.knmi.nl/data/inao.dat', './data/nao_cru.dat')

##Load data to environment
load('./data/clim_stations.rdata')
nao <- fread('./data/nao_cru.dat', skip = 7)

#Prepare data
nao <- nao[, 1:13]
c_names <- c("year", "jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
colnames(nao) <- c_names

#Tidy data
nao_tidy <- melt(nao, id.vars = 'year', value.factor = TRUE)
colnames(nao_tidy)[2:3] <- c('month', 'nao')
nao_tidy[nao < -100, nao := NA] 

clim_stations <- nao_tidy[clim_stations, on = .(year, month)] #clim_stations

clim_stations <- clim_stations[complete.cases(clim_stations)] #remove missing values

ggplot(clim_stations[variable == 'temp' & (nao > 2 | nao < -2)], aes(x = nao, y = values)) +
  geom_point(col = "dark red") +
  geom_density2d( col = "black") +
  facet_wrap(month ~ station, scales = 'free_y', nrow = 3) +
  theme_minimal()

to_plot <- clim_stations[variable == 'temp' & year >= 1900 & station == 'oslo' & (nao > 2 | nao < -2)]

to_plot[year > 1950, period := '1950-']
to_plot[year <= 1950, period := '-1950']
  
  ggplot(to_plot, aes(nao, values)) +
  geom_point(aes(col = period)) +
  geom_density2d( col = "black") +
  facet_wrap(month ~ station, scales = 'free_y', nrow = 3) +
  theme_minimal()
