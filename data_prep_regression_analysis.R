library(data.table); library(ggplot2)

load('data/clim_stations.rdata')
load('data/precip_stations.rdata')

#Annual temperature for the cities

annual_means <- clim_stations[, mean(values), .(year, variable, station)] #estimate
annual_means_tab <- dcast(annual_means, year ~ variable + station)

oslo_temp_annual <- annual_means_tab[, c('year', 'temp_oslo')]

ggplot(data = oslo_temp_annual, aes(x = year, y = temp_oslo)) +
  geom_line() +
  geom_smooth(method = 'lm', se = F) +
  theme_minimal()

prague_temp_annual <- annual_means_tab[, c('year', 'temp_prague')]

ggplot(data = prague_temp_annual, aes(x = year, y = temp_prague)) +
  geom_line() +
  geom_smooth(method = 'lm', se = F) +
  theme_minimal()

athens_temp_annual <- annual_means_tab[, c('year', 'temp_athens')]

ggplot(data = athens_temp_annual, aes(x = year, y = temp_athens)) +
  geom_line() +
  geom_smooth(method = 'lm', se = F) +
  theme_minimal()

#Annual Precipitations for the cities

oslo_prcp_annual <- annual_means_tab[, c('year', 'prcp_oslo')]

ggplot(data = oslo_prcp_annual, aes(x = year, y = prcp_oslo)) +
  geom_line() +
  geom_smooth(method = 'lm', se = F) +
  theme_minimal()

prague_prcp_annual <- annual_means_tab[, c('year', 'prcp_prague')]

ggplot(data = prague_prcp_annual, aes(x = year, y = prcp_prague)) +
  geom_line() +
  geom_smooth(method = 'lm', se = F) +
  theme_minimal()

athens_prcp_annual <- annual_means_tab[, c('year', 'prcp_athens')]

ggplot(data = athens_prcp_annual, aes(x = year, y = prcp_athens)) +
  geom_line() +
  geom_smooth(method = 'lm', se = F) +
  theme_minimal()

# residuals of the linear regression behave by fitting a linear model in our data

fitted_model <- lm(temp_oslo ~ year, data = oslo_temp_annual)

resid <- data.table(year = oslo_temp_annual$year[as.numeric(names(residuals(fitted_model)))])
resid$residuals <- residuals(fitted_model)

ggplot(data = resid, aes(x = year, y = residuals)) +
  geom_point() + 
  geom_smooth(se = F) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme_bw()

oslo <- clim_stations[station == 'oslo']
oslo <- dcast(oslo, year + month ~ variable + station )

ggplot(oslo, aes(x = temp_oslo, y = prcp_oslo)) +
  geom_point() + 
  geom_smooth(method = 'lm', se = F) +
  facet_wrap( ~ month, scales = 'free', nrow = 4)
