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

clim_stations[is.na(values)]



annual_means_tab <- dcast(annual_means, year ~ variable + station)
annual_means_cor <- cor(annual_means_tab[, -1], use = "complete.obs")

ann_m_m <- melt(annual_means_cor)

ggplot(ann_m_m) +
  geom_tile(aes(x = Var1, y = Var2, fill = values)) +
  geom_text(aes(x = Var1, y = Var2, label = round(values, 2)), col = 'white') +
  coord_fixed() +
  theme_bw()

library(corrplot)
library(RColorBrewer)


my_col <- rev(colorRampPalette(brewer.pal(11, "RdBu"))(100))
corrplot(annual_means_cor, col = my_col)

corrplot(annual_means_cor, 
         type = "upper", 
         tl.col = "black", 
         tl.pos = "d", 
         col = my_col)
corrplot(annual_means_cor, 
         add = TRUE, 
         type = "lower", 
         method = "number", 
         cl.lim = c(-1,1), 
         col = my_col, 
         diag = FALSE,
         tl.pos = "n", 
         cl.pos = "n")




monthly_means_tab <- dcast(monthly_means, year ~ variable + station)
monthly_means_cor <- cor(monthly_means_tab[, -1], use = "complete.obs")

month_m_m <- melt(monthly_means_cor)

ggplot(month_m_m) +
  geom_tile(aes(x = Var1, y = Var2, fill = values)) +
  geom_text(aes(x = Var1, y = Var2, label = round(values, 2)), col = 'white') +
  coord_fixed() +
  theme_bw()

library(corrplot)
library(RColorBrewer)


my_col <- rev(colorRampPalette(brewer.pal(11, "RdBu"))(100))
corrplot(monthly_means_cor, col = my_col)

corrplot(monthly_means_cor, 
         type = "upper", 
         tl.col = "black", 
         tl.pos = "d", 
         col = my_col)
corrplot(monthly_means_cor, 
         add = TRUE, 
         type = "lower", 
         method = "number", 
         cl.lim = c(-1,1), 
         col = my_col, 
         diag = FALSE,
         tl.pos = "n", 
         cl.pos = "n")


#Regression
oslo_temp_annual <- annual_means_tab[, c('year', 'temp_oslo')]
ggplot(data = oslo_temp_annual, aes(x = year, y = temp_oslo)) +
  geom_line() + 
  geom_smooth(method = 'lm', se = F) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme_bw()

fitted_model <- lm(temp_oslo ~ year, data = oslo_temp_annual)

resid <- data.table(year = oslo_temp_annual$year[as.numeric(names(residuals(fitted_model)))])
resid$residuals <- residuals(fitted_model)

ggplot(data = resid, aes(x = year, y = residuals)) +
  geom_point() + 
  geom_smooth(se = F) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme_bw()
