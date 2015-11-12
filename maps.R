# Mapping example
# Plot some of the session attributes in a map
library(maps)
library(ggplot2)
library(mapproj)

# Capture data for US, grouped by region
state_summary <- filter(pages, countryCode == 'US') %>%
    group_by(regionCode) %>%
    summarize(visits=n()) %>% 
    left_join(data.frame(cbind(state_name=tolower(state.name), regionCode=state.abb)))

state_map_data <- right_join(state_summary, map_data('state'), by=c('state_name'='region')) %>%
    arrange(order)

q_visits <- quantile(state_summary$visits, seq(0, 1, length.out=10))

levels <-  cut(state_map_data$visits,
               q_visits,
               labels=paste("Up to", round(q_visits[1:9]), "page views"),
               include.lowest = T)

pal <- brewer.pal(9, 'YlOrRd')

ggplot(state_map_data) +
    aes(x=long, y=lat, group=group, fill=levels) +
    scale_fill_manual(values=pal) +
    geom_polygon(fill='white', color='gray') +
    geom_polygon(color='black') +
    coord_map('globular') +
    ggtitle("Traffic Density by State")
