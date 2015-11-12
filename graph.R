# Graphing page navigation
# Plot the sessions as directed page flow graphs
# See http://kateto.net/network-visualization for a good overview of network graphs in R

library(igraph)

# Pick a random session with at least 6 pages:
repeat {
    session <- sample(sessions_list,1)[[1]]
    if (nrow(session) > 5) break
}

# Compile the list of transitions for a given session from page to page.
# Return a data frame of from, to, delay, session
get_transitions <- function(session_events) {
    n <- nrow(session_events)
    if (n<2) return(data.frame())
    from <- vector('character')
    to <- vector('character')
    duration <- vector('numeric')
    for (i in 2:n) {
        from[i-1] <- as.character(session_events[i-1, 'name'])
        to[i-1] <- as.character(session_events[i, 'name'])
        duration[i-1] <- session_events[i, 'duration']
    }
    session_id <- as.character(session_events$session[1])
    return(data.frame(from=from, to=to, session=c(session_id), duration=duration))
}

edges <- get_transitions(session)
nodes <- 
    group_by(session, name) %>% 
    summarize(count=n(), duration=median(duration)) %>% 
    mutate(name=as.character(name))

# Show the generic plot:
graph <- graph.data.frame(edges, nodes, directed=T)
plot(graph)

# Try a cleaned up plot:

V(graph)$label.color <- "black"
V(graph)$label.family <- 'Helvetica'
V(graph)$label.color <- 'black'
V(graph)$label.cex <- 0.8
V(graph)$label.dist <- 0.5
V(graph)$label.degree <- pi/2
V(graph)$color <- 'lightblue'
V(graph)$size <- 20
E(graph)$color <- '#CCCCCC'
E(graph)$curved <- T
E(graph)$arrow.size <- 1
E(graph)$width <- 1.0

plot(graph, layout=layout.kamada.kawai)

# Add some attributes:
scale <- function(domain, range) {
    b <- (range[2] - range[1]) / (max(domain) - min(domain))
    a <- range[1] - b * min(domain)
    sapply(domain, function(d) { a + b * d })
}

# Plot with size proportional to count
V(graph)$size <- scale(nodes$count, c(10,25))
plot(graph, layout=layout.kamada.kawai)

# Set edge colors to response time
library(RColorBrewer)
V(graph)$size=15
pal <- colorRampPalette(c('gray', 'red'))
E(graph)$color <- pal(10)[scale(edges$duration, c(1,10))]
plot(graph, layout=layout.kamada.kawai)

plot(graph, layout=layout.kamada.kawai)
plot(graph, layout=layout.circle)
plot(graph, layout=layout.grid)


# Preview all the layouts
layouts <- grep("^layout\\.", ls("package:igraph"), value=TRUE) 
# Remove layouts that do not apply to our graph.
layouts <- layouts[!grepl("bipartite|merge|norm|sugiyama", layouts)]

# Set panel layout to 3 by 9
par(mfrow=c(3,3))
for (layout in layouts) {
    print(layout)
    l <- do.call(layout, list(graph)) 
    plot(graph, vertex.label=NA, edge.arrow.mode=0, layout=l, main=layout) 
}

# reset the panel layout
par(mfrow=c(1,1))


# Aggregate all transitions for all sessions     

edges <- lapply(sessions_list, get_transitions) %>% rbind.fill()
nodes <- pages %>%
    group_by(name) %>% 
    summarize(count=n()) %>%
    mutate(name=as.character(name))

plot(graph.data.frame(edges, nodes, directed=T))

