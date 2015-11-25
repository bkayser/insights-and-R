library(httr)
library(plyr)
library(dplyr)

# Utility to access Insights through the Web API
#
# Example:
# api <- insights(account_id, api_key)
# response <- api.call('select count(*) from PageView TIMESERIES AUTO')
# 
#
insights <- function(account_id, api_key, app_id) {
    
    call <- function(nrql_query) {
        message(paste("Query:", nrql_query))
        response <- GET(paste("https://insights-api.newrelic.com/v1/accounts/",
                              account_id, "/query", sep = ''),
                        query=list(nrql=nrql_query),
                        as='text',
                        accept("application/json"),
                        add_headers('X-Query-Key'=api_key))
        result <- content(response)
        if (!is.null(result$error)) {
            stop("Error in response: ", result$error)
        }
        if (!is.null(result$facets)) {
            ldply(result$facets, as.data.frame)
        } else if(names(result$results[[1]])[1] == 'events') {
            ldply(result$results[[1]]$events, as.data.frame)      
        } else {
            stop("Unsupported result type; only facets and events supported now.")
        }
    }
    
    get_top_session_ids <- function(app_id, from=10, to=6, size=50) {
        session_ids <- vector('character')
        # Get the users with their pages:
        df <- call(paste('select count(*) from PageView ',
                         'where appId =', app_id,
                         'since', from, 'hours ago until', 
                         to, 'hours ago facet session',
                         'limit', size))
        return(df$name)
    }
    
    # Different from get top in that it runs a query to ensure the sessions have at least several
    # different pages
    get_real_session_ids <- function(app_id, size=50, hours_ago=24) {
        session_ids <- vector('character')
        while (size > 0) {
            limit <- min(c(size, 60))
            # Get the users with their pages:
            # select uniques(name) from PageView where last_transaction != name and auth_mechanism='login_from_login_service' facet user limit 100
            until_minutes_ago <- hours_ago * 60
            
            v <- api$call(paste('select uniques(name) from PageView ',
                                'where appId =', app_id,
                                "and last_transaction != name", # and auth_mechanism='login_from_login_service'", 
                                'since', until_minutes_ago + 30, "minutes ago",
                                'until', until_minutes_ago, "minutes ago",
                                'limit', limit))
            session_ids <- append(session_ids, as.character(v$name))
            until_minutes_ago <- until_minutes_ago + 30
            size <- size - nrow(v)
        }
        unique(session_ids)
    }
    
    get_sessions <- function(session_ids, limit=750) {
        sessions <- list()
        
        for(session in session_ids) {
            events <- call(paste("select * from PageView",
                                 paste("where session='",session,"'",sep=''),
                                 'since 24 hours ago', 
                                 'limit', limit))
            if (nrow(events) < limit) {
                message(paste("Session:", session, "-", nrow(events), "events"))
                sessions[[session]] <- postprocess(events)
            } else {
                message(paste("Skipped",session, "because there were", nrow(events), "events."))
            }
        }
        sessions
    }
    
    # Return a list of all three functions
    list(call=call, 
         get_top_session_ids=get_top_session_ids,
         get_real_session_ids=get_real_session_ids,
         get_sessions=get_sessions)
}

## Utility functions

insights_timestamp <- function(t) {
    as.POSIXct(t/1000, origin="1970-01-01")
}
postprocess <- function(events) {
    # Remove uninteresting fields
    v <- select(events,
                -browserTransactionName,
                -appId,
                -appName) %>%
        # Convert unix timestamp to posix time object
        mutate(timestamp=insights_timestamp(timestamp),
               name=gsub('^(WebTransaction/(JSP/|Servlet/)|Controller/)', '', name)) %>%
        # Sort by timestamp
        arrange(timestamp)
    if (nrow(v) > 1) {
        for (i in 1:(nrow(v)-1)) {
            s <- v[i+1,'timestamp'] - v[i,'timestamp']
            d <- v[i, 'duration']
            if (s > d) {
                v[i,'think'] <- (s - d)
            }
        }
    }
    return(v)
}