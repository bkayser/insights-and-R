source('insights_api.R')

# Load in data for an account

install.packages('ggplot2')
# Create the API helper

api <- insights(account_id, api_key)

# API returns three methods:
names(api)

# Get top sessions
session_ids <- api$get_top_session_ids(app_id = app_id, size=80)
session_ids <- api$get_real_session_ids(app_id = app_id, size=80)

# This returns a list of sessions; that is to say, a list
# of data frames, one per session, of the complete list of page views
# in that particular session.
sessions_list <- api$get_sessions(session_ids)

# Combine the list of sessions into a single data frame
pages <- rbind.fill(sessions_list)

# Save the pages:
# save(sessions_list, session_ids, account_id, api_key, app_id, file='./accounts/account.RData')

# Load the pages:
# load('./accounts/account.RData')

# Show the summary information for countries and regions:
table(pages$countryCode)
table(pages$userAgentName)

table(pages$countryCode, pages$userAgentName)

# Show the summary information for the duration:
summary(pages$duration)

# Show the summary stats for the duration, by region
tapply(pages$duration, pages$countryCode, summary)

# Let's try a histogram of page counts
qplot(name, data=pages)

# Flip the coordinates so you can read the transaction:
qplot(name, data=pages) + coord_flip()

# Try a bar plot of means
means<-tapply(pages$duration, pages$countryCode, mean, na.rm=T) %>% sort(decreasing=T)
barplot(means, col='lightgreen')
abline(h=mean(pages$duration, na.rm=T), col='red', lwd=4)

# Try looking at the duration
qplot(name, y=duration, data=pages) + coord_flip()

# Try a boxplot
qplot(name, y=duration, data=pages, geom='boxplot') + coord_flip()

      