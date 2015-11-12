# Accessing Insights Data through the R environment

This project contains sample code for use with New Relic Insights, as well as some sample charts and analyses.

* `insights_api.R` - A helper utility for getting data using the
  Insights HTTP api and transforming it into data frames.  Currently
  only event and faceted queries are supported.  Nothing yet for
  timeseries queries.
* `distribution_analysis.R` - some code for exploring the distribution of page view response times.
* `graph.R` - An example of building a page flow graph using the `igraph` library.
* `maps.R` - Sample code to populate a US map with attributes from Insights' PageEvents.

Other Files:
* `country_codes.R` data for use with the maps to translate country codes into names.
* `futurestack15` contains slides from my FutureStack 15 talk in the hacker lounge.
