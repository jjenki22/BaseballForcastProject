library(dplyr)
library(rvest)

teams <- read.csv("Data/TeamsToScrape.csv", stringsAsFactors = FALSE)
baselink <- "https://www.baseball-reference.com/teams/tgl.cgi?team="

get_games <- function(table_name, url, team, year) {
  Sys.sleep(1)
  out = tryCatch({
    link = paste(url, team, "&t=p&year=", year, "#rowsum_desc", sep = "")
    tableResults <- read_html(link) %>% 
      html_table(fill=TRUE)
    tableReturn = tableResults[[2]]
    tableReturn$Team = team 
    tableReturn$Year = year
    output = list(table_name = tableReturn)
  }, error = function(e){
    print(paste(team, "_", year, sep = ""))
  })
  closeAllConnections()
  return(out)
}

all_games <- mapply(get_games, teams$TableName, baselink, teams$Team, teams$Year)

all_games_together <- do.call("rbind", all_games)

write.csv(all_games_together, file.path("data/", "BOS_NYY_2009_2019.csv"), row.names = FALSE)
