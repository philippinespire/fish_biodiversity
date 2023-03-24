#### Tutorial from uc-r.github.io using example website ####
# Tutorial link: http://uc-r.github.io/scraping_HTML_tables
webpage2 <- read_html("http://www.bls.gov/web/empsit/cesbmart.htm")

tbls0 <- html_nodes(webpage2, "table") # Describes website sections manually named "Table#"
tbls1 <- html_nodes(webpage2, "tr")
tbls2 <- html_nodes(webpage2, "td")
tbls3 <- html_nodes(webpage2, "th")

# each of these elements bring info back



head(tbls0)
head(tbls1)
head(tbls2) #HTML script says <td headers=.. is under th id=...
head(tbls3) #

text0 <- html_text(tbls0)
head(text0)


#### Tutorial from uc-r.github.io using MPA website ####
# Tutorial link: http://uc-r.github.io/scraping_HTML_tables
library(rvest)
library(XML)

webpage <- read_html("http://www.database.mpasupportnetwork.com/")

ptbls1 <- html_nodes(webpage, "h3")
ptbls2 <- html_nodes(webpage, "div")
ptbls3 <- html_nodes(webpage, "style")
ptbls4 <- html_nodes(webpage, "dataTables")
ptbls5 <- html_nodes(webpage, "th")
ptbls6 <- html_nodes(webpage, "dataTables_info")

# None of these elements bring anything back

text0 <- html_text(tbls0)

head(ptbls1)
head(ptbls2)
head(ptbls3)


head(text0)

#### Tutorial from statsandr.com using MPA website
link <- "http://www.database.mpasupportnetwork.com/"
MPApage <- read_html(link)

MPA_css <- MPApage %>%
  html_elements(css = ".summary-class")

head(summaries_css)

#### ChatGPT ####
library(rvest)

url <- "http://www.database.mpasupportnetwork.com/"
webpage <- read_html(url)
element <- html_nodes(webpage, "td[style][class]")
text <- html_text(element)

head(element)


url <- "http://www.database.mpasupportnetwork.com/"
webpage <- read_html(url)
table <- html_table(webpage)[[1]]
text <- table[table == "Bacud Reef"]
