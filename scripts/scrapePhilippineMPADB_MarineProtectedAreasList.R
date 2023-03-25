# this script extracts the data from the Marine Protected Areas List table http://www.database.mpasupportnetwork.com/

#### Setting Up Your Computer, READ THIS! ####

# install latest jdk here (java 20 or newest) https://www.oracle.com/java/technologies/downloads/

# update chrome to the newest version (ask chatgpt how to do this)

# there were all kinds of problems with having the right versions of the drivers, java, and the webbrowser and having them match
# in all likelihood, when you try this, the code will not work because the versions have changed, so pay attn to those

# not a bad idea to update R, RStudio, and the packages used in this script (from github) 

# NOTE, this will only work if port 4445 is not being used by another program
# this can be checked in the windows command prompt
# netstat -ano | findstr :4445
# you can try other ports if you're getting errors, or kill the process using a port like this in the win cmd prompt: 
# taskkill /F /PID ChangeThisToTheTaskNum

#### THINGS I TRIED THAT DIDN'T SOLVE THE PROBLEM BUT MIGHT BE USEFUL IN THE FUTURE ####

# binman organizes the drivers for the webbrowsers and selenium, it would be possible to put drivers there manually
# C:\Users\cbird\AppData\Local\binman\binman_chromedriver\win32

# dowlodaed selenium-server-standalone-3.9.1.jar from https://selenium-release.storage.googleapis.com/index.html?path=3.9/

# in the windows terminal (not ubuntu, windows cmd), I navigated to the Downloads dir and ran
# C:\Users\cbird\Downloads>java -jar selenium-server-standalone-3.9.1.jar -port 4445

# this is the output of success:
# 09:24:45.594 INFO - Selenium build info: version: '3.9.1', revision: '63f7b50'
# 09:24:45.595 INFO - Launching a standalone Selenium Server on port 4445
# 2023-03-25 09:24:45.752:INFO::main: Logging initialized @335ms to org.seleniumhq.jetty9.util.log.StdErrLog
# 2023-03-25 09:24:45.855:INFO:osjs.Server:main: jetty-9.4.7.v20170914, build timestamp: 2017-11-21T15:27:37-06:00, git hash: 82b8fb23f757335bb3329d540ce37a2a2615f0a8
# 2023-03-25 09:24:45.878:WARN:osjs.SecurityHandler:main: ServletContext@o.s.j.s.ServletContextHandler@647e447{/,null,STARTING} has uncovered http methods for path: /
#   2023-03-25 09:24:45.881:INFO:osjsh.ContextHandler:main: Started o.s.j.s.ServletContextHandler@647e447{/,null,AVAILABLE}
# 2023-03-25 09:24:46.001:INFO:osjs.AbstractConnector:main: Started ServerConnector@7d6b223d{HTTP/1.1,[http/1.1]}{0.0.0.0:4445}
# 2023-03-25 09:24:46.002:INFO:osjs.Server:main: Started @585ms
# 09:24:46.002 INFO - Selenium Server is up and running on port 4445

# i ran into issues, so I tried getting the selenium server grid up and running  https://www.selenium.dev/downloads/
# i downloaded the gecko driver https://github.com/mozilla/geckodriver/releases which is needed by the server
# then I put it into the windows PATH C:\Users\cbird\AppData\Local\Microsoft\WindowsApps

# I was getting error because another task was using port, but I can't figure out how to specify another port
# the solution was to kill the task using the port with the following commands in the windows command prompt
# C:\Users\cbird\Downloads>netstat -ano | findstr :4444
# TCP    0.0.0.0:4444           0.0.0.0:0              LISTENING       21356
# TCP    [::]:4444              [::]:0                 LISTENING       21356
# 
# C:\Users\cbird\Downloads>taskkill /F /PID 21356
# SUCCESS: The process with PID 21356 has been terminated.

#Then I could run the server as follows at the windows command prompt
# C:\Users\cbird\Downloads>java -jar selenium-server-4.8.2.jar standalone
# 10:28:58.722 INFO [LoggingOptions.configureLogEncoding] - Using the system default encoding
# 10:28:58.727 INFO [OpenTelemetryTracer.createTracer] - Using OpenTelemetry for tracing
# 10:29:00.715 INFO [NodeOptions.getSessionFactories] - Detected 8 available processors
# 10:29:00.733 INFO [NodeOptions.discoverDrivers] - Driver(s) already present on the host: 1
# 10:29:00.757 INFO [NodeOptions.report] - Adding Firefox for {"browserName": "firefox"} 8 times (Host)
# 10:29:00.841 INFO [Node.<init>] - Binding additional locator mechanisms: relative
# 10:29:00.865 INFO [GridModel.setAvailability] - Switching Node 25ebf010-4fd3-41fb-bd2d-73190d3234b2 (uri: http://172.27.192.1:4444) from DOWN to UP
# 10:29:00.866 INFO [LocalDistributor.add] - Added node 25ebf010-4fd3-41fb-bd2d-73190d3234b2 at http://172.27.192.1:4444. Health check every 120s
# 10:29:01.421 INFO [Standalone.execute] - Started Selenium Standalone 4.8.2 (revision 826dbfc730): http://172.27.192.1:4444

#next I followed https://docs.ropensci.org/RSelenium/articles/basics.html

#### INITIALIZE ####
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#### LIBRARIES ####

library(RSelenium)
library(rvest)
library(tidyverse)
library(janitor)
library(wdman)


#### Get the Selenium Server Running and Goto MPA Database ####

binman::list_versions("chromedriver")
chrome_driver <- wdman::chrome(port = 4445L,
                               version = "111.0.5563.64")

binman::list_versions("seleniumserver")
selenium_server <- wdman::selenium(port = 4445L,
                                   version = "3.141.59",
                                   chromever = "111.0.5563.64")

selenium_driver <-
  rsDriver(port = 4445L,
           chromever = "111.0.5563.64")

# there should now be one chrome window that was auto-spawned

remDr <- selenium_driver[["client"]]

remDr$open()



# you should now have two chrome windows open

#### Get 1 page of the Table ####

# Navigate to the target URL
remDr$navigate("http://www.database.mpasupportnetwork.com")

# Get the page source
page_source <- remDr$getPageSource()[[1]]

# extract the table data
table_data <-
  xml2::read_html(page_source) %>%
  rvest::html_nodes("#table_handler table") %>%
  rvest::html_table() %>%
  pluck(1) %>%
  clean_names() %>%
  select(-starts_with("x")) %>%
  dplyr::slice(-1) %>%
  dplyr::mutate(across(c(core_area_ha,buffer_area_ha), as.numeric))

#### Functions to Manipulate the Table ####

# Click the next page button
next_page <-
  function(next_page_button_xpath = "//a[@id='example_next']"){
    next_page_button <- 
      remDr$findElement(using = "xpath", 
                        next_page_button_xpath)
    if (next_page_button$getElementAttribute("class") != "paginate_button next disabled") {
      next_page_button$clickElement()
      Sys.sleep(5)
    } else {
      break
    }
  }

# # Click the details-control button
# 
# button_xpath = "//tr[15]/td[contains(@class, 'details-control')]"
# remDr$findElement(using = "xpath",
#                   button_xpath)$clickElement()

# Click the details-control button for each row

click_details_buttons <-
  function(){
    # Get the table element
    table_elem <- remDr$findElement(using = "id", 
                                    "example")
    
    # Find all rows of the table
    rows <- table_elem$findElements(using = "css selector", 
                                    "tbody tr")
    
    # Click the "details-control" button in each row
    for (i in sort(seq_along(rows),
                   decreasing = TRUE)) {
      button_xpath = str_c("//tr[",
                           i,
                           "]/td[contains(@class, 'details-control')]")
      remDr$findElement(using = "xpath", 
                        button_xpath)$clickElement()
      Sys.sleep(2)
    }
    
  }




#### Function to Grab Data Table Page ####

get_table_page_data <-
  function(){
    page_source <- remDr$getPageSource()[[1]]
    
    xml2::read_html(page_source) %>%
      rvest::html_nodes("#table_handler table") %>%
      rvest::html_table() %>%
      pluck(1) %>%
      clean_names() %>%
      select(-starts_with("x")) %>%
      dplyr::slice(-1) %>%
      dplyr::mutate(across(c(core_area_ha,buffer_area_ha), 
                           as.numeric))
  }

get_expanded_table_page_data <-
  function(){
    # click the buttons to show the location, legislation, and network data
    click_details_buttons()
    Sys.sleep(5)
    
    page_source <- remDr$getPageSource()[[1]]
    
    xml2::read_html(page_source) %>%
      rvest::html_nodes("#table_handler table") %>%
      rvest::html_table() %>%
      pluck(1) %>%
      clean_names() %>% 
      rename(category = x,
             value = complete_name) %>%
      select(category,
             value) %>% 
      dplyr::slice(-1) %>% 
      dplyr::filter(! str_detect(category,
                                 "^Location:.")) %>%
      dplyr::mutate(category = case_when(category == "" ~ "complete_name",
                                         TRUE ~ category),
                    category = str_to_lower(category),
                    category = str_remove_all(category,
                                              ":")) %>% 
      dplyr::mutate(across(everything(),
                           ~na_if(., "na")),
                    across(everything(),
                           ~na_if(., "n/a")),
                    across(everything(),
                           ~na_if(., ""))) %>% 
      group_by(category) %>%
      mutate(row = row_number()) %>%
      pivot_wider(names_from = category,
                  values_from = value) %>%
      select(-row) 
      
  }

#### Get the whole table ####

# Navigate to the target URL
remDr$navigate("http://www.database.mpasupportnetwork.com")

# Allow the page to load and the JavaScript to execute
Sys.sleep(10)

# Get the total number of pages
page_num_xpath = "//div[@class='dataTables_paginate paging_simple_numbers']//span//a[last()]"
total_pages <- 
  remDr$findElement(using = "xpath", 
                    page_num_xpath)$getElementText()[[1]] %>%
  as.numeric()

# prime the pump with the first page of data
table_data <- get_table_page_data()
next_page()

# get the rest of the data
for (i in 2:total_pages) {
  table_data <-
    bind_rows(table_data,
              get_table_page_data())
  next_page()
}

#### WRITE TABLE DATA ####
# # be careful, you  might overwrite data if the db changed
# write_tsv(table_data,
#           "../data/philippine_mpa_database-marine_protected_areas_list.tsv")


#### Get the whole table with expanded rows ####

# Navigate to the target URL
remDr$navigate("http://www.database.mpasupportnetwork.com")

# Allow the page to load and the JavaScript to execute
Sys.sleep(10)

# Get the total number of pages
page_num_xpath = "//div[@class='dataTables_paginate paging_simple_numbers']//span//a[last()]"
total_pages <- 
  remDr$findElement(using = "xpath", 
                    page_num_xpath)$getElementText()[[1]] %>%
  as.numeric()



# prime the pump with the first page of data
table_data <- 
  get_table_page_data() %>%
  left_join(get_expanded_table_page_data()) %>%
  select(complete_name,
         location:network,
         everything())
  
next_page()

# get the rest of the data
for (i in 2:total_pages) {
  table_data <-
    bind_rows(table_data,
              get_table_page_data() %>%
                left_join(get_expanded_table_page_data()) %>%
                select(complete_name,
                       location:network,
                       everything()))
  
  next_page()
}
#### Shut Down Selenium Server ####


# Close the browser when finished
remDr$close()

# Stop the Selenium server
selenium_driver[["server"]]$stop()

