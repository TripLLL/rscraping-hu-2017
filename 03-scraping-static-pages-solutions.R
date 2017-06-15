### -----------------------------
## simon munzert
## scraping static webpages
## solutions
### -----------------------------


source("packages.r")
source("functions.r")



## 1. repeat playing CSS diner and complete all levels!

---

## 2. go to the following website
browseURL("https://www.jstatsoft.org/about/editorialTeam")
# a) which CSS identifiers can be used to describe all names of the editorial team?
# b) write a corresponding CSS selector that targets them!

".member a"
"#group a"



## 3. revisit the jstatsoft.org website from above and use rvest to extract the names! Bonus: try and extract the full lines including the affiliation, and count how many of the editors are at a statistics or mathematics department or institution!

url <- "https://www.jstatsoft.org/about/editorialTeam"
url_parsed <- read_html(url)
names <- html_nodes(url_parsed, css = ".member a") %>% html_text()
names <- html_nodes(url_parsed, css = ".member") %>% html_text()
names <- html_nodes(url_parsed, "#group a") %>% html_text()

xpath <- '//div[@id="content"]//li/a'
html_nodes(url_parsed, xpath = xpath) %>% html_text()

affiliations <- html_nodes(url_parsed, ".member li") %>% html_text()
str_detect(affiliations, "tatisti|athemati") %>% table



## 4. scrape the table tall buildings (300m+) currently under construction from the following page. How many of those buildings are currently built in China? and in which city are most of the tallest buildings currently built?
browseURL("https://en.wikipedia.org/wiki/List_of_tallest_buildings_in_the_world")

url <- "https://en.wikipedia.org/wiki/List_of_tallest_buildings_in_the_world"
url_parsed <- read_html(url)
tables <- html_table(url_parsed, fill = TRUE)
buildings <- tables[[6]]

table(buildings$`Country`) %>% sort
table(buildings$City) %>% sort

buildings$height <- str_extract(buildings$height, "[[:digit:],.]+") %>% str_replace(",", "") %>% as.numeric()



## 5. Go to http://en.wikipedia.org/wiki/List_of_MPs_elected_in_the_United_Kingdom_general_election,_1992 and extract the table containing the elected MPs int the United Kingdom general election of 1992. Which party has most Sirs?

url <- "http://en.wikipedia.org/wiki/List_of_MPs_elected_in_the_United_Kingdom_general_election,_1992"

url_parsed <- read_html(url)
tables <- html_table(url_parsed, fill = TRUE)
names(tables)
mps <- tables[[4]]
head(mps)

# clean up
head(mps)
names(mps) <- c("con", "name", "party")
mps <- mps[!str_detect(mps$con, "\\[edit"),]
mps <- mps[-1,]
nrow(mps)

# look for Sirs
mps$name <- as.character(mps$name)
mps$sir <- str_detect(mps$name, "^Sir ")
table(mps$party, mps$sir)
prop.table(table(mps$party, mps$sir), 1)


## 6. Create code that scrapes and cleans all headlines from the main pages of sueddeutsche.de and spiegel.de!




## 7. use SelectorGadget to identify a CSS selector that helps extract all article author names from Buzzfeed's main page! Next, use rvest to scrape these names!

url <- "https://www.buzzfeed.com/?country=us"
url_parsed <- read_html(url)
authors <- html_nodes(url_parsed, css = ".link-gray-lighter .xs-text-6") %>% html_text() %>% str_replace_all("\\n", "") %>% str_trim()
table(authors) %>% sort



## 8. Go to http://earthquaketrack.com/ and make a request for data on earthquakes in "Florence, Italy". Try to parse the results into one character vector! Hint: After filling out a form, you might have to look for a follow-up URL and parse it in a second step to arrive at the data you need.

url <- "http://earthquaketrack.com/"
url_parsed <- read_html(url)
html_form(url_parsed)
earthquake <- html_form(url_parsed)[[1]]

earthquake_form <- set_values(earthquake, q = "Florence, Italy")
uastring <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_4) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36"
session <- html_session(url, user_agent(uastring))
earthquake_search <- submit_form(session, earthquake_form)
url_parsed <- read_html(earthquake_search)

url_link <- url_parsed %>% html_nodes(".url a") %>% html_attr("href")
url_parsed <- read_html(url_link) %>% html_nodes(".col-sm-4 li") %>% html_text()

earthquakes_florence <- str_replace_all(url_parsed,"\\n","")


## 9. The English Wikipedia features an entry with a list of political scientists around the world: https://en.wikipedia.org/wiki/List_of_political_scientists. Make use of this list to 

# (1) download all articles of the listed scientists to your hard drive, 
      # 1.1 create folder
      tempwd <- "../data/wikipedia_polsci"
      dir.create(tempwd)
      setwd(tempwd)
      
      # 1.2 inspect page
      url <- "https://en.wikipedia.org/wiki/List_of_political_scientists"
      
      # 1.3 retrieve links
      html <- read_html(url)
      anchors <- html_nodes(html, xpath = "//ul/li/a[1]")
      links <- html_attr(anchors, "href")
      links <- links[!is.na(links)]
      
      links_iffer <-
            seq_along(links) >=
            seq_along(links)[str_detect(links, "Alan_Abramowitz")] &
            seq_along(links) <= 
            seq_along(links)[str_detect(links, "John_Zaller")] &
            str_detect(links, "/wiki/")
      links_index <- seq_along(links)[links_iffer]
      links <- links[links_iffer]
      length(links)
      
      # 1.4 extract names
      names <-    links %>% 
                  basename %>% 
                  sapply(., URLdecode) %>% 
                  str_replace_all("_", " ") %>% 
                  str_replace_all(" \\(.*\\)", "") %>% 
                  str_trim
      head(names)
      
      # 1.5 get personal wiki pages in folder
      baseurl <- "http://en.wikipedia.org"
      HTML <- list()
      Fname <- str_c(basename(links), ".html")
      URL <- str_c(baseurl, links)
      # loop
      for ( i in seq_along(links) ){
            # url
            url <- URL[i]
            # fname
            fname <- Fname[i]
            # download
            if ( !file.exists(fname) ) download.file(url, fname)
            # read in files
            HTML[[i]] <- read_html(fname)
      }
      
# (2) gather the network structure behind these articles and visualize it

      #identify links between political scientists
      # loop preparation
      connections <- data.frame(from=NULL, to=NULL)
      # loop
      for (i in seq_along(HTML)) {
            pslinks <- html_attr(
                  html_nodes(HTML[[i]], xpath="//p//a"), 
                  # note: only look for links in p sections; otherwise too many links collected
                  "href")
            links_in_pslinks <- seq_along(links)[links %in% pslinks]
            links_in_pslinks <- links_in_pslinks[links_in_pslinks!=i]
            connections <- rbind(
                  connections,
                  data.frame(
                        from=rep(i-1, length(links_in_pslinks)), # -1 for zero-indexing
                        to=links_in_pslinks-1 # here too
                  )
            )
      }
     
      # results
      names(connections) <- c("from", "to")
      head(connections)
      
      # make symmetrical
      connections <- rbind(
            connections,
            data.frame(from=connections$to,
                       to=connections$from)
      )
      connections <- connections[!duplicated(connections),]
      
# (3) identify the top 10 of political scientists that have the most links from other political scientists pointing to their page!
      
      ## step 6: visualize connections
      connections$value <- 1
      nodesDF <- data.frame(name = names, group = 1)
      
      network_out <- forceNetwork(Links = connections, Nodes = nodesDF, Source = "from", Target = "to", Value = "value", NodeID = "name", Group = "group", zoom = TRUE, opacityNoHover = 3)
      
      saveNetwork(network_out, file = 'connections.html')
      browseURL("connections.html")
      
      
      ## step 7: identify top nodes in data frame
      nodesDF$id <- as.numeric(rownames(nodesDF)) - 1
      connections_df <- merge(connections, nodesDF, by.x = "to", by.y = "id", all = TRUE)
      to_count_df <- count(connections_df, name)
      # answer
      arrange(to_count_df, desc(n))




