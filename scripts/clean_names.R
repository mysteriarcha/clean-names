library(tidyverse)
## Load the function to clean the names
source("./scripts/functions/get_clean_names.R")

## Read the species list and make it a vector, not a data frame
spp_names <- readRDS("./data/spp_names.RDS")

## Make functions to detect the anomalous names due to aggregates, unidentified
#  species, numbers appearing in the species name, etc. Some datasets, according
#  to many phytosociological releve forms, will also contain words as "Rocks", 
#  "Soil", etc, so we must also keep track of them:

regex <- "sp\\.|agg\\.|cf\\.|[0-9]|(\\?|\\!)|[rR]ock?|[sS]oil|[lL]ichen?|[lL]itter|[bB]ryophyte?"

filter_bad_names   <- function(spp) grepl(regex, spp)
filter_good_names  <- Negate(filter_bad_names)

## Get the filtered species list: those that we want to run our function on and 
#  those that we don't want and will have to handle manually
spp_names_filtered <- spp_names[filter_good_names(spp_names)]
spp_names_out      <- spp_names[filter_bad_names(spp_names)]

## Now let's first deal with the species we have "in good shape" to standardize
# their names according to the World Catalogue of Vascular Plants

## Run the seed and the function at once, to make sure the fuzzy matching
#  always returns the same values
{
  set.seed(1452)
  x <- get_clean_names(spp_names_filtered, "WCVP", "/data/")
}

## Check which species did fail to be translated:
x$failures
# Just one: Euonimus europeus

## Create a dataframe with the species names before correction, after correction,
#  and with the families:
spp_db <- 
  data.frame(
    spp_raw = spp_names_filtered,
    spp_clean = x$spp,
    family = x$family
  ) %>% 
  arrange(spp_clean)

## Correct the "Euonimus europeus" error:
spp_db[spp_db$spp_raw %in% x$failures, ] <- 
  c(x$failures, "Euonymus europaeus", "Celastraceae")

spp_db <- arrange(spp_db, spp_clean)

## Get what species suffered a translation change
translations <- spp_db[!(spp_db$spp_raw %in% spp_db$spp_clean), ]
NROW(translations) # 66 of them had to be translated to fit WCVP standards

## Let's do some webscraping!
library(RSelenium)
library(netstat)
library(rvest)

# Start the driver of R Selenium
rD <- rsDriver(browser="firefox", 
               port=free_port(), 
               verbose=F,
               version = "latest",
               chromever = NULL)

# Now select the client from the server
remDr <- rD$client
# Open the ghost browser session
remDr$open()


# Make a list of your urls in the Kew gardens website
target_urls <- paste0("https://powo.science.kew.org/results?q=", 
                      na.omit(translations$spp_clean)
)
names(target_urls) <- translations$spp_clean

# Set a list with names we will fill in
synonyms_list        <- vector("list", length = length(target_urls))
names(synonyms_list) <- na.omit(translations$spp_clean)
ssp_list <- synonyms_list

## Small data set test:
## Take only 3 species from the list and try to extract the relevant information

tst_urls <- target_urls[c(1, 34, 66)]
tst_synonyms_list <-  synonyms_list[c(1, 34, 66)]

## Each species can contain related names either in the Synonyms or the Accepted
#  Intraspecifics sections of the POWO website. Each section has its own xpath:
xpath_button <- "/html/body/div[2]/div/div[3]/main/section/div[1]/div/article/a"
xpath_button <- "/html/body/div[2]/div/div[3]/main/section/div[1]/div/article/a/div/p"
xpath_syns  <- "/html/body/div[2]/main/section[2]/div/div[2]/div/ul"
xpath_intra <- "/html/body/div[2]/main/section[3]/div/div[2]/div"

## Now let's try navigating to one of the species urls. Acer campestre, that
#  has both synonyms and accepted intraspecifics, will work:

## First let's access the general information url of Acer campestre:

get_spp_url <- 
  function(
    search_url, 
    xpath = xpath_button
    ){
  remDr$navigate(search_url)
  Sys.sleep(1.5)
  temp <- remDr$findElement(
    using = "xpath",
    value = xpath
  )
  temp$clickElement()
}
get_spp_url(tst_urls[[1]])
# If we run this command, it works, leading us to the correct website:
get_spp_url(tst_urls[[1]])

## Now let's find which are the synonyms. Making a function will be useful
#  as we have to know both the synonyms at the species and at the subspecies
#  level, so we want a consistent tool:

get_synonyms <- function(url, xpath = xpath_syns){
  if(url %in% target_urls) get_spp_url(url)
  else remDr$navigate(url)
  # Let the system update
  Sys.sleep(.5)
  
  # Set a temporal object on which to call the url contents
  tmp_syns_tbl <-
    remDr$findElement("xpath", "/html/body/div[2]/main/section[2]/div/div[2]")
  
  # Obtain the synonyms
  tmp <- 
    tmp_syns_tbl$getCurrentUrl()[[1]] %>% 
    read_html() %>% 
    html_elements(xpath = xpath) %>% 
    html_text2() %>% 
    str_split("\n") %>% 
    unlist() %>% 
    .[!grepl("\\r", .)]
  
  # Get a version of the synonyms without all the author names,
  # identified by capital letters or parentheses
  tmp_clean <-
    sapply(
      tmp,
      function(x){
        places <- str_locate_all(x, "[A-Z]|(\\s\\()")[[1]]
        if(NROW(places) > 1){
         cut_at <- places[2,1]-1
         str_sub(x, 1, cut_at) %>% str_trim()
        }
        else x})
  
  names(tmp_clean) <- NULL

  # Let's store the results in a list
  res <- list()
  res$n <- length(tmp)
  res$names <- tmp
  res$clean <- tmp_clean
  
  return(res)
}
# It works, run the following command to try:
# get_synonyms(tst_urls[[1]])

get_ssp <- 
  function(
    url,
    xpath = xpath_intra){
    # The absence of subspecies in some cases leads us to use a tryCatch to
    # obtain some specific value (in our case, NULL) once we don't find the 
    # corresponding infraspecifics of a species, instead of obtaining an error
    # that stops the process
    
    tryCatch(
      {
      Sys.sleep(1)
      if(url %in% target_urls) get_spp_url(search_url = url)
      else stop("The species' URL can only be from the accepted names, not one of the synonyms")
      Sys.sleep(1)
      
      # Many species don't have any subspecies, so it is convenient to search
      # by link text rather than by xpath, as the xpath will be overtaken
      # by another section if the subspecies are missing, retrieving then
      # wrong information
      tmp_ssp_tbl <-
        remDr$findElement(using = "link text", "Accepted Infraspecifics")
      
      # In case we would like to use an available xpath, it would be this one:
      # "/html/body/div[2]/main/section[2]/div/div[2]")
      
      
      # Now, similar as in the body of get_synonyms(), obtain number and two 
      # version (raw and clean) of the subspecies names:
      
      tmp <-    
        tmp_ssp_tbl$getCurrentUrl()[[1]] %>% 
          read_html() %>% 
          html_elements(xpath = xpath) %>% 
          html_text2() %>% 
          str_split("\n") %>% 
          unlist() %>% 
          .[!grepl("\\r", .)]
      
      tmp_clean <-
        sapply(
          tmp,
            function(x){
              places <- str_locate_all(x, "[A-Z]|(\\s\\()")[[1]]
              if(NROW(places) > 1){
                cut_at <- places[2,1]-1
                str_sub(x, 1, cut_at) %>% str_trim()
              }
             else x
            })
      
      names(tmp_clean) <- NULL
      
      res <- list()
      res$n <- length(tmp)
      res$names <- tmp
      res$clean <- tmp_clean
      
      return(res)
        },
      error = function(e) NULL
      )
  }

# It works, run the following command to try:
# get_ssp(target_urls[[1]]) # (Acer campestre, returns 3 ssps)
# get_ssp(target_urls[[2]]) # (Allium scorodoprasum, 0 ssps, returns NULL)

## Now we now how to retrieve the synonyms and the subspecies of the species
#  present in our list of accepted names. However, we have to add a layer of
#  complexity by, then, searching the synonyms of the subspecies.
## This means that, for each species in target_urls, we must obtain:
#  1) List of synonyms:   Done -> get_synonyms()
#  2) List of subspecies: Done -> get_ssp()
#  3) For each subspecies, list of synonyms: pending

## Let's go back to the start, and find how to obtain the URLs of the 
#  different ssps. At least we know what species do contain subspecies,
#  so we can make our search easier:
y <- vector("list", length = length(target_urls))
names(y) <- translations$spp_clean
for(i in seq_along(target_urls)){
 y[[i]] <- get_ssp(target_urls[[i]]) 
}

spp_with_ssps <- names(y[-c(which(sapply(y, is.null)))])

get_subsp_url <- function(url){
  get_spp_url(url)
  tmp <- remDr$findElement(using = "link text", value = "Accepted Infraspecifics")
  tmp$clickElement()
  x <- 
    tmp$getCurrentUrl()[[1]] %>% 
    read_html() %>% 
    html_nodes(xpath = "//*[@class='teal-link']") %>% 
    html_attr("href")
  # If we explore a little bit, we find that the links that end with 1 are those
  # belonging to (sub)species, while those ending with 2 or higher are for genera
  # and families. 
  x <- x[grepl("-1$", x)]
  x <- paste0("https://powo.science.kew.org", x)
  return(x)
}

tst_fun <- function(url){
  url_db <- get_subsp_url(url)
  Sys.sleep(1)
  my_ls  <-lapply(url_db, get_synonyms)
  my_ls <-
    my_ls %>% 
      lapply("[[", "clean") %>% 
      Reduce(c, .) %>% 
      unlist() %>% 
      unique() %>% 
      .[grepl("\\s", .)]
  return(my_ls)
}

tst <- tst_fun(target_urls[spp_with_ssps][[4]])

subspecies_synonyms <- vector("list", length(spp_with_ssps))
for(i in seq_along(subspecies_synonyms)){
  subspecies_synonyms[[i]] <- 
    get_synonyms("https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:884615-1")
}

# Find the element to click on:
tmp <- remDr$findElement(using = "xpath", "/html/body/div[2]/div/div[3]/main/section/div[1]/div/article/a")
tmp$clickElement()
## Now we are in the website devoted to that species. Let's search for the
#  information of each section:
tmp_syns <- remDr$findElement(using = "link text", "Synonyms")

# tmp_syns_tbl <- remDr$findElement("xpath", "/html/body/div[2]/main/section[2]/div/div[2]")
# tmp_syns_tbl$clickElement()

tmp_syns_tbl$getCurrentUrl()[[1]] %>% 
  read_html() %>% 
  html_elements(xpath = xpath_syns) %>% 
  html_text2() %>% 
  str_split("\n") %>% 
  unlist() %>% 
  .[!grepl("\\r", .)]


tmp_ssp  <- remDr$findElement(using = "link text", "Accepted Infraspecifics")

# We can already store all the names of the subspecies like this:
tmp_ssp_vec <- 
  tmp_ssp$getCurrentUrl()[[1]] %>% 
  read_html() %>% 
  html_elements(xpath = "/html/body/div[2]/main/section[3]/div/div[2]") %>% 
  html_text2() %>% 
  str_split("\n") %>% 
  unlist() %>% 
  .[!grepl("\\r", .)]

## All the 

ssp_url  <- tmp$getCurrentUrl()[[1]]
ind <- 1
tmp_ssp_url <- 
  remDr$findElement(
  using = "xpath", 
  value = 
    paste0("/html/body/div[2]/main/section[3]/div/div[2]/div/ul/li[", ind, "]/a")
)
tmp_ssp_url$clickElement()


# You first have to go to the kew gardens website and reject the cookies. 
remDr$navigate(target_urls[[1]])
# After rejecting them don't to
cookies_close <- ""
pop_up_close  <- "/html/body/div[2]/div/div[1]/div/div/div[1]/div/button/svg"



# Iterate the search along the target urls:
for(i in seq_along(synonyms_list)){
  tryCatch(
    {
      remDr$navigate(target_urls[[i]])
      Sys.sleep(1) # Stop for half a second. Necessary, otherwise the system gets confused
      tmp <- remDr$findElement(using = "xpath", "/html/body/div[2]/div/div[3]/main/section/div[1]/div/article/a")
      
      # class(A.campestre)
      # tmp$getElementAttribute("href")
      
      tmp$clickElement()
      tmp_syns <- remDr$findElement(using = "link text", "Synonyms")
      tmp_ssp  <- remDr$findElement(using = "link text", "Accepted Infraspecifics")
      
      tmp_syns$clickElement()
      
      # tmp_syns_tbl <- remDr$findElement("xpath", "/html/body/div[2]/main/section[2]/div/div[2]")
      # tmp_syns_tbl$clickElement()
      
      ssp_list[[i]] <-
        tmp_ssp$getCurrentUrl()[[1]] %>% 
        read_html() %>% 
        html_elements(xpath = "/html/body/div[2]/main/section[3]/div/div[2]") %>% 
        html_text2() %>% 
        str_split("\n") %>% 
        unlist()
      
      if(!is.null(ssp_list[[i]])){
        sp_url  <- tmp$getCurrentUrl()[[1]]
        ind <- 1
        tmp_ssp <- 
          remDr$findElement(
            using = "xpath", 
            value = 
              paste0("/html/body/div[2]/main/section[3]/div/div[2]/div/ul/li[", ind, "]/a")
          )
        tmp_ssp$clickElement()
        x <- list()
        x[[ind]] <- 
          tmp_ssp$getCurrentUrl()[[1]] |>
          read_html(tmp_syns$getCurrentUrl()[[1]]) |> 
          html_elements(xpath = "/html/body/div[2]/main/section[2]/div/div[2]") |>
          html_text()
        
        while(exists("tmp_ssp")){
          tryCatch(
            {
              rm("tmp_ssp")
              ind <- ind + 1
              remDr$navigate(sp_url)
              tmp_ssp <- 
                remDr$findElement(
                  using = "xpath", 
                  value = 
                    paste0("/html/body/div[2]/main/section[3]/div/div[2]/div/ul/li[", ind, "]/a")
                )
              tmp_ssp$clickElement()
              x[[ind]] <- 
                tmp_ssp$getCurrentUrl()[[1]] |>
                read_html(tmp_syns$getCurrentUrl()[[1]]) |> 
                html_elements(xpath = "/html/body/div[2]/main/section[2]/div/div[2]") |>
                html_text()
            },
            error = function(e) NULL)
        }
        
        tmp_ssp$getElementText()
        tmp_ssp$clickElement()
        
        tmp_ssp$getCurrentUrl()[[1]] %>% 
          read_html() %>% 
          html_elements(xpath = "/html/body/div[2]/main/section[3]/div/div[2]") %>% 
          html_text2()
      }
      
      synonyms_list[[i]] <-
        read_html(tmp_syns$getCurrentUrl()[[1]]) |> 
        html_elements(xpath = "/html/body/div[2]/main/section[2]/div/div[2]") |>
        html_text()
      
      
      
      
    },
    error = function(e){
      print(paste0("No synonyms for ", translations$spp_clean[[i]], ". Better check manually!"))
      return(synonyms_list[[i]] <- "\nNo synonyms!\n")
    }
  )
}

## The display will be much faster if you first clear your console history.
#  In my system it's cleaned with ctrl+l
#  You need the cat function to display nicely the contents retrieved by 
#  html_text function
cat(synonyms_list$`Veronica chamaedrys`)
ssp_list$`Silene otites`

good_synonym <- logical(length(na.omit(translations$spp_clean)))
for(i in seq_along(synonyms_list)){
  tryCatch(
    good_synonym[i] <- grepl(translations[i,1], x = synonyms_list[[i]]),
    error = function(e) good_synonym[i] <- F
  )
}

## There are now duplicated species, delete them by:
spp_db <- spp_db[unique(match(spp_db$spp_clean, spp_db$spp_clean)), ]



## Make a column for genera with:
spp_db$genus <- stringr::str_split(spp_db$spp_clean, "\\s") %>% sapply("[[", 1)

## Now we can deal with the other 25 species manually. 
#  Some of them will be implicitly already present in our spp_db object, 
#  some of them not
spp_names_out

## THE FOLLOWING ACTIONS ARE JUST A SUGGESTION. You know better than me what 
#  to do with and how to interpret all the "cf." and "agg.". My suggestion
#  goes in the line of thinking that those strings are uninformative (i.e. the
#  data is taxonomically trustworthy or that the study question does not require 
#  the 'aggregatum' level of precision):

## Let's gather first which spp in spp_names_out have species name. These are
#  10 out of 25
spp_out <- c("Carex montana", "Euphorbia dulcis", "Festuca rubra", "Festuca valesiaca",
             "Knautia arvensis", "Medicago falcata", "Potentilla verna",
             "Ranunculus auricomus", "Verbascum nigrum", "Vicia cracca")

## Let's see which ones have been implicitly incorporated already into the
#  clean species list
redundant_spp <- sapply(spp_out, function(x) any(spp_db$spp_clean == x))

names(redundant_spp)[redundant_spp] # Don't need to worry about these, they are already in spp_db
diff_spp <- names(redundant_spp)[!redundant_spp] # These are not yet in spp_db
## They can be easily added manually to spp_db

## Now let's take a look at the genera that were left out: 
genera_out <- stringr::str_split(spp_names_out, "\\s") %>% sapply("[", 1)
length(unique(genera_out)) # 22 genera
intersect(genera_out, spp_db$genus) # 19 of them are already present in spp_db
setdiff(genera_out, spp_db$genus)   # These are the 3 not present in spp_db

## Now you will have to decide how to deal with the 15 (25 - 10) remaining species,
#  as they don't have species names. Most genera are already included in spp_bd,
#  but 3 of them are not there yet (Betula, Rosa and Taraxacum), so you cannot 
#  guess from the congeneric species in spp_db 

## Make the necessary changes manually on the spp_db object until you're done,
#  and then run, changing the file path and name to your convenience:
#  write.csv(spp_db "./data/spp_names/spp_names_clean.csv")

spp_combn  <- t(combn(spp_names_filtered, 2))
congeneric_pairs <- 
  t(
    apply(spp_combn, 
          1, 
          function(x){
            gen1 <- str_split(x[1], "\\s")[[1]] 
            gen2 <- str_split(x[2], "\\s")[[1]] 
            return(gen1 == gen2)
          }
    )
  )[, 1]
spp_combn[congeneric_pairs, ]
