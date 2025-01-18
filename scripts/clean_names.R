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
#  always returns the same values (the brackets are a way to make sure
#  we run the two commands together in case we are interactively executing
#  the script)
{
  set.seed(1452)
  x <- get_clean_names(spp_names_filtered, "WCVP", "/data/")
}

## Check which species did fail to be translated:
# x$failures
# Just one: Euonimus europeus (should be Euonymous europaeus)

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

# A browser window will pop-up automatically; close it manually. 
# Now select the client from the server
remDr <- rD$client
remDr$close()
# Open the ghost browser session
remDr$open()

# Make a list of your urls in the Kew gardens website. We need to add the
# quote-unquote because otherwise the first result from our search is not
# the match to our species but to some subspecies of another species.
# For example searching for Arenaria serpyllifolia returns as first result
# Arenaria nevadensis, a species from Nevada, US, which contains a subspecies 
# whose synonym matches with Arenaria serpyllifolia. Of course this is not
# what we want. The quote-unquote around the binomen solves this
target_urls <- paste0('https://powo.science.kew.org/results?q=', 
                      '"', na.omit(translations$spp_clean), '"'
)
target_urls <- unique(target_urls)
names(target_urls) <- unique(translations$spp_clean)

# Set a list with names we will fill in
synonyms_list        <- vector("list", length = length(target_urls))
names(synonyms_list) <- unique(translations$spp_clean)

## Each species can contain related names either in the Synonyms or the Accepted
#  Intraspecifics sections of the POWO website. Each section has its own xpath:
xpath_accepted <- "/html/body/div[2]/div/div[3]/main/div/div[1]/div/div/ul/li[2]/div"
xpath_button <- "/html/body/div[2]/div/div[3]/main/section/div[1]/div/article/a/div/p"
xpath_syns  <- "/html/body/div[2]/main/section[2]/div/div[2]/div/ul"
xpath_intra <- "/html/body/div[2]/main/section[3]/div/div[2]/div"

# There's a free parameter, the threshold time to concatenate the different
# actions in the ghost browser, as the browser needs to execute each command
# before the subsequent one makes sense, but the codes evaluates the commands
# faster. Let's set that threshold time at 2.5 seconds (it depends on  the machine
# and the connection speed):
threshold_time <- 1.5

## Now let's try navigating to one of the species urls. Acer campestre, that
#  has both synonyms and accepted intraspecifics, will work:

## First let's access the general information url of Acer campestre:

get_spp_url <- 
  function(
    search_url, 
    xpath0 = xpath_accepted,
    xpath1 = xpath_button
    ){
  tryCatch(
    {
      remDr$navigate(search_url)
      Sys.sleep(threshold_time)
      
      # Select only accepted species names (without it face similar problems 
      # as those coming from not using the quote-unquote in the URL)
      temp <- remDr$findElement(
        using = "xpath",
        value = xpath0
      )
      temp$clickElement()
      
      Sys.sleep(1)
      
      # Click on the species name button
      temp <- remDr$findElement(
        using = "xpath",
        value = xpath1
      )
      temp$clickElement()
      
    },
    error = function(e) FALSE  # For "unplaced" species (i.e. Knautia rimosa) 
    )
  }

## IMPORTANT!
#  1) Close the cookies and pop-up information window manually
#  2) Then rerun the first call to get_spp_url
## PENDING: find the xpath of these two buttons to press them automatically

## Run the following and close manually the cookies box and the pop-up window:
get_spp_url(target_urls[[1]])

## Some species are "unplaced", that means that their names are not truly
#  located anywhere. This forces us to examine their synonyms and subspecies
#  manually, not through our webscraping functions. We can detect them with
#  the following code:
x <- lapply(target_urls, get_spp_url)
unplaced_spp <- names(x)[sapply(x, isFALSE)]
# unplaced_spp
## Clinopodium arvense, Knautia rimosa and Trisetum elatum are unplaced species.
#  Let's get rid of them:
target_urls <- target_urls[!sapply(x, isFALSE)]

## Now let's find which are the synonyms. Making a function will be useful
#  as we have to know both the synonyms at the species and at the subspecies
#  level, so we want a consistent tool
get_synonyms <- function(url, xpath = xpath_syns){
  tryCatch(
    {
      if(url %in% target_urls) get_spp_url(url)
      else remDr$navigate(url)
      # Let the system update
      Sys.sleep(threshold_time)
      
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
      res       <- list()
      res$n     <- length(tmp)
      res$names <- tmp
      res$clean <- tmp_clean
      
      return(res)
    },
    error = function(e) "Error. Probably the species is unplaced. Check by using get_spp_url()"
  )
}
# It works, run the following command to try:
# get_synonyms(tst_urls[[1]])

# The anonymous function within the sapply will come about many times. Let's give it
# a name:
get_binomen <- function(x){
  places <- str_locate_all(x, "[A-Z]|(\\s\\()")[[1]]
  if(NROW(places) > 1){
    cut_at <- places[2,1]-1
    str_sub(x, 1, cut_at) %>% str_trim()
  }
  else x
}

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
      Sys.sleep(threshold_time)
      if(url %in% target_urls) get_spp_url(search_url = url)
      else stop("The species' URL can only be from the accepted names, not one of the synonyms")
      
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
      
      tmp_clean <- sapply(tmp_syns, get_binomen)
      
      names(tmp_clean) <- NULL
      
      res       <- list()
      res$n     <- length(tmp)
      res$names <- tmp
      res$clean <- tmp_clean
      
      if(any(res$clean == "")) return(get_synonyms(url))
      
      return(res)
        },
      error = function(e) NULL
      )
  }

# It works, run the following command to try:
get_ssp(target_urls[["Acer campestre"]]) # (Acer campestre, returns 3 ssps)
# get_ssp(target_urls[[2]]) # (Allium scorodoprasum, 0 ssps, returns NULL)

## Now we now how to retrieve the synonyms and the subspecies of the species
#  present in our list of accepted names. However, we have to add a layer of
#  complexity by, then, searching the synonyms of the subspecies.
## This means that, for each species in target_urls, we must obtain:
#  1) List of synonyms:   Done -> get_synonyms()
#  2) List of subspecies: Done -> get_ssp()
#  3) For each subspecies, list of synonyms: pending

## It would be easier to have a function that would return both, synonyms
#  and, in case of there being any, subspecies, with a single call, to 
#  speed up the process. Let's do it
tst_get_all_names <- 
  function(
    url,
    xpath1 = xpath_syns,
    xpath2 = xpath_intra
  ){
    tryCatch(
      {
        if(url %in% target_urls) get_spp_url(search_url = url)
        else stop("The species' URL can only be from the accepted names, not one of the synonyms")
        
        tmp <- remDr$getCurrentUrl()[[1]]
        
        tmp_syns <-
          tmp %>% 
          read_html() %>% 
          html_elements(xpath = xpath1) %>% 
          html_text2() %>% 
          str_split("\n") %>% 
          unlist() %>% 
          .[!grepl("\\r", .)]
        
        tmp_syns_clean <- sapply(tmp_syns, get_binomen)

        res_syns       <- list()
        res_syns$n     <- length(tmp_syns)
        res_syns$names <- tmp_syns
        res_syns$clean <- unname(tmp_syns_clean)
        
        tmp_subsp <-
          tmp %>% 
          read_html() %>% 
          html_elements(xpath = xpath2) %>% 
          html_text2() %>% 
          str_split("\n") %>% 
          unlist() %>% 
          .[!grepl("\\r", .)]
        
        tmp_subsp_clean <- sapply(tmp_syns, get_binomen)

        if(any(tmp_subsp_clean == "")) return(res_syns)
        
        res_subsp       <- list()
        res_subsp$n     <- length(tmp_subsp_clean)
        res_subsp$names <- tmp_subsp
        res_subsp$clean <- unname(tmp_subsp_clean)
       
        res       <- list()
        res$n     <- sum(res_syns$n, res_subsp$n)
        res$names <- c(res_syns$names, res_subsp$names)
        res$clean <- c(res_syns$clean, res_subsp$clean)
        
        return(res)
      },
      error = function(e) "This is an error"
    )
  }

# tst_get_all_names(taret_url[[1]])

## Let's go back to the start, and find how to obtain the URLs of the 
#  different ssps. At least we know what species do contain subspecies,
#  so we can make our search easier:
# y <- vector("list", length = length(target_urls))
# for(i in seq_along(target_urls)){
#  y[[i]] <- get_ssp(target_urls[[i]]) 
# }
# names(y) <- names(target_urls)
# possible_false_negatives <- names(y)[sapply(y, is.null)]
# y[possible_false_negatives] <- 
#   lapply(
#     possible_false_negatives, 
#     function(x) y[[x]] <- get_ssp(target_urls[[x]])
#     )

## Now we can with some confidence say which species do contain subspecies: 
# spp_with_ssps <- names(y[-c(which(sapply(y, is.null)))])

## And it's a matter of finding the URLs of the subspecies of each species:
get_subsp_url <- function(url){
  tryCatch(
    {
      tmp <- remDr$getCurrentUrl()[[1]]
      # The following lines can increase safety if we would use this function
      # outside the others that provide the adequate context:
      # if(tmp != url) remDr$navigate(url)
      tmp <- remDr$findElement(using = "link text", value = "Accepted Infraspecifics")
      tmp$clickElement()
      
      # In x we collect all the urls of the different subspecies
      x <- 
        tmp$getCurrentUrl()[[1]] %>% 
        read_html() %>% 
        html_nodes(xpath = "//*[@class='c-synonym-list two-col']//*[@class='teal-link']") %>% 
        html_attr("href")
      
      # And now add the main directory
      x <- paste0("https://powo.science.kew.org", x)
      
      return(x)
    },
    error = function(e) FALSE
  )
}

## Now we make a function to get the synonyms of the list of subspecies URLs
#  coming from a certain species:
get_ssp_synonyms <- function(url_db){
  my_ls  <-lapply(url_db, get_synonyms)
  x <- list()
  x$n <- length({
    my_ls %>% 
      lapply("[[", "names") %>% 
      Reduce(c, .)
  })
  x$names <-
    my_ls %>% 
    lapply("[[", "names") %>% 
    Reduce(c, .) %>% 
    unlist() %>% 
    unique() %>% 
    .[grepl("\\s", .)]
  x$clean <-
    my_ls %>% 
    lapply("[[", "clean") %>% 
    Reduce(c, .) %>% 
    unlist() %>% 
    unique() %>% 
    .[grepl("\\s", .)]
  return(x)
}

## And now we tie everything together in a new function
get_all_names <- 
  function(
    url,
    xpath1 = xpath_syns,
    xpath2 = xpath_intra
  ){
    tryCatch(
      {
        if(url %in% target_urls) get_spp_url(search_url = url)
        else stop("The species' URL can only be from the accepted names, not one of the synonyms")
        
        tmp <- remDr$getCurrentUrl()[[1]]
        
        tmp_syns <-
          tmp %>% 
          read_html() %>% 
          html_elements(xpath = xpath1) %>% 
          html_text2() %>% 
          str_split("\n") %>% 
          unlist() %>% 
          .[!grepl("\\r", .)]
        
        tmp_syns_clean <- sapply(tmp_syns, get_binomen)
        
        res_syns       <- list()
        res_syns$n     <- length(tmp_syns)
        res_syns$names <- tmp_syns
        res_syns$clean <- unname(tmp_syns_clean)
        
        # With this conditional we can handle species that don't contain
        # subspecies:
        if(isFALSE(get_subsp_url(tmp))) return(res_syns)
        
        tmp_subsp <-
          tmp %>% 
          read_html() %>% 
          html_elements(xpath = xpath2) %>% 
          html_text2() %>% 
          str_split("\n") %>% 
          unlist() %>% 
          .[!grepl("\\r", .)]
        
        tmp_subsp_clean <- sapply(tmp_syns, get_binomen)

        res_subsp       <- list()
        res_subsp$n     <- length(tmp_subsp_clean)
        res_subsp$names <- tmp_subsp
        res_subsp$clean <- unname(tmp_subsp_clean)
        
        subs <- get_subsp_url(tmp)
        res_subsyns <- get_ssp_synonyms(subs)
        
        # Combine everyhing in a single list:
        res       <- list()
        res$n     <- sum(res_syns$n, res_subsyns$n, res_subsp$n)
        res$names <- c(res_syns$names, res_subsyns$names, res_subsp$names)
        res$clean <- c(res_syns$clean, res_subsyns$clean, res_subsp$clean)
        
        return(res)
      },
      # And the only errors that might happen now are due to either
      # d
      error = function(e) "This is an error. Probably unplaced species, or need to increase threshold_time"
    )
  }

# get_all_names(target_urls[[1]])
get_all_names(target_urls[[2]])

tmp <- lapply(target_urls, get_all_names)


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
