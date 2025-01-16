## Function to obtain, from a vector of species names, a 3-parts list with:
#  1) accepted names according to a reference database
#  2) detect which species have failed to be translated from the input name list
#  to the reference DB (sometimes there are genera that the function is not able
#  to handle well)
#  3) get a vector with the index positions of duplicated species (basically
#  to check easily if there is any loss of diversity by the translation)

## The reference databases are downloadable as rdata objects in:
#  https://github.com/nameMatch/Database/tree/main

get_clean_names <- 
  function(
    spp_list, # A character vector with the species names to check
    spp_db,   # Any of the 3 possible databases (see code)
    suff_dir = NULL # Optional argument to reach the data if it's in a subfolder
  ){
    
    ## Get current project directory:
    pre_dir <- here::here()
    
    ## Visualize it interactively:
    print(paste("Current directory is:", pre_dir))
    
    ## Make room to search for the data in subfolders of the directory:
    if(!is.null(suff_dir)) dir <- paste0(pre_dir, suff_dir) else dir <- pre_dir
    
    if(!(spp_db %in% c("WCVP", "LCVP", "WFO"))){
      stop("The reference database has to be either:\nthe World Checklist of Vascular Plants ('WCVP'), \nthe Leipzing Catalogue of Vascular plants ('LCVP')\nor World Flora Online ('WFO')")
    }
    
    ## Load the required database
    
    if(spp_db == "WCVP") load(paste0(dir, "/Plants_WCVP.rdata"))
    else if(spp_db == "LCVP") load(paste0(dir, "/Plants_LCVP.rdata"))
    else if(spp_db == "WFO")  load(paste0(dir, "/Plants_WFO.rdata"))
    
    spp_db <- database; rm("database")
    
    ## Clean names from inconsistent case use, underscores, and other possible typos
    spp_list  <- U.Taxonstand::nameClean(spp_list, author = F)$NameClean
    
    
    ## Do the actual name standardization
    all_      <- U.Taxonstand::nameMatch(spp_list, spp_db)
    all_1     <- all_[match(spp_list, all_$Submitted_Name), ]
    all_spp   <- all_1$Accepted_SPNAME
    fams      <- all_1$Family
    ind_fail  <- which(is.na(all_spp))
    spp_fail  <- all_1$Submitted_Name[ind_fail]
    dups      <- which(duplicated(all_spp))
    
    return(list("spp" = all_spp, "failures" = spp_fail, "duplicated" = dups, 
                "family" = fams))
    
    
    ## The use of return command is safer when we have an impure function such
    #  as this one (i.e. a function that returns console interactive results, such
    #  as printings, plots, etc.). It makes sure that when assigning a name to the
    #  output of the function, the output is actually what is inside the return
    #  command, and not anything else produced on the way.
  }


