geocode_xyz_query <- function(query, filter, attempts = 10, threshold = .4,
                              quiet = FALSE){
  require(httr)
  require(jsonlite)
  require(sf)
  
  for(i in 1:length(query)){
    if(quiet != TRUE){cat("\r", (i - 1) / length(query))}
    z <- 0
    final_skip <- FALSE
    while(z < attempts){
      Sys.sleep(1)
      response <- GET(paste0("https://geocode.xyz/", paste0(str_split(query[i], pattern = " ")[[1]], collapse = "\\+"),
                        "?json=1"), accept_json())
      if("standard" %in% names(content(response))){
        break
      }else if("error" %in% names(content(response))){
        if(content(response)$error$description == "Your request produced no suggestions."){
          final_skip = TRUE
          break
        }else{
          z <- z + 1
        }
      }else{
        z <- z + 1
      }
    }
    if(!final_skip){
      result <- data.frame(t(data.frame(unlist(fromJSON(content(response, "text"), flatten = TRUE)$standard))))
      result$latitude <- as.numeric(fromJSON(content(response, "text"), flatten = TRUE)$latt)
      result$longitude <- as.numeric(fromJSON(content(response, "text"), flatten = TRUE)$longt)
      rownames(result) <- 1:nrow(result)
      result <- result[result$confidence >= threshold]
    }
    if(final_skip | nrow(result) == 0){
      object <- data.frame(query = query[i],
                           missing = TRUE)
    }else{
      object <- result %>% 
        mutate(query = query[i])
    }
    if(i == 1){final <- object}else{final <- bind_rows(final, object)}
  }
  return(final)
}
