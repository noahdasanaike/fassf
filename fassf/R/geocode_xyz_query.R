#' @export

geocode_xyz_query <- function(query, filter, attempts = 10, threshold = .4,
                              quiet = FALSE){
  require(httr)
  require(jsonlite)
  require(sf)
  require()
  
  for(i in 1:length(query)){
    if(quiet != TRUE){cat("\r", (i - 1) / length(query))}
    z <- 0
    final_skip <- FALSE
    while(z < attempts){
      throttled <- TRUE
      while(throttled){
        Sys.sleep(2)
        response <- GET(paste0("https://geocode.xyz/", paste0(strsplit(query[1], pattern = " ")[[1]], collapse = "\\+"),
                               "?json=1"), accept_json())
        if("longt" %in% names(content(response))){
          if(content(response)$longt == "Throttled! See geocode.xyz/pricing"){
            throttled <- TRUE
          }
          throttled <- FALSE
        }else{
          throttled <- FALSE
        }
      }
      
      if("standard" %in% names(content(response))){
        break
      }else if("error" %in% names(content(response))){
        if(content(response)$error$description == "Your request produced no suggestions." |
           content(response)$error$description == "Supply a valid query."){
          final_skip = TRUE
          break
        }else{
          z <- z + 1
        }
      }else{
        z <- z + 1
      }
    }
    if(grepl(content(response, "text"), pattern = "DOCTYPE")){
      object <- data.frame(query = query[i],
                           missing = TRUE)
    }else if(!final_skip & "latt" %in% names(fromJSON(content(response, "text"), flatten = TRUE))){
      result <- data.frame(t(data.frame(unlist(fromJSON(content(response, "text"), flatten = TRUE)$standard))))
      result$latitude <- as.numeric(fromJSON(content(response, "text"), flatten = TRUE)$latt)
      result$longitude <- as.numeric(fromJSON(content(response, "text"), flatten = TRUE)$longt)
      rownames(result) <- 1:nrow(result)
      result <- result[result$confidence >= threshold,]
      if(nrow(result) > 0){
        object <- result %>% 
          mutate(query = query[i])
      }else{
        object <- data.frame(query = query[i],
                             missing = TRUE)
      }
    }else{
      object <- data.frame(query = query[i],
                           missing = TRUE)
    }
    if(i == 1){final <- object}else{final <- bind_rows(final, object)}
  }
  return(final)
}
