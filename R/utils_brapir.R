# Function to POST several objects
# The body in json is an array 
# returns human readable response content
post_several_objects <- function(callurl, token, body) {
  resp <- httr::POST(url = callurl,
                     body = body,
                     encode = "json",
                     httr::timeout(25),
                     httr::add_headers(
                       "Authorization" = paste("Bearer", token),
                       "Content-Type"= "application/json",
                       "accept"= "*/*"
                     )
  )
  
  cont <- httr::content(x = resp, as = "text", encoding = "UTF-8")
  resp$content <- jsonlite::fromJSON(cont)
  
  return(resp)
}

brapi_post_search_obsUnits <- function(con, body) {
  server_url <- paste0(con$protocol, con$db, ":", con$port, "/", con$apipath, "/", con$commoncropname, "/brapi/v2")
  callurl <- paste0(server_url, "/search/observationunits")
  resp <- post_several_objects(callurl, con$token, body)
  return(resp)
  }

# Function to create several new variables with BrAPI
brapi_post_several_variables <- function(con, body) {
  # usedArgs <- brapirv2:::brapi_usedArgs(origValues = FALSE)
  # ## Obtain the call url
  # callurl <- brapirv2:::brapi_POST_callURL(usedArgs = usedArgs,
  #                                          callPath = "/variables",
  #                                          reqArgs = "",
  #                                          packageName = "BrAPI-Phenotyping",
  #                                          callVersion = 2.0)
  server_url <- paste0(con$protocol, con$db, ":", con$port, "/", con$apipath, "/", con$commoncropname, "/brapi/v2")
  callurl <- paste0(server_url, "/variables")
  resp <- post_several_objects(callurl, con$token, body)
  return(resp)
}

# Function to create several new observationUnits with BrAPI
brapi_post_several_observationUnits <- function(con, body) {
  server_url <- paste0(con$protocol, con$db, ":", con$port, "/", con$apipath, "/", con$commoncropname, "/brapi/v2")
  callurl <- paste0(server_url, "/observationunits")
  resp <- post_several_objects(callurl, con$token, body)
  return(resp)
}

# Function to create several new observations with BrAPI
brapi_post_several_observations <- function(con, body) {
  server_url <- paste0(con$protocol, con$db, ":", con$port, "/", con$apipath, "/", con$commoncropname, "/brapi/v2")
  callurl <- paste0(server_url, "/observations")
  resp <- post_several_objects(callurl, con$token, body)
  return(resp)
}

# Function to update a variable with BrAPI
brapi_put_variable <- function(con, body, variableDbId) {
  server_url <- paste0(con$protocol, con$db, ":", con$port, "/", con$apipath, "/", con$commoncropname, "/brapi/v2")
  callurl <- paste0(server_url, "/variables/", variableDbId)
  resp <- httr::PUT(url = callurl,
                     body = body,
                     encode = "json",
                     httr::timeout(25),
                     httr::add_headers(
                       "Authorization" = paste("Bearer", con$token),
                       "Content-Type"= "application/json",
                       "accept"= "*/*"
                     )
  )
  
  cont <- httr::content(x = resp, as = "text", encoding = "UTF-8")
  resp$content <- jsonlite::fromJSON(cont)
  return(resp)
}

# Function get variable from searchResultsDbId with BrAPI
brapi_get_variable_searchResultsDbId <- function(con, searchResultsDbId) {
  server_url <- paste0(con$protocol, con$db, ":", con$port, "/", con$apipath, "/", con$commoncropname, "/brapi/v2")
  callurl <- paste0(server_url, "/search/variables/", searchResultsDbId)
  resp <- httr::GET(url = callurl,
                    encode = "json",
                    httr::timeout(25),
                    httr::add_headers(
                      "Authorization" = paste("Bearer", con$token),
                      "Content-Type"= "application/json",
                      "accept"= "*/*"
                    )
  )
  
  cont <- httr::content(x = resp, as = "text", encoding = "UTF-8")
  resp$content <- jsonlite::fromJSON(cont)
  return(resp)
}

# Function to get variable methods
get_BLUES_methodsDbIds <- function(con, programDbId) {
  methodNames = list(
    "BLUEs" = "STABrAPP BLUES", 
    "BLUPs" = "STABrAPP BLUPS", 
    "seBLUEs" = "STABrAPP SEBLUES", 
    "seBLUPs" = "STABrAPP SEBLUPS"
  )
  
  callurl <- paste0(con$protocol, con$db, ":", con$port, "/", con$apipath, "/crops/", con$commoncropname, "/methods?programUUID=", programDbId)
  resp <- httr::GET(url = callurl,
                    httr::timeout(25),
                    httr::add_headers(
                      "Authorization" = paste("Bearer", con$token),
                      "Content-Type"= "application/json",
                      "accept"= "*/*"
                    )
  )
  
  cont <- httr::content(x = resp, as = "text", encoding = "UTF-8")
  res <- jsonlite::fromJSON(cont)

  methodIds <- list()
  if (nrow(res[res$name == methodNames$BLUEs, ]) > 0) { methodIds["BLUEs"] =  res[res$name == methodNames$BLUEs, "id"]}
  if (nrow(res[res$name == methodNames$BLUPs, ]) > 0) { methodIds["BLUPs"] =  res[res$name == methodNames$BLUPs, "id"]}
  if (nrow(res[res$name == methodNames$seBLUEs, ]) > 0) { methodIds["seBLUEs"] =  res[res$name == methodNames$seBLUEs, "id"]}
  if (nrow(res[res$name == methodNames$seBLUPs, ]) > 0) { methodIds["seBLUPs"] =  res[res$name == methodNames$seBLUPs, "id"]}
  
  if (length(methodIds) < 4) {
    #missing at least one method
    missing_methods = c()
    if (is.null(methodIds$BLUEs)) {missing_methods = append(missing_methods, methodNames$BLUEs)}
    if (is.null(methodIds$BLUPs)) {missing_methods = append(missing_methods, methodNames$BLUPs)}
    if (is.null(methodIds$seBLUEs)) {missing_methods = append(missing_methods, methodNames$seBLUEs)}
    if (is.null(methodIds$seBLUPs)) {missing_methods = append(missing_methods, methodNames$seBLUPs)}
    message = paste("Missing variable methods in BMS:",paste0(missing_methods, collapse = ", "))
    stop(message)
  }
  
  return(methodIds)
}

