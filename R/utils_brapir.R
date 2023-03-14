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



