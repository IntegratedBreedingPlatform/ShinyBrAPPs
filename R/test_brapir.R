# con <- brapirv2::brapi_connect(
#   secure = TRUE,
#   protocol = "https://",
#   db = "bms-uat-test.net",
#   port = 443,
#   apipath = "bmsapi",
#   multicrop = TRUE,
#   commoncropname = "maize",
#   token = "aboizet:1678807709888:2a1739ea370c453badc4348e866d6f85",
#   granttype = "token",
#   clientid = "brapir",
#   bms = TRUE
# )
# 
# #variable PH_M_cm
# variableDbId <- "20456"
# 
# #method id
# methodDbId <- "100876"
# 
# methodIds <- list(
#   BLUES = "100874",
#   BLUPS = "100875",
#   seBLUES = "100876",
#   seBLUPS = "100877"
# )
# 
# brapi_post_several_objects <- function(callurl, token, body) {
#   resp <- httr::POST(url = callurl,
#                      body = json_body,
#                      encode = "json",
#                      httr::timeout(25),
#                      httr::add_headers(
#                        "Authorization" = paste("Bearer", usedArgs[["con"]][["token"]]),
#                        "Content-Type"= "application/json",
#                        "accept"= "*/*"
#                      )
#   )
#               
#   cont <- httr::content(x = resp, as = "text", encoding = "UTF-8")
#   resp$content <- jsonlite::fromJSON(cont)
#   
#   return(resp)
# }
# 
# brapi_post_several_variables <- function(con, body) {
#   # usedArgs <- brapirv2:::brapi_usedArgs(origValues = FALSE)
#   # ## Obtain the call url
#   # callurl <- brapirv2:::brapi_POST_callURL(usedArgs = usedArgs,
#   #                                          callPath = "/variables",
#   #                                          reqArgs = "",
#   #                                          packageName = "BrAPI-Phenotyping",
#   #                                          callVersion = 2.0)
#   server_url <- paste0(con$protocol, con$db, ":", con$port, "/", con$apipath, "/", con$commoncropname, "/brapi/v2")
#   callurl <- paste0(server_url, "/variables")
#   resp <- brapi_post_several_objects(callurl, con$token, body)
#   return(resp)
# }
# 
# brapi_post_bluesblups_variables <- function(variableDbId) {
#   
#   # get variable name, scaleId, traitId
#   search_variables <- brapirv2::brapi_post_search_variables(
#     con = con,
#     #dataTypes = "",
#     #externalReferenceIDs = "",
#     #externalReferenceSources = "",
#     #methodDbIds = "",
#     observationVariableDbIds = variableDbId,
#     #observationVariableNames = "",
#     #ontologyDbIds = "",
#     #page = 0,
#     #pageSize = 1000,
#     #scaleDbIds = "",
#     #studyDbId = "",
#     #traitClasses = "",
#     #traitDbIds = ""
#   )
#   
#   variables <- brapirv2::brapi_get_search_variables_searchResultsDbId(
#     con = con,
#     searchResultsDbId = search_variables$searchResultsDbId,
#     #page = 0,
#     #pageSize = 1000
#   )
#   
#   scaleDbId <- variables$scale.scaleDbId[1]  #"6085"
#   variableName <- variables$observationVariableName[1]
#   traitDbId <- variables$trait.traitDbId[1]  #"20454"
#   
#   # check if the BLUES/BLUPS variables already exist for this variable
#   search_variables <- brapirv2::brapi_post_search_variables(
#     con = con,
#     #dataTypes = "",
#     #externalReferenceIDs = "",
#     #externalReferenceSources = "",
#     methodDbIds = c(BLUES_methodDbId, BLUPS_methodDbId, seBLUES_methodDbId, seBLUPS_methodDbId),
#     #observationVariableDbIds = "",
#     #observationVariableNames = "",
#     #ontologyDbIds = "",
#     #page = 0,
#     #pageSize = 1000,
#     scaleDbIds = scaleDbId,
#     #studyDbId = "",
#     #traitClasses = "",
#     traitDbIds = traitDbId
#   )
#   variables <- brapirv2::brapi_get_search_variables_searchResultsDbId(
#     con = con,
#     searchResultsDbId = search_variables$searchResultsDbId,
#     #page = 0,
#     #pageSize = 1000
#   )
#   
#   missing_variables_df <- data.frame(matrix(nrow = 0, ncol = 5))
# 
#   for (i in 1:length(methodIds)) {
#     if (!methodIds[[i]] %in% variables$method.methodDbId) {
#       new_variable_name <- paste0(variableName, "_", names(methodIds[i]))
#       missing_variables_df <- rbind(missing_variables_df, c(new_variable_name, "MEANS", methodIds[[i]], scaleDbId, traitDbId))
#     }
#   }  
#   
#   colnames(missing_variables_df) =  c("observationVariableName", "contextOfUse", "methodDbId", "scaleDbId", "traitDbId")
#   
#   
#   # create missing variables
# 
#   body <- list()
#   for (i in 1:nrow(missing_variables_df)) {
#     var <- list(
#       contextOfUse = as.array("MEANS"),
#       method = list(methodDbId = jsonlite::unbox(missing_variables_df[i, "methodDbId"])),
#       observationVariableName = jsonlite::unbox(missing_variables_df[i, "observationVariableName"]),
#       scale = list(scaleDbId = jsonlite::unbox(missing_variables_df[i, "scaleDbId"])),
#       trait = list(traitDbId = jsonlite::unbox(missing_variables_df[i, "traitDbId"]))
#     )
#     
#     body <- c(body, list(var))
#   }
#   
#   resp <- brapi_post_several_variables(con, body)
# 
# }
# 
# 
# # germplasm_df <- select(rv$data, c("germplasmDbId","germplasmName"))
# # germplasm_df <- germplasm_df[!duplicated(germplasm_df), ]
# # colnames(df2) <- c("germplasmDbId","genotype")
# # total <- merge(df2, rv_mod$metrics_B, by="genotype")
# # 
# # colnames <- colnames(rv_mod$metrics_B)
# # variableNames <- setdiff(colnames, c('genotype','entryType'))
# # variables_df <- select(rv$data, c("observations.observationVariableDbId","observations.observationVariableName"))
# # variables_df <- variables_df[!duplicated(variables_df), ]
# 
# # variables <- list()
# # for (i in 1:nrow(variables_df)) {
# #   var <- list(variables_df[i,]["observations.observationVariableName"] = variables_df[i,][observations.observationVariableDbId])
# #   variables <- c(body, var)
# # }
# 
# 
# 
