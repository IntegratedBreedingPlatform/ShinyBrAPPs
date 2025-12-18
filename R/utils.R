#' @export
shortest_interval <- function(v){
  values <- v[order(v)]
  intervals <- diff(values)
  return(min(intervals[intervals!=0]))
}

#' @export
select_from_layout <- function(d, input_click = NULL, input_brush = NULL){
  int_x <- shortest_interval(d[,positionCoordinateX])
  int_y <- shortest_interval(d[,positionCoordinateY])

  if(!is.null(input_brush)){
    obs <- d[
      (positionCoordinateX+int_x/2<=input_brush$xmax) &
        (positionCoordinateX-int_x/2>=input_brush$xmin) &
        (positionCoordinateY+int_y/2<=input_brush$ymax) &
        (positionCoordinateY-int_y/2>=input_brush$ymin),
      observationDbId
    ]
    return(obs)
  }else if(!is.null(input_click)){
    obs <- d[
      (abs(input_click$x-positionCoordinateX)<=int_x/2) & (abs(input_click$y-positionCoordinateY)<=int_y/2),
      observationDbId
    ]
    return(obs)
  }

}


#' @param con brapi_connection
#' @param studyDbId 
#' @param env_number 
#' @param loc_name 
#' @param loc_name_abbrev 
#' @param stu_name_app 
#' @param stu_name_abbrev_app 
#' @param obs_unit_level can be a vector, e.g. c('PLOT', 'REP')
#'
#' @export
get_env_data <- function(con = NULL, 
                         studyDbId = NULL, 
                         env_number = NULL, 
                         loc_name = NULL, 
                         loc_name_abbrev = NULL, 
                         stu_name_app = NULL, 
                         stu_name_abbrev_app = NULL, 
                         obs_unit_level = NULL){
  
  # brapir_con <- brapir::brapi_connect(
  #   secure = con$secure, 
  #   db = con$db, 
  #   port = con$port, 
  #   apipath = con$apipath, 
  #   multicrop = con$multicrop, 
  #   commoncropname = con$commoncropname,
  #   token = con$token)

  print(paste0("retrieving data from study ", studyDbId))
  try({
    if (is.null(obs_unit_level)) {
      res <- brapir::phenotyping_observationunits_post_search(
        con = con, 
        studyDbIds = studyDbId,
        includeObservations = T
      )
      print(res$status_code)
    } else {
      obs_levels <- data.frame(levelName = obs_unit_level)
      res <- brapir::phenotyping_observationunits_post_search(
        con = con, 
        studyDbIds = studyDbId,
        observationLevels = obs_levels,
        includeObservations = T
      )
      print(res$status_code)
    }
    if (res$status_code == 200 | res$status_code == 202) {
      searchResultDbId <- as.character(res$data$searchResultsDbId)
      res <- brapir::phenotyping_observationunits_get_search_searchResultsDbId(con, searchResultsDbId = searchResultDbId)
      print(res$status_code)
      if (res$status_code == 200) {
        if (nrow(res$data) > 0) {
          observations <- tidyr::unnest(res$data, cols = "observationUnitPosition.observationLevelRelationships", names_sep = ".", keep_empty = T)
          observations <- tidyr::unnest(observations, cols = "observations", names_sep = ".", keep_empty = T)
          study_obs <- as.data.table(observations)
        } else {
          return(NULL)
        }
        
        page = 0
        while (res$metadata$pagination$totalCount > (res$metadata$pagination$currentPage + 1) * res$metadata$pagination$pageSize) {
          page <- page + 1
          res <- brapir::phenotyping_observationunits_get_search_searchResultsDbId(con, searchResultsDbId = searchResultDbId, page = page)
          observations <- tidyr::unnest(res$data, cols = "observationUnitPosition.observationLevelRelationships", names_sep = ".", keep_empty = T)
          observations <- tidyr::unnest(observations, cols = "observations", names_sep = ".", keep_empty = T)
          study_obs <- rbindlist(list(study_obs, as.data.table(observations)), use.names = T,fill = T)
        }
      } else {
        return(NULL)
      }
    }
    if (!"observations.observationDbId" %in% colnames(study_obs)) {
      study_obs <- NULL
      return(study_obs)
    } else {
      
      #to manage the case when we get MEANS and PLOTS
      if ("observationUnitPosition.observationLevelRelationships.levelCode" %in% names(study_obs)) {
        study_obs[, oLR.levelCode := `observationUnitPosition.observationLevelRelationships.levelCode`]
      } else {
        study_obs[, oLR.levelCode := NA]
      }
      if ("observationUnitPosition.observationLevelRelationships.levelName" %in% names(study_obs)) {
        study_obs[, oLR.levelName := `observationUnitPosition.observationLevelRelationships.levelName`]
      } else {
        study_obs[, oLR.levelName := NA]
      }
      
      if ("observationUnitPosition.observationLevel.levelName" %in% names(study_obs)) {
        study_obs[, observationLevel := `observationUnitPosition.observationLevel.levelName`]
      } else {
        study_obs[, observationLevel := NA]
      }
      if ("observationUnitPosition.observationLevel.levelCode" %in% names(study_obs)) {
        study_obs[, observationLevelCode := `observationUnitPosition.observationLevel.levelCode`]
      } else {
        study_obs[, observationLevelCode := NA]
      }

      study_obs <- study_obs[, .(
        observationUnitDbId,
        observationUnitName,
        germplasmDbId, 
        germplasmName, 
        studyDbId, 
        studyName, 
        programDbId, 
        programName, 
        locationDbId, 
        locationName, 
        trialDbId, 
        trialName,
        observationDbId = `observations.observationDbId`,
        observationLevel, 
        observationLevelCode, 
        entryType = `observationUnitPosition.entryType`,
        entryNumber = `additionalInfo.ENTRY_NO`,
        oLR.levelCode,
        oLR.levelName,
        positionCoordinateX = `observationUnitPosition.positionCoordinateX`,
        positionCoordinateY = `observationUnitPosition.positionCoordinateY`,
        observationTimeStamp = `observations.observationTimeStamp`, 
        observationVariableDbId = `observations.observationVariableDbId`, 
        observationVariableName = `observations.observationVariableName`, 
        observationValue = `observations.value`
      )]
      
      # remove NA or "" observations
      study_obs <- study_obs[!is.na(observationValue) & observationValue != "",]
      
      #study_obs <- study_obs[, .(plotNumber = levelCode[levelName == "PLOT"],
      #                           replicate = levelCode[levelName == "REP"],
      #                           blockNumber = levelCode[levelName == "BLOCK"]),
      #                       by = grouping_cols]
      if (any(study_obs$observationLevel=="PLOT")){
        grouping_cols <- setdiff(names(study_obs), c("oLR.levelCode", "oLR.levelName"))
        study_obs<-dcast(unique(study_obs[observationLevel=="PLOT",.(observationUnitDbId, oLR.levelCode, oLR.levelName)]),observationUnitDbId~oLR.levelName, value.var = "oLR.levelCode")[unique(study_obs[,.SD, .SDcols=grouping_cols]),on=.(observationUnitDbId)]
        #browser()
        for (f in setdiff(c("PLOT", "REP", "BLOCK"), names(study_obs))){
          study_obs[[f]] <- NA
        }
        setnames(study_obs,
                 old=c("PLOT",
                       "REP",
                       "BLOCK"),
                 new=c("plotNumber",
                       "replicate",
                       "blockNumber"))        
      }
      
      study_obs[,study_name_BMS := paste0(
        env_number, "-",
        loc_name
      )]
      study_obs[,environment_number := env_number]
      study_obs[,location_name := loc_name]
      study_obs[,location_abbrev := loc_name_abbrev]
      study_obs[,study_name_app := stu_name_app]
      study_obs[,study_name_abbrev_app := stu_name_abbrev_app]
      return(unique(study_obs))
    }
  })
  
}

#' @export
parse_api_url <- function(url){
  ## get protocol (default = "https://")
  protocolless_url <- gsub("(^http://|^https://)(.*)$", "\\2",url)
  brapi_protocol <- gsub("(^http://|^https://)(.*)$", "\\1",url)
  brapi_protocol <- ifelse(brapi_protocol == url, "https://", brapi_protocol)

  ## get base url and port (default = 443)
  db_split <- strsplit(gsub("([^/]*).*", "\\1",protocolless_url), ":")
  brapi_db <- db_split[[1]][1]
  brapi_port <- ifelse(is.na(db_split[[1]][2]),443,as.numeric(db_split[[1]][2]))

  ## brapi api path (default = "/")
  brapi_apipath <- ifelse(grepl("/.*",protocolless_url),gsub("[^/]*/(.*)", "\\1", protocolless_url),"/")

  return(
    list(
      brapi_protocol = brapi_protocol,
      brapi_db = brapi_db,
      brapi_port = brapi_port,
      brapi_apipath = brapi_apipath
    )
  )
}

#' @import data.table
#' @export
make_study_metadata <- function(con, studyDbIds=NULL, trialDbId= NULL){
  if(!is.null(trialDbId)){
    ## get environment metadata by trialDbId
    tryCatch({
      study_metadata <- as.data.table(brapir::core_studies_get(con = con, trialDbId = trialDbId)$data)
      study_metadata <- tidyr::unnest(study_metadata, cols = "environmentParameters", names_sep = ".", keep_empty = T)
      study_metadata <- as.data.table(study_metadata)
    },
    error=function(e){
      print(e)
      showNotification(paste0("Environment metadata not found for trialDbId ",trialDbId), type = "error", duration = notification_duration)
    })
  }else if(!is.null(studyDbIds)){
    ## get environment metadata by studyDbId
    ids <- unlist(strsplit(studyDbIds, ","))
    #browser()
    srid <- brapir::core_studies_post_search(con = con, studyDbIds = ids, commonCropNames = con$commoncropname)
    study_metadata <- as.data.table(tidyr::unnest( brapir::core_studies_get_search_searchResultsDbId(con, searchResultsDbId = srid$data$searchResultsDbId)$data, cols = "environmentParameters", names_sep = "."))
    #study_metadata <- rbindlist(lapply(ids,function(id){
    # tryCatch({
    #   as.data.table(brapir::core_studies_get_studyDbId(con = con, studyDbId = id)$data)
    # },
    # error=function(e){
    #   showNotification(paste0("Environment metadata not found for studyDbId ",id), type = "error", duration = notification_duration)
    # })
    #}),use.names = T, fill = T)
    if(study_metadata[,.N]==0){
      showNotification("No environment data found. Check apiURL, token, cropDb and studyDbIds", type = "error", duration = notification_duration)
    }
  }

  study_ids <- unique(study_metadata$studyDbId)
  location_ids <- unique(study_metadata$locationDbId)

  ## get location abbreviations
  #withProgress(message = "Reaching location metadata", value = 0, {
  #  req(location_ids)
  #  loc_names <- rbindlist(l = lapply(1:length(location_ids), function(k){
  #    incProgress(
  #      1/length(location_ids),
  #      detail = study_metadata[locationDbId==location_ids[k], unique(locationName)]
  #    )
  #    try({
  #      brapirv2::brapi_get_locations_locationDbId(con = con, locationDbId = location_ids[k])
  #    })
  #  }), fill = T, use.names = T)
  #})
  loc_names <- brapir::core_locations_get_search_searchResultsDbId(con, searchResultsDbId = brapir::core_locations_post_search(con, locationDbIds = location_ids)$data$searchResultsDbId)$data
  setDT(loc_names)
  req(loc_names)
  maxchar <- 9
  
  loc_names[,location_name_abbrev := lapply(abbreviation, function(x){
    if(is.na(x)){
      if(nchar(locationName)>maxchar){
        paste0(
          substr(locationName,1,4),
          "...",
          substr(locationName,nchar(locationName)-3,nchar(locationName))
        )
      }else{
        locationName
      }
    }else{
      x
    }
  })]
  study_metadata <- merge.data.table(
    x = study_metadata[,-intersect(names(study_metadata)[names(study_metadata)!="locationDbId"], names(loc_names)), with = F],
    # x = study_metadata[,-"locationName", with = F],
    y = loc_names,
    by = "locationDbId", all.x = T)

  ## get/set environment number
  if("environmentParameters.parameterName"%in%names(study_metadata)){
    env_number <- merge.data.table(
      x = study_metadata[,.(studyDbId = unique(studyDbId))],
      y = study_metadata[environmentParameters.parameterName == "ENVIRONMENT_NUMBER" | environmentParameters.parameterName == "Environment_name",.(studyDbId, environment_number = environmentParameters.value)],
      by = "studyDbId", all.x = T)
    env_number[is.na(environment_number), environment_number:=studyDbId]
  }else{
    env_number <- study_metadata[,.(studyDbId = unique(studyDbId))]
    env_number[, environment_number:=studyDbId]
  }
  study_metadata <- merge.data.table(
    x = study_metadata,
    y = unique(env_number),
    by = "studyDbId", all.x = T)

  ## set environment names
  study_metadata[,study_name_BMS := paste0(
    environment_number, "-",
    locationName
  )]
  study_metadata[,study_name_app := paste0(
    study_name_BMS, " (",
    location_name_abbrev, ")"
  )]
  study_metadata[,study_name_abbrev_app := paste0(
    environment_number, "-",
    location_name_abbrev
  )]
  study_metadata[, loaded:=F]
  return(data.table(study_metadata))
}

# Modal for group creation
# can be called from scatterplot or groups_sidebar modules
# parent_session enables to get the app server namespace and get modal elements from the 2 modules
#' @export
groupModal <- function(rv, parent_session, modal_title, group_description, group_prefix="M_Group") {
  req(rv$selection[,.N]>0)
  ns <- parent_session$ns
  modalDialog(
    title = modal_title,
    fade = F,
    tagList(
      tags$label(paste(rv$selection[,N]," selected germplasms")),
      tags$p(rv$selection[,germplasmNames_label]),
      textInput(
        ns("modal_create_group_text_input_label"), 
        label = tags$label("Group Name", class = "required"), 
        value = paste(group_prefix, rv$selection[,group_id]), 
        placeholder = "Group Label"
      ),
      textAreaInput(
        ns("modal_create_group_text_input_descr"), 
        label = tags$label("Group Description", class = "required"), 
        placeholder = "Group Description", 
        resize = "vertical",
        value = group_description
      )
    ),
    footer = tagList(
      modalButton("Cancel"),
      actionButton(ns("modal_create_group_go"), label = "Create", class = "btn btn-info")
    )
  )
}

# Modal to rename a group
# can be called from scatterplot or groups_sidebar modules
# parent_session enables to get the app server namespace and get modal elements from the 2 modules
#' @export
renameGroupModal <- function(rv, parent_session) {
  req(!is.null(rv$selected_group_id))
  selected_group <- rv$groups[group_id  == rv$selected_group_id,]
  ns <- parent_session$ns
  modalDialog(
    title = "Rename group",
    fade = F,
    tagList(
      textInput(ns("modal_rename_group_text_input_label"), label = "Group Name", value = selected_group$group_name, placeholder = "Group Label"),
      textAreaInput(
        ns("modal_rename_group_text_input_descr"), 
        label = "Group Description", 
        placeholder = "Group Description", 
        resize = "vertical",
        value = selected_group$group_desc
      )
    ),
    footer = tagList(
      modalButton("Cancel"),
      actionButton(ns("modal_rename_group_go"), label = "Rename", class = "btn btn-info")
    )
  )
}

whoami_bmsapi <- function(con){
  progs <- brapir::core_programs_get(con)$data
  aprogr <- progs$programDbId[1]
  if (con$secure) {
    protocol <- "https://"
  } else {
    protocol <- "http://" 
  }
  server_url <- paste0(protocol, con$db, ":", con$port, "/", con$apipath)
  callurl <- paste0(server_url, "/users/filter?cropName=",con$commoncropname,"&programUUID=",aprogr)
  resp <-   httr::GET(url = callurl,
                      httr::timeout(25),
                      httr::add_headers(
                        "Authorization" = paste("Bearer", con$token),
                        "Content-Type"= "application/json",
                        "accept"= "*/*"
                      ))
  cont <- httr::content(x = resp, as = "text", encoding = "UTF-8")
  uname <- strsplit(con$token,split = ":")[[1]][1]
  return(data.table(jsonlite::fromJSON(cont))[username==uname])
}


#' @title bmsapi_post_germplasm_search
#' @description post a germplasm search. As compared to BrAPI implementation this call allows for incomplete search on names (starts with, ends with and contains)
#' @param con a brapriv2 connection object
#' @param gids a vector of gids
#' @param nameFilter see details
#' @param page
#' @param pageSize
#'
#' @details nameFilter is a list with two character components:
#'    \describe{
#'      \item{type}{one of the following values : STARTSWITH, ENDSWITH, EXACTMATCH, CONTAINS.}
#'      \item{value}{the text string to search for}
#'    }


#' @return
#' @export
#' @import httr
#' @import jsonlite
#' @examples
bmsapi_post_germplasm_search <- function(con = NULL,
                                         gids='',
                                         nameFilter=NULL,
                                         page = 0,
                                         pageSize = 1000){
  mf <- match.call()
  mf <- mf[-1]
  mf <- mf[!names(mf)%in%c("con","page","pageSize")]
  args <- lapply(names(mf), function(a) get(a))
  names(args) <- names(mf)
  if (length(args)==0) args<-NULL
  url1 <- bmscon_geturl(con)
  cropdb <- con$commoncropname
  url <- paste0(url1, "/crops/",cropdb,"/germplasm/search")
  url <- httr::modify_url(url, query=list( page=page, pageSize=pageSize))
  resp <- httr::POST(url,
                     accept_json(),
                     content_type_json(),
                     body = jsonlite::toJSON(args, auto_unbox = T),
                     httr::add_headers(Authorization=paste("Bearer", con$token)),
                     encode = "json")
  return(fromJSON(rawToChar(resp$content)))
}


#' bmsapi_get_germplasm_search_searchResultsDbId
#'
#' @param con
#' @param searchRequestId
#'
#' @return
#' @export
#' @import httr
#' @import jsonlite
#' @examples
bmsapi_get_germplasm_search_searchResultsDbId <- function(con = NULL,  searchRequestId=''){
  url1 <- bmscon_geturl(con)
  cropdb <- con$commoncropname
  url <- paste0(url1, "/crops/",cropdb,"/germplasm/search?searchRequestId=",searchRequestId)
  resp <- httr::GET(url,
                    accept_json(),
                    httr::add_headers(Authorization=paste("Bearer", con$token)))
  respc <- rawToChar(resp$content)
  if (respc=="[]"){
    return(data.frame())
  } else {
    return(fromJSON(respc))
  }
}

#' Title
#'
#' @param con
#'
#' @return
#' @export
#' @import httr
#' @examples
bmscon_geturl <- function(con){
  if (is.null(con))
    return(NULL)
  if (!is.null(con$apipath)) {
    con$apipath <- paste0("/", con$apipath)
  }
  if (con$secure) {
    con$protocol <- "https://"
  }
  port <- ifelse(con$port == 80, "", paste0(":", con$port))
  url <- paste0(con$protocol, con$db, port, con$apipath)
  return(url)
}


rename_envs <- function(TD, old, new){
  names(TD) <- new[match(names(TD),old)]
  TD <- lapply(TD, function(a) {
    a$trial <- new[match(a$trial,old)]
    return(a)
  })
  return(TD)
}

#' @export
generate_ui_with_grid <- function(num_rows, num_cols, choices, ns=ns, control_label_stem="Field") {
  # Créer une liste pour stocker les lignes
  rows_list <- list()
  num <- 0
  for (i in 1:num_rows) {
    # Créer une liste pour stocker les colonnes de la ligne actuelle
    columns_list <- list()
    
    for (j in 1:num_cols) {
      num <- num + 1
      # Ajouter une colonne à la liste avec un selectInput
      columns_list[[j]] <- column(
        width = 12 / num_cols,
        selectInput(ns(paste0("select_", num)), paste(control_label_stem, num),
                    choices = choices,
                    selected = choices[num])
      )
    }
    
    # Ajouter la ligne à la liste des lignes
    rows_list[[i]] <- fluidRow(columns_list)
  }
  
  # Retourner un div contenant toutes les lignes
  return(div(rows_list))
}

# Function to get variable methods
#' @export
get_BLUES_methodsDbIds <- function(con, programDbId) {
  methodNames = list(
    "BLUEs" = "STABrAPP BLUES", 
    "BLUPs" = "STABrAPP BLUPS", 
    "seBLUEs" = "STABrAPP SEBLUES", 
    "seBLUPs" = "STABrAPP SEBLUPS"
  )
  if (con$secure) {
    protocol = "https://"
  } else {
    protocol = "http://"
  }
  callurl <- paste0(protocol, con$db, ":", con$port, "/", con$apipath, "/crops/", con$commoncropname, "/methods?programUUID=", programDbId)
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
  if (resp$status_code == 200) {
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
  } else if (resp$status_code == 401) {
    stop("Couldn't retrieve BLUES/BLUPS methods, please authenticate again in BMS")
  } else {
    stop("Couldn't retrieve BLUES/BLUPS methods, you won't be able to push BLUEs/BLUPs")
  }
  
  return(methodIds)
}


# Function to construct colgeno vector of named colors from a factor of genotypes descriptors
# and using two possible palettes one for a few levels, and the other for many levels
colgeno <- function(genofac, shortpal=getOption("statgen.genoColors"), longpal=topo.colors, missing="Unknown", missing.col="grey"){
  if (!is.null(genofac)){
    if (length(shortpal) >= length(genofac)) {
      if (any(genofac==missing)){
        colGeno <- setNames(c(shortpal[1:(length(genofac)-1)],missing.col), c(setdiff(genofac,missing),missing))
      } else {
        colGeno <- setNames(shortpal[1:length(genofac)],genofac)
      }
    }
    else {
      if (any(genofac==missing)){
        colGeno <- setNames(c(longpal(length(genofac)-1),missing.col), c(setdiff(genofac,missing),missing))
      } else {
        colGeno <- setNames(longpal(length(genofac)),genofac)
      }
    }
  } else {
    colGeno <- NULL
  }
  return(colGeno)
}

#' Save rv in .rds file
#' @param rv 
#'
#' @importFrom later later
#' @export
save_user_data <- function(rv) {
  if (!is.null(rv$hash)) {
    filename <- paste0(rv$hash, ".rds")
    snapshot <- list(
      con = rv$con,
      connect_mode = rv$connect_mode,
      data = rv$data,
      excluded_obs = rv$excluded_obs,
      obs_unit_level = rv$obs_unit_level,
      study_metadata = rv$study_metadata,
      trial_metadata = rv$trial_metadata,
      extradata = rv$extradata,
      groups = rv$groups,
      selection = rv$selection,
      column_datasource = rv$column_datasource,
      environmentParameters = rv$environmentParameters
    )
    # remove all NULL elements to avoid error notification (nrow dimension error)
    snapshot <- snapshot[!vapply(snapshot, is.null, logical(1))]
    later(function() {
      saveRDS(snapshot, filename)
      print("rv saved !")
    }, 0)
  }
}

#' @export
update_selectors_with_groups <- function(rv, new_group, initial_name = NULL) {
  ## update selectors (shape, colour)
  data_plot <- copy(rv$extradata) # to avoid issues related to assignment by reference
  data_plot[germplasmDbId %in% new_group[,unlist(germplasmDbIds)], eval(new_group$group_name) := paste0('In')]
  data_plot[!(germplasmDbId %in% new_group[,unlist(germplasmDbIds)]), eval(new_group$group_name) := paste0('Out')]

  if (!is.null(initial_name)) { # renaming a group
    data_plot[, eval(initial_name) := NULL]
    rv$column_datasource[cols == initial_name & source == "group", cols := new_group$group_name]
  } else { # add a new group
    rv$column_datasource <- rbindlist(
      list(
        rv$column_datasource,
        data.table(cols = new_group$group_name, source = "group", type = "Text", visible = T)
      ),
      use.names = T
    )
  }
  
  rv$new_group_created <- T #to avoid environments selection reset
  rv$extradata <- data_plot
}

#' @export
summary.stats <- function(x){
  x <- x[!is.na(observationValue)]
  sumtable_notexcl_nodat <- x[scale.dataType != "Date", .(
    "Environment" = study_name_app,
    "No. of observations" = .N,
    "Mean" = mean(observationValue, na.rm = T),
    "Minimum" = min(observationValue, na.rm = T),
    "Quantile 0.25" = quantile(observationValue, probs = c(0.25), na.rm = TRUE),
    "Median" = quantile(observationValue, probs = c(0.5),na.rm = TRUE),
    "Quantile 0.75" = quantile(observationValue, probs = c(0.75),na.rm = TRUE),
    "Maximum" = max(observationValue, na.rm = T),
    "Standard deviation" = sd(observationValue, na.rm = T),
    "Variance" = var(observationValue, na.rm = T),
    "Sum of values" = sum(observationValue, na.rm = T),
    "Sum of squares" = sum((
      observationValue - mean(observationValue, na.rm = T)
    ) ^ 2),
    "Uncorrected sum of squares" = sum(observationValue ^ 2, na.rm = T),
    "Skewness" = e1071::skewness(observationValue, na.rm = TRUE),
    "Kurtosis" = e1071::kurtosis(observationValue, na.rm = TRUE)
  ), .(study_name_app, observationVariableName)]
  
  if (any(x$scale.dataType == "Date")){
    sumtable_notexcl_dat <- x[scale.dataType == "Date", .(
      "Environment" = study_name_app,
      "No. of observations" = .N,
      "Mean" = mean(observationValue, na.rm = T),
      "Minimum" = min(observationValue, na.rm = T),
      "Quantile 0.25" = quantile(observationValue, type=1, probs = c(0.25), na.rm = TRUE),
      "Median" = quantile(observationValue, type=1, probs = c(0.5),na.rm = TRUE),
      "Quantile 0.75" = quantile(observationValue, type=1, probs = c(0.75),na.rm = TRUE),
      "Maximum" = max(observationValue, na.rm = T),
      "Standard deviation" = sd(observationValue, na.rm = T),
      "Variance" = var(observationValue, na.rm = T),
      "Sum of values" = sum(observationValue, na.rm = T),
      "Sum of squares" = sum((
        observationValue - mean(observationValue, na.rm = T)
      ) ^ 2),
      "Uncorrected sum of squares" = sum(observationValue ^ 2, na.rm = T),
      "Skewness" = e1071::skewness(observationValue, na.rm = TRUE),
      "Kurtosis" = e1071::kurtosis(observationValue, na.rm = TRUE)
    ), .(study_name_app, observationVariableName)]
    
    sumtable_notexcl <- rbind(sumtable_notexcl_nodat,sumtable_notexcl_dat)        
  } else {
    sumtable_notexcl <- sumtable_notexcl_nodat
  }
  
  
  sumtable_notexcl[, "Standard error of mean" := `Standard deviation` /
                     sqrt(`No. of observations`)]
  sumtable_notexcl[, "Standard error of variance" := `Variance` /
                     sqrt(`No. of observations`)]
  sumtable_notexcl[, "%cov" := `Variance` / `Mean`]
  sumtable_notexcl[, "%Standard error of skewness" := `Skewness` /
                     sqrt(`No. of observations`)]
  sumtable_notexcl[, "%Standard error of kurtosis" := `Kurtosis` /
                     sqrt(`No. of observations`)]
  sumtable_notexcl[, "Range" := Maximum - Minimum]
  
  columns <- c(
    "Environment",
    "observationVariableName",
    #"No. of values",
    "No. of observations",
    #"No. of excluded values",
    "Mean",
    "Minimum",
    "Quantile 0.25",
    "Median",
    "Quantile 0.75",
    "Maximum",
    "Range",
    "Standard deviation",
    "Standard error of mean",
    "Variance",
    "Standard error of variance",
    "%cov",
    "Sum of values",
    "Sum of squares",
    "Uncorrected sum of squares",
    "Skewness",
    "%Standard error of skewness",
    "Kurtosis",
    "%Standard error of kurtosis"
  )
  setkey(sumtable_notexcl, study_name_app)
  return(sumtable_notexcl[, columns, with = F])
}