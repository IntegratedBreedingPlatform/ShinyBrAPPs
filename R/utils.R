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
  
  study <- data.table()
  try({
    if (is.null(obs_unit_level)) {
      study_obs <- as.data.table(brapirv2::brapi_get_observationunits(
        con = con,
        studyDbId = studyDbId,
        includeObservations = T)
      )
    } else {
      obs_levels <- data.frame(levelName = obs_unit_level)
      
      res <- brapirv2::brapi_post_search_observationunits(
        con = con, 
        studyDbIds = studyDbId,
        observationLevels = obs_levels,
        includeObservations = T)
      study_obs <- as.data.table(brapirv2::brapi_get_search_observationunits_searchResultsDbId(con, as.character(res)))
      # study_obs2 <- as.data.table(brapirv2::brapi_get_observationunits(
      #   con = con,
      #   studyDbId = studyDbId,
      #   observationUnitLevelName = "PLOT",
      #   includeObservations = T)
      # )
    }

    if (!"observations.observationDbId" %in% colnames(study_obs)) {
      study_obs <- NULL
    } else {
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
        observationLevel = `observationUnitPosition.observationLevel.levelName`, 
        observationLevelCode = `observationUnitPosition.observationLevel.levelCode`, 
        entryType = `observationUnitPosition.entryType`,
        entryNumber = `additionalInfo.ENTRY_NO`,
        levelCode = `observationUnitPosition.observationLevelRelationships.levelCode`,
        levelName = `observationUnitPosition.observationLevelRelationships.levelName`,
        positionCoordinateX = `observationUnitPosition.positionCoordinateX`,
        positionCoordinateY = `observationUnitPosition.positionCoordinateY`,
        observationTimeStamp = `observations.observationTimeStamp`, 
        observationVariableDbId = `observations.observationVariableDbId`, 
        observationVariableName = `observations.observationVariableName`, 
        observationValue = `observations.value`
      )]
      
      grouping_cols <- setdiff(names(study_obs), c("levelCode", "levelName"))
      
      study_obs <- study_obs[, .(plotNumber = levelCode[levelName == "PLOT"],
                                 replicate = levelCode[levelName == "REP"],
                                 blockNumber = levelCode[levelName == "BLOCK"]),
                             by = grouping_cols]
      
      variables <- as.data.table(brapirv2::brapi_get_variables(con = con, studyDbId = studyDbId))
      variables <- variables[, .(observationVariableDbId, scale.dataType)] 
      if (any(colnames(study_obs)=="observationVariableDbId")){
        study_obs <- merge(study_obs, variables, 
                       by.x = "observationVariableDbId", 
                       by.y = "observationVariableDbId")
      }
      
      if(!("observationValue"%in%names(study_obs))){
        study_obs[,observationValue:=NA]
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
    }
  })
  return(study_obs)
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

#' @export
make_study_metadata <- function(con, studyDbIds=NULL, trialDbId= NULL){
  if(!is.null(trialDbId)){
    ## get environment metadata by trialDbId
    tryCatch({
      study_metadata <- as.data.table(brapirv2::brapi_get_studies(con = con, trialDbId = trialDbId))
    },
    error=function(e){
      print(e)
      showNotification(paste0("Environment metadata not found for trialDbId ",trialDbId), type = "error", duration = notification_duration)
    })
  }else if(!is.null(studyDbIds)){
    ## get environment metadata by studyDbId
    ids <- unlist(strsplit(studyDbIds, ","))
    study_metadata <- rbindlist(lapply(ids,function(id){
      tryCatch({
        as.data.table(brapirv2::brapi_get_studies(con = con, studyDbId = id))
      },
      error=function(e){
        showNotification(paste0("Environment metadata not found for studyDbId ",id), type = "error", duration = notification_duration)
      })
    }),use.names = T, fill = T)
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
  loc_names <- brapirv2::brapi_get_search_locations_searchResultsDbId(con, searchResultsDbId = brapirv2::brapi_post_search_locations(con, locationDbIds = location_ids)$searchResultsDbId)
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
      y = study_metadata[environmentParameters.parameterName == "ENVIRONMENT_NUMBER",.(studyDbId, environment_number = environmentParameters.value)],
      by = "studyDbId", all.x = T)
    env_number[is.na(environment_number), environment_number:=studyDbId]
  }else{
    env_number <- study_metadata[,.(studyDbId = unique(studyDbId))]
    env_number[, environment_number:=studyDbId]
  }
  study_metadata <- merge.data.table(
    x = study_metadata,
    y = env_number,
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
