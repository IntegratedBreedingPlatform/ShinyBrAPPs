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
      observations.observationDbId
    ]
    return(obs)
  }else if(!is.null(input_click)){
    obs <- d[
      (abs(input_click$x-positionCoordinateX)<=int_x/2) & (abs(input_click$y-positionCoordinateY)<=int_y/2),
      observations.observationDbId
    ]
    return(obs)
  }

}

#' @export
get_env_data <- function(con, studyDbId, environment_number, location_name, location_name_abbrev, study_name_app, study_name_abbrev_app){
  study <- data.table()
  try({
    study <- as.data.table(brapirv1::brapi_get_studies_studyDbId_observationunits(con = con, studyDbId = studyDbId))
    if(!("observations.value"%in%names(study))){
      study[,observations.value:=NA]
    }
    study[,observations.value:=as.numeric(observations.value)] # XXX this should not always be the case
    study[, locationName:=location_name]
    study[, environmentNumber:=environment_number]

    study[,study_name_BMS := paste0(
      environment_number, "-",
      location_name
    )]
    study[,environment_number := environment_number]
    study[,location_name := location_name]
    study[,location_abbrev := location_name_abbrev]
    study[,study_name_app := study_name_app]
    study[,study_name_abbrev_app := study_name_abbrev_app]
  })
return(study)
}

