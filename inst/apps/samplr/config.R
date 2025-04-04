brapi_version <- "2.0"
appname <- "SampleR"

notification_duration = 10 # in seconds
label_sizes <- fread("data/common_labels.csv")
label_layouts <- jsonlite::fromJSON(paste(readLines("data/layouts.json"), collapse=""), simplifyMatrix = F)
barcode_field <- "sampleDbId"
