#' Single Trial Analysis report generation as Word document using officer
#'
#' @param fit 
#' @param file 
#' @param template 
#' @param trialName 
#' @param spatial 
#' @param toc 
#' @param table.dec 
#'
#' @returns
#' @export
#' @import officer
#' @import cowplot
#' @import ggplot2
#'
#' @examples
stareport <- function(fit, file=file, template=templt, trialdesc="", trialName="", crop="", outliers=NULL, excluded=NULL, spatial=FALSE, toc=TRUE, table.dec=4){
  my_doc <- read_docx(template)
  ## Add metadata
  metatab <- data.frame(V1=c("Date - Time",
                             "Crop",
                             "Trial",
                             "Description",
                             "Design",
                             "Engine"),
                        V2=c(format(Sys.time(), '%d %B, %Y - %H:%M:%S'),
                             crop,
                             trialName,
                             trialdesc,
                             fit[[1]]$design,
                             fit[[1]]$engine))
  my_doc<-my_doc%>%cursor_bookmark("meta")%>%
    body_add_table(value = metatab, pos="after" , header = F, alignment = c("left","right"), style = "meta_table")
  
  my_doc <- my_doc%>%cursor_end()
  my_doc<-my_doc%>%body_add_break()
  fitex <- statgenSTA::extractSTA(fit,what="all")
  
  for(st in seq_along(fit)){
    stname <- names(fit)[st]
    my_doc<-my_doc%>%
      body_add_par(value = paste0("Study: ",stname),style = "heading 1",pos="after" )
    fitst <- fit[[st]]
    traits <- fitst$traits
    for (tr in seq_along(traits)){
      incProgress(amount = 1, message = paste0("Building report - Study ",st,"/",length(fit)," - Variable ",tr,"/",length(traits)))
      my_doc<-my_doc%>%
        body_add_par(value = paste0("Variable: ",traits[tr]),style = "heading 2",pos="after" )
      fitsum <- statgenSTA:::summary.STA(fit, trials = stname, trait = traits[tr])
      ss <- data.table(statistic=attr(fitsum$stats,which="dimnames")[[1]], value=as.numeric(fitsum$stats))
      ss$value[ss$value!=0] <- gsub("\\.0+$","",round(ss$value[ss$value!=0],table.dec))
      my_doc<-my_doc%>%
        body_add_par(value = "Summary Statistics",style = "heading 3",pos="after" )
      my_doc<-my_doc%>%
        body_add_table(value = ss,pos="after" ,header = F, alignment = c("left","right"), style = "STA_Table")
      if (!is.null(excluded)){
        excltr <- excluded[study_name_app==stname & observationVariableName==traits[tr],.(germplasmName, replicate, plot = observationLevelCode , observationVariableName, observationValue)]
        if (nrow(excltr)>0){
          my_doc<-my_doc%>%
            body_add_par(value = "Excluded observations",style = "heading 3",pos="after" )
          my_doc<-my_doc%>%
            body_add_table(value = excltr,pos="after" ,header = T, alignment = c("left","right"), style = "STA_Table")
        }
      }
      my_doc<-my_doc%>%
        body_add_par(value = "Model plots",style = "heading 3",pos="after" )
      gg <- statgenSTA:::plot.STA(fit, trials = stname, traits = traits[tr], output = FALSE )[[1]][[1]]
      title <- cowplot::ggdraw() + 
        cowplot::draw_label(traits[tr], fontface = 'bold', x = 0, hjust = 0, size=12) +
        ggplot2::theme(plot.margin = margin(0, 0, 0, 7))
      ggg <- cowplot::plot_grid(title, cowplot::plot_grid(plotlist = gg, ncol = 2), ncol=1, rel_heights = c(0.1, 1))
        my_doc<-my_doc%>%
          body_add_gg(value = ggg, height = length(gg)*1.25)
        if (spatial){
          my_doc<-my_doc%>%
          body_add_par(value = "Spatial plots",style = "heading 3",pos="after" )
          ggs <- statgenSTA:::plot.STA(fit, trials = stname, traits = traits[tr], plotType = "spatial", output = FALSE )[[1]][[1]]
          #browser()
          gggs <- cowplot::plot_grid(title, cowplot::plot_grid(plotlist = ggs, ncol = 2), ncol=1, rel_heights = c(0.1, 1))
          my_doc<-my_doc%>%
            body_add_gg(value = gggs, height = length(ggs)*1.25)
          
        }
        mstats <- data.frame(V1=c("Heritability",
                               "Genetic Variance component",
                               "Residual Variance component",
                               "CV"),
                             V2=c(fitex[[st]]$heritability[[tr]],
                               fitex[[st]]$varGen[[tr]],
                               fitex[[st]]$varErr[[tr]],
                               fitex[[st]]$CV[[tr]]))
        if (fit[[st]]$engine=="lme4"){
          wald <- fitex[[st]]$wald[[tr]]
          mstats <- rbind(mstats,data.frame(V1=paste0("wald.",names(wald)),V2=t(wald)[,1]))
        } else {
          if (fit[[st]]$engine=="SpATS"){
            varSpat <- fitex[[st]]$varSpat[,tr]
            mstats <- rbind(mstats,data.frame(V1=paste0("varSpat.",names(varSpat)),V2=varSpat))
          }
        }
        mstats$V2[mstats$V2!=0] <- gsub("\\.0+$","",round(mstats$V2[mstats$V2!=0],table.dec))
        
        my_doc<-my_doc%>%
          body_add_par(value = "Model statistics",style = "heading 3",pos="after" )%>%
          body_add_table(value = mstats, pos="after" ,header = F, alignment = c("left","right"),style = "STA_Table")
          #body_add_par(value = paste0("Heritability: ",round(fitsum$heritability,2)),style = "Normal",pos="after" )
        if (!is.null(outliers)){
          if (nrow(outliers[outliers$trait==traits[tr] & outliers$trial==stname,])>0){
            my_doc<-my_doc%>%
              body_add_par(value = "Candidate outliers",style = "heading 3",pos="after" )%>%
              body_add_table(value = outliers[outliers$trait==traits[tr] & outliers$trial==stname,c("genotype", "trait","outlier", "value")], pos="after" ,header = T, alignment = c("left","right"),style = "STA_Table")
          }
        }
        best20 <- summary(fit,trait=traits[tr], trial=stname)$meanTab
        colnames(best20)[seq(2,ncol(best20),by=2)] <- paste(colnames(best20)[seq(2,ncol(best20),by=2)],colnames(best20)[seq(1,ncol(best20),by=2)], sep="_")
        best20 <- data.frame(genotype=row.names(best20), best20)
        my_doc<-my_doc%>%
        body_add_par(value = paste0(traits[tr], " - Predicted means (BLUEs & BLUPs) - Best 20 genotypes"),style = "heading 3",pos="after" )%>%
        body_add_table(value = best20, pos="after" ,header = T,style = "STA_Table_boldh")
        
        my_doc<-my_doc%>%body_add_break()
    }
  }
  
  if(fit[[1]]$engine=="lme4"){
    metrics <- rbindlist(Map(function(f,t) data.table(Environment=t,Trait=names(f$heritability),Heritability=f$heritability, CV=f$CV, `Wald p.value`=unlist(lapply(f$wald,function(a) a$`p.value`))),fitex, names(fitex)))
  }else{
    metrics <- rbindlist(Map(function(f,t) data.table(Environment=t,Trait=names(f$heritability),Heritability=f$heritability),fitex, names(fitex)))
  }
  my_doc<-my_doc%>%
    body_add_par(value = "Model Statistics Summary",style = "heading 1",pos="after" )%>%
    body_add_table(value = metrics, pos="after" ,header = T,style = "STA_Table_boldh")
  
  if (toc){
    my_doc<-my_doc%>%
      cursor_bookmark(id = "TOC")%>%body_add_toc(level = 2)
  }
  incProgress(amount = 1, message = "Printing report")
  
  print(my_doc, target = file)
}

