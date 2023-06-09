

cl_phv = c("entire_cell_cd19_opal_480_min", "entire_cell_cd19_opal_480_max", 
"entire_cell_cd19_opal_480_std_dev", "entire_cell_cd19_opal_480_total", 
"entire_cell_dapi_dapi_min", "entire_cell_dapi_dapi_max", "entire_cell_dapi_dapi_std_dev", 
"entire_cell_dapi_dapi_total", "entire_cell_autofluorescence_min", 
"entire_cell_autofluorescence_max", "entire_cell_autofluorescence_std_dev", 
"entire_cell_autofluorescence_total")

phv = c("phenotype_cd68", "phenotype_ki67", 
"phenotype_ck", "phenotype_cd19", "phenotype_p_stat3", "phenotype_cd3", 
"phenotype_cd8")

library(SpatialExperiment)
library(ggplot2)
library(shiny)
library(DT)
library(dplyr)

show_core = function(spe, colvar = "phenotype_cd8", ...) {
  xy = spatialCoords(spe)
  datf = data.frame(x=xy[,1], y=xy[,2])
  datf$feat = factor(colData(spe)[, colvar])
  ggplot(datf, aes(x=x, y=y, colour=feat)) + geom_point(...)
}
 
load("newov.rda")
show_core(newov)

if (!exists("ovVP")) load("ovVP.rda")

ovVP$nid = as.numeric(factor(ovVP$sample_id))

extract_core = function(id, spe) {
 stopifnot("nid" %in% names(colData(spe)))
 spe[, which(spe$nid==id)]
}

colNplus = function(datf) {
 ans = do.call(cbind, lapply(datf, function(x) length(grep("\\+$", x))))
 names(ans) = names(datf)
 ans
}


get_summaries = function(spe, phv) {
  dat = as.data.frame(colData(spe)[, c("nid", phv) ])
  sdat = split(dat, nid)
  ans = data.frame(do.call(rbind, lapply(sdat, colNplus)))
  ans$nid = names(sdat)
  ans
}

display_by_core = function(spe, phv) {
 stopifnot("nid" %in% names(colData(spe)))
 ui = fluidPage(
  sidebarLayout(
   sidebarPanel(
    selectInput("core", "core", choices=unique(spe$nid)),
    radioButtons("phenotype", "phenotype", choices=phv)
    ),
   mainPanel(
    tabsetPanel(
     tabPanel("core", plotOutput("coreview")),
     tabPanel("phdata", DT::dataTableOutput("datatab")),
     tabPanel("about", helpText("Data described in PMID 34615692"))
     )
    )
   )
  )
 server = function(input, output) {
  output$coreview = renderPlot({
    newov = extract_core( input$core, spe )
    show_core(newov, input$phenotype)
    })
  output$datatab = DT::renderDataTable({
    get_summaries(spe, phv)
    })
 }
 runApp(list(ui=ui, server=server))
}
   
   
    
