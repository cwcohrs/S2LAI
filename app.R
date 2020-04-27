# Project: Calculate Leaf Area Index from Sentinel-2 Imagery, a Shiny web application
#
#          Access via https://treepoet.com/s2lai
# 
#          For more details on the model used to calculate LAI, please see:  
#          Cohrs, C.W., R.L. Cook, J.M. Gray, and T.J. Albaugh. (2020). Sentinel-2 Leaf Area Index Estimation for Pine Plantations in the Southeastern United States. Remote Sensing. under-review.
#
# Objectives: 
# 1. Allow users to calculate Leaf Area Index on-the-fly when supplied with Sentinel-2 bands 4 and 8 (red and NIR).
# 2. Provide an intuitive UI/UX to enable non-technical users to perform remote sensing image analysis.
# 3. Enable users to calculate mean leaf area index for a given vector polygon boundary, or combination of boundaries (e.g. multiple forest stands)
# 4. 
#
#
# Date last modified: 31 Mar 2020
#             Author: Chris Cohrs | cohrs.xyz@gmail.com | treepoet.com
#
#############
############
###########
##########
#########
########
#######
######
#####
####
###
##
#

options(shiny.maxRequestSize = 3000*1024^2) #increases maximum upload size to about a bajillion

library(shiny)
library(dplyr)
library(shinycssloaders)
library(rgdal)
library(raster)
library(rasterVis)
library(sf)

cohrs_LAI <- function(inFileb4, inFileb8) {
  
  inFileb4 <- raster(inFileb4)
  inFileb8 <- raster(inFileb8)
  
  # rescale 0 to 1
  b4_norm <- inFileb4 * 0.0001
  
  b8_norm <- inFileb8 * 0.0001
  
  # convert to Simple Ratio
  b8b4_sr <- b8_norm / b4_norm
  
  # use Cohrs equation to estimate LAI
  ((b8b4_sr * 0.31034) - 0.09776)
  
}

match_crs <- function(inFileb4, inFileAOI) {
  #get the CRS of inFileb4, and assign it to a new object
  spTransform(inFileAOI, crs(raster(inFileb4)))
  #assign CRS to inFileAOI 
}

clip_raster_to_aoi <- function(inFileb4, inFileb8, inFileAOI) {
  
  inFileb4 <- raster(inFileb4)
  inFileb8 <- raster(inFileb8)
  inFileAOI <- vector(file)
  
}

## Set up color gradient with 100 values between 0.0 and 6.0

breaks <- seq(0, 6, by = 0.10)
cols <- colorRampPalette(c("white", "lightgreen", "darkgreen"))

#############
############
###########
##########
#########
########
#######
######
#####
####
###
##
#

ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
      .shiny-output-error-validation {
        color: gray;
      }
    "))
  ),
  
  titlePanel(title = p("Calculate Leaf Area Index from Sentinel-2 Imagery", style = "color:#3474A7"), windowTitle = "Sentinel-2 LAI Calculator"),
  sidebarLayout(
    sidebarPanel(
      fileInput(
        inputId = "filerasterS2B4",
        label = "Upload raster: Sentinel-2 | Level-2A | 10 m | Band 4 (Red)",
        accept = c(".jp2",".tif")
      ),
      fileInput(
        inputId = "filerasterS2B8",
        label = "Upload raster: Sentinel-2 | Level-2A | 10 m | Band 8 (NIR)",
        accept = c(".jp2",".tif")
      ),
      fileInput(
        inputId = "filevector",
        label = "Upload vector: Shapefile | AOI Polygon(s)",
        multiple = TRUE,
        accept = c(".shp",".dbf",".sbn",".sbx",".shx",".prj")
      ),
      actionButton("renderClip", "Render LAI Output, clipped to AOI"),
      actionButton("renderFull", "Render LAI Output, entire scene"),
      # actionButton("renderCombo", "Render LAI Output + AOI"),
      htmlOutput("linebreak1"),
      downloadButton("downloadDataClipped", "Download LAI Output, clipped to AOI"),
      downloadButton("downloadDataEntire", "Download LAI Output, entire scene"),
      htmlOutput("linebreak2"),
      p("Note: Depending on dataset filesize, it may take a minute for the download to begin."),
      htmlOutput("linebreak3"),
      p("Need Sentinel-2 data and don't know where to get it?", a("Click here for a walkthrough.",
                                                                  href = "https://eae59453-626c-435a-aeb7-8b2466da9365.usrfiles.com/ugd/eae594_17089add367c437f9bd331ac98a815f6.pdf"
      )),
      htmlOutput("linebreak4"),
      p("For more details on the model used, please see", a("Cohrs, C.W., R.L. Cook, J.M. Gray, and T.J. Albaugh. (2020). Sentinel-2 Leaf Area Index Estimation for Pine Plantations in the Southeastern United States. Remote Sensing. under-review.",
                                                            href = "https://treepoet.com"
      ), "(temporarily redirecting)"),
      tags$a(
        href = "https://treepoet.com", 
        tags$img(src = "treepoet_v2.png", 
                 title = "Made by Chris Cohrs", 
                 width = "20px",
                 height = "30px")
      )
    ),
    
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  tabPanel("LAI Plot", htmlOutput("linebreak5"), plotOutput("uploadmapclip") %>% withSpinner(color = "#0dc5c1"), plotOutput("b8b4_cohrsEq_done") %>% withSpinner(color = "#0dc5c1")),
                  tabPanel("AOI Plot", htmlOutput("linebreak6"), plotOutput("uploadmapmap"), dataTableOutput('uploadmaptable'), verbatimTextOutput("uploadmapsummary"), verbatimTextOutput("uploadmapextent")),
                  tabPanel("Band Input Plots", plotOutput("Band_4"), p("Above: Band 4 (Red)"), plotOutput("Band_8"), p("Above: Band 8 (NIR)")),
                  tabPanel("Summary Tables", verbatimTextOutput("b8b4_cohrsEq_done_summary"))
      )
      
    )
    
  )
)

#############
############
###########
##########
#########
########
#######
######
#####
####
###
##
#

server <- function(input, output) {
  
  # INI reactiveValues
  rv <- reactiveValues(map = NULL)
  
  output$Band_4 <- renderPlot({
    validate(need(input$filerasterS2B4$datapath != "", "Please select a Band 4 dataset"))
    plot(raster(input$filerasterS2B4$datapath))
  })
  output$Band_8 <- renderPlot({
    validate(need(input$filerasterS2B8$datapath != "", "Please select a Band 8 dataset"))
    plot(raster(input$filerasterS2B8$datapath))
  })
  
  b8b4_cohrsEq_done <- eventReactive(input$renderFull,{
    levelplot(cohrs_LAI(input$filerasterS2B4$datapath, input$filerasterS2B8$datapath), at = breaks, col.regions = cols, main = "Leaf Area Index, estimated", colorkey = list(title = "LAI", title.gpar = list(cex = 1, font = 2, col = 'darkgreen', lineheight = 2)), margin = FALSE)
  })
  
  output$b8b4_cohrsEq_done <- renderPlot({
    validate(need(input$filerasterS2B4$datapath != "" & input$filerasterS2B8$datapath != "", "Please select both Band 4 and Band 8 datasets for Leaf Area Index to be calculated. Then choose whether to render the LAI output clipped to an input AOI (faster, recommended, but requires input AOI), or to render the entire LAI output image (render speed will depend on input sizes of Bands 4 & 8)"))
    b8b4_cohrsEq_done()
  })
  
  output$b8b4_cohrsEq_done_summary <- renderPrint({
    validate(need(input$filerasterS2B4$datapath != "" & input$filerasterS2B8$datapath != "", "Please select both Band 4 and Band 8 datasets for raster summary statistics to be shown."))
    print(summary(cohrs_LAI(input$filerasterS2B4$datapath, input$filerasterS2B8$datapath)))
  })
  
  # START OF SHAPEFILE WORK
  
  
  output$uploadmapmap <- renderPlot({
    if (is.null(rv$map))
      return(NULL)
    plot(rv$map)
  })
  
  
  output$uploadmapsummary <- renderPrint({
    if (!is.null(rv$map)) {
      print(summary(rv$map@data))
    }
  })
  
  output$uploadmapextent <- renderPrint({
    if (!is.null(rv$map)) {
      print(extent(rv$map))
    }
  })
  
  output$uploadmaptable  <- renderDataTable({
    if (is.null(rv$map))
      return(NULL)
    rv$map@data
  })
  
  # Upload shapefile
  observe({
    shpdf <- input$filevector
    if (is.null(shpdf)) {
      return()
    }
    previouswd <- getwd()
    uploaddirectory <- dirname(shpdf$datapath[1])
    setwd(uploaddirectory)
    for (i in 1:nrow(shpdf)) {
      file.rename(shpdf$datapath[i], shpdf$name[i])
    }
    setwd(previouswd)
    
    #map <- readShapePoly(paste(uploaddirectory, shpdf$name[grep(pattern="*.shp", shpdf$name)], sep="/"),  delete_null_obj=TRUE)
    #reads the file that finishes with .shp using $ at the end: grep(pattern="*.shp$", shpdf$name)
    map <- readOGR(paste(uploaddirectory, shpdf$name[grep(pattern = "*.shp$", shpdf$name)], sep = "/"))#,  delete_null_obj=TRUE)
    map <- spTransform(map, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
    rv$map <- map
    
  })  
  
  ##
  uploadmapclip <- eventReactive(input$renderClip,{
    if (is.null(rv$map))
      return(NULL)
    shpdf <- input$filevector
    if (is.null(shpdf)) {
      return()
    }
    previouswd <- getwd()
    uploaddirectory <- dirname(shpdf$datapath[1])
    setwd(uploaddirectory)
    for (i in 1:nrow(shpdf)) {
      file.rename(shpdf$datapath[i], shpdf$name[i])
    }
    setwd(previouswd)
    
    map <- readOGR(paste(uploaddirectory, shpdf$name[grep(pattern = "*.shp$", shpdf$name)], sep = "/"))#,  delete_null_obj=TRUE)
    map <- spTransform(map, crs(raster(input$filerasterS2B4$datapath)))
    rv$map <- map
    plot(crop(cohrs_LAI(input$filerasterS2B4$datapath, input$filerasterS2B8$datapath), extent(rv$map)))
    plot(rv$map, add = TRUE)
  })
  
  output$uploadmapclip <- renderPlot({
    uploadmapclip()
  })
  ##
  
  # END OF SHAPEFILE WORK
  
  output$linebreak1 <- renderText({
    paste0("</p>")
  })
  output$linebreak2 <- renderText({
    paste0("</p>")
  })
  output$linebreak3 <- renderText({
    paste0("</p>")
  })
  output$linebreak4 <- renderText({
    paste0("</p>")
  })
  output$linebreak5 <- renderText({
    paste0("</p>")
  })
  output$linebreak6 <- renderText({
    paste0("</p>")
  })
  
  # Downloadable tif of selected dataset ----
  output$downloadDataEntire <- downloadHandler(
    filename = 'LAI_output_entire.tif',
    content = function(file) {
      shiny::withProgress(
        message = paste0("Downloading Data"),
        value = 0,
        {
          shiny::incProgress(1/10)
          Sys.sleep(1)
          shiny::incProgress(5/10)
          r <- cohrs_LAI(input$filerasterS2B4$datapath, input$filerasterS2B8$datapath)
          res <- writeRaster(r, filename = file, format = "GTiff", overwrite = TRUE)
        }
      )
      
      # Show the corresponding output filename
      print(res@file@name)
      
      # Rename it to the correct filename
      file.rename(res@file@name, file)
    }
  )
  
  output$downloadDataClipped <- downloadHandler(
    filename = 'LAI_output_clipped.tif',
    content = function(file) {
      shiny::withProgress(
        message = paste0("Downloading Data"),
        value = 0,
        {
          shiny::incProgress(1/10)
          Sys.sleep(1)
          shiny::incProgress(5/10)
          shpdf <- input$filevector
          if (is.null(shpdf)) {
            return()
          }
          previouswd <- getwd()
          uploaddirectory <- dirname(shpdf$datapath[1])
          setwd(uploaddirectory)
          for (i in 1:nrow(shpdf)) {
            file.rename(shpdf$datapath[i], shpdf$name[i])
          }
          setwd(previouswd)
          
          map <- readOGR(paste(uploaddirectory, shpdf$name[grep(pattern = "*.shp$", shpdf$name)], sep = "/"))#,  delete_null_obj=TRUE)
          map <- spTransform(map, crs(raster(input$filerasterS2B4$datapath)))
          rv$map <- map
          r <- crop(cohrs_LAI(input$filerasterS2B4$datapath, input$filerasterS2B8$datapath), extent(rv$map))
          res <- writeRaster(r, filename = file, format = "GTiff", overwrite = TRUE)
        }
      )
      
      # Show the corresponding output filename
      print(res@file@name)
      
      # Rename it to the correct filename
      file.rename(res@file@name, file)
    }
  )
}

# shinyApp()
shinyApp(ui = ui, server = server)

# fin

##############
############
###########
##########
#########
########
#######
######
#####
####
###
##
#
# 
# ........................................................................................................................
# ........................................................................................................................
# ........................................................................................................................
# ........................................................................................................................
# ........................................................................................................................
# ........................................................................................................................
# .........................................................=...=..........................................................
# ......................................................=.====.===.=.=....................................................
# ....................................................========.==========.................................................
# ...............................................===========I================.............................................
# .........................................=....========I===II=I=I=III=I======............................................
# .........................................=.=.======I==III=II=III=II==I======............................................
# .......................................=.==========II=IIIIII=I=IIII=I=====I=.===........................................
# .....................................=..========I===IIIIIIII=IIIIII=I=I==II======.==....................................
# .....................................============I=II=IIIIIIIAIIIAI=I====II========.==..................................
# ....................................=====I=I===I=.=III=IIIIAAIIIIAI=I==IIII============.................................
# .....................................=====II==III==IIAIIIIIAII=AIAIII=IIII===II=======..................................
# ...................................==.======II=III==IAIIIIIAII=AAIII=II==I===II=I=====.==.=.............................
# ....................................========IIIIIII=IIAIIIAAIIIIAIII==I=II==IIIII==I===.===.............................
# .............................=....=========IIIIIIIIIIIAIIAAAIIIIAIII=IAIII=IIIIIIII=========..==........................
# ..............................===.======I===IIAAAIIIIAAAIAAAAIAIAIIIIIAIIIAIIAAI======.=======.==.......................
# ............................===I===========IIIIAAAIIIIAAAAAAAIAIAIAI=IIIIAAIII=I=I==============........................
# ..............................==I======III==IIIIIIAAI=AAIAAXAAAAIAAIIAAIAAIIIIII=I===IIII======....=....................
# ............................=..=IAI===IIIIIIIIIIIAAAAAIAIXIXAAXAIAXAAIIAAAIIAII=IIIIIII=======..====....................
# ...........................====..IAII=IIIII=IIIIAIAIAAAAAXAXAAXAIAAAIAAAAIIIAIIIIIIIIII=====.=.===......................
# ..................=.......========IIIIIIIIIAIIIIIIIAAXAAAXAAXXXAIXAIAXIAAAIAIIA==I=II=====I=====........................
# ..................==......=========IIAIIIAIIIIAAAIIIAAAAAXAXXXXAAXAAAAAAAIAIIII==III=======.=========...................
# ...................=I==..=====I======IIIIIAAIAIIIAAAAAXXAXXXXAXAAXAXXIAAIAIIIIIIAII==.==IIII==III===....................
# ....................=II===II===IIIIIIIAAAAIAAAAAIIIAXXXXXXXXXXXAAAAAAAXAAAAAAAAAIIIAAAAAIIIIII====......................
# ....................==IIIIIIIIIIIIAAAAAAXAAAAIIIAIIIAXXXMXXXMXAAXXIAXXAXAAAAIII=IIII======IIII=I=====...................
# ................==..====IIIAAAAIIIIAIAAAAAXAAXAIIIAAAIXAAMXMMMXXAAXXXXXXAAIIIIIAIIIIIIIIIIIIIII========.................
# ...............=====I====IIAAAAAIII=====IIAXXAXXAAIAAAAXIAMMXMXAAAXMMXAIIAAAI=====IAIIIIIAII========....................
# ...................=====IIIIIIAAAAAAAIIIIIIAAAIAAXMMXXXXXXMMXMMXXMMXAIAAAAAIII====IIAAAAIIII====.=.........=............
# ...................========IIIIIAXXAAAAIAIIIIIAAAIAXAAAXXXXMMMXXMMMAAAAAAAAAIAIIIIAAAIAIII==========..=====.............
# ................=======II=========IAXAAXXXXAAAAAXXAAXAAIMMMMMXXMMMAIAAAAIAAXXAAAAAAI==II=========..======...............
# ..........========.======IIIIIIIIIIIAAAXXXXXXXXAAXXXMXXXAXMMMMMMXXAAAXAAAAAXAAAAAIIIIIIIIIIIIIIIIIII===.................
# ............===IIIAIIIIIIIIAAAAAAXXXXXXXXXXXXXAXXAXAXXMXXMMMMMMXMXXXXAAAAAAAIIAIIAAAAAAAAAIIIIIII=====..................
# ................======IIIIIIIIAAAAAAAAAXAXAAAAXMMMXXAAMXIAMMMMMMMXMXXXXXXXXXXXAAAIIIIIIIIIIAIAII========................
# .................====II==I=IIIAIIIIAIAAXAAAAXXXXXXXMXXXMMAMMMMXIAMXAXXXXXXXXXXXAAAAAAAAAIAAIIIIII=I===..................
# ...............====================IIAAAAXXXXXXXAXXMMMMMXMMWMMMMXXXXXAAAAAAIAAXXXAAAAAII================................
# .....................==...======IIIAAAXXXXAAAXAXIAAIAXMMAAMWWMXAXXAXXXAXAAXXAXAXXXAIIIIIII====I=======.=................
# ..................=====IIIIIIAAAAAAAAAAAI=IAIAAA=IIAXXAXMXMMMXIAAAAXXXXXAXAIAAAXAAAAAAAAIIIIII=========.................
# .................=====IIIIIIAAAIIIII=I=IIIIAXAAIAAIAXXXAIMMMMXIAIXAXXXAAXXXXXAIIIAAAIAAIAIIIII=====I====................
# .................=======III=I======IIIIIIIAXAAAAIIAAIIIAAAMMMAAAAAIIIAXXXMXXXXAIII==IIIII==IIIII=======.................
# ................=================II==IIIAAAAAAAIIAI===IIIMMMMXAIIAIIIIIAAXXXXXXXAAII==IIII========.====.................
# ..................====..=============IIAAAAAIIIIIII===IIIXMMMAIIAIIAIII=IAAAAAXXAAAIAII==============...................
# ........................============IAAAIAII===I======I==AMMMAIII=IAI====IIIAAAAAAAAIIIIIII===========..................
# ......................======..======III===II============IIMMMIIIIIII=====I===IAAAAIAAI=I========........................
# ===================....===....==============I=============MMMI=================IIAAIIIIIIII========.....................
# ==========================.....================..=========MMMI===================IIIIII====I==.==.......................
# =============================.================....========MMMI=.......=================I===I====........................
# ================================================..========MMMA==......==.=.......===..===.===.==........................
# ==========================================================MMMA=================.====.====...=.......=========...........
# ==========================================================MMMX==========================================================
# =========================================================IMMMX==========================================================
# ========II==IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIAAAIIAIIXMMMMAIIIIAAAIIIIIIIIIIIIIII=IIIII=IIIIIIIIIIII================
# XXXXXXXXXXXXXXXXXXXXXXXXXXMXXMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMXMMM
# MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
# MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMWWMWMMMMMWWWWMMWWWWWWWWWWWWMWMWMMWMMWMMMMWWMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
# treepoet.com
