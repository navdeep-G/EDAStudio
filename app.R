suppressMessages (library(shiny))
suppressMessages (library(ggplot2))
suppressMessages (library(ggrepel))
suppressMessages (library(scales))
suppressMessages (library(DT))
suppressMessages (library(tidyr))
suppressMessages (library(dplyr))
suppressMessages (library(Hmisc))
suppressMessages (library(quantreg))


stat_sum_df <- function(fun, geom="point", ...) {
  stat_summary(fun.data=fun,  geom=geom,  ...)
}
stat_sum_single <- function(fun, geom="point", ...) {
  stat_summary(fun.y=fun,  geom=geom,  ...)
}

options(shiny.maxRequestSize=250*1024^2) 
tableau10 <- c("#1F77B4","#FF7F0E","#2CA02C","#D62728","#9467BD",
               "#8C564B","#E377C2","#7F7F7F","#BCBD22","#17BECF")

tableau20 <- c("#1F77B4","#AEC7E8", "#FF7F0E","#FFBB78"  ,"#2CA02C",
               "#98DF8A" ,"#D62728","#FF9896" ,"#9467BD","#C5B0D5" ,
               "#8C564B","#C49C94" ,"#E377C2","#F7B6D2" ,"#7F7F7F",
               "#C7C7C7" ,"#BCBD22","#DBDB8D" ,"#17BECF","#9EDAE5")

ui  <-  fluidPage(
  titlePanel("Explore Data"),
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        tabPanel("Inputs", 
                 fileInput("datafile", "Choose csv file to upload",
                           multiple = FALSE, accept = c("csv")),
                 uiOutput("ycol"),uiOutput("xcol"),
                 tabsetPanel(id = "filtercategorize",
                             tabPanel("Categorize/Rename", 
                                      uiOutput("catvar"),
                                      uiOutput("ncuts"),
                                      uiOutput("catvar2"),
                                      uiOutput("catvar3"),
                                      uiOutput("ncuts2"),
                                      uiOutput("asnumeric"),
                                      textOutput("bintext"),
                                      uiOutput("catvar4"),
                                      textOutput("labeltext"),
                                      uiOutput("nlabels")
                             ),
                             
                             tabPanel("Combine Variables",
                                      h6("Combined variables can be used for colour, fill, group, size and facets. They cannot be used as X or Y variables."),
                                      
                                      uiOutput("pastevar")
                             ),
                             tabPanel("Filters", 
                                      uiOutput("maxlevels"),
                                      uiOutput("filtervar1"),
                                      uiOutput("filtervar1values"),
                                      uiOutput("filtervar2"),
                                      uiOutput("filtervar2values"),
                                      uiOutput("filtervar3"),
                                      uiOutput("filtervar3values"),
                                      uiOutput("filtervarcont1"),
                                      uiOutput("fslider1"),
                                      uiOutput("filtervarcont2"),
                                      uiOutput("fslider2"),
                                      uiOutput("filtervarcont3"),
                                      uiOutput("fslider3")
                             ),
                             tabPanel("Simple Rounding",
                                      uiOutput("roundvar"),
                                      numericInput("rounddigits",label = "N Digits",value = 0,min=0,max=10) 
                             ),
                             tabPanel("Reorder Variables", 
                                      uiOutput("reordervar"),
                                      conditionalPanel(condition = "input.reordervarin!='' " ,
                                                       selectizeInput(  "functionordervariable", 'The:',
                                                                        choices =c("Median","Mean","Minimum","Maximum") ,multiple=FALSE)
                                      ),
                                      uiOutput("variabletoorderby"),
                                      conditionalPanel(condition = "input.reordervarin!='' " ,
                                                       checkboxInput('reverseorder', 'Reverse Order ', value = FALSE) ),
                                      
                                      uiOutput("reordervar2"),
                                      uiOutput("reordervar2values"),
                                      uiOutput("catvar5"),
                                      textOutput("labeltext5"),
                                      uiOutput("nlabels5")
                             )
                 ),
                 hr()
        ), # tabsetPanel
        
        
        tabPanel("Graph Options",
                 tabsetPanel(id = "graphicaloptions",
                             tabPanel(  "X/Y Log /Labels",
                                        hr(),
                                        textInput('ylab', 'Y axis label', value = "") ,
                                        textInput('xlab', 'X axis label', value = "") ,
                                        hr(),
                                        checkboxInput('logy', 'Log Y axis', value = FALSE) ,
                                        checkboxInput('logx', 'Log X axis', value = FALSE) ,
                                        conditionalPanel(condition = "!input.logy" ,
                                                         checkboxInput('scientificy', 'Comma separated Y axis ticks', value = FALSE)),
                                        conditionalPanel(condition = "!input.logx" ,
                                                         checkboxInput('scientificx', 'Comma separated X axis ticks', value = FALSE)),
                                        checkboxInput('rotateyticks', 'Rotate/Justify Y axis Ticks ', value = FALSE),
                                        checkboxInput('rotatexticks', 'Rotate/Justify X axis Ticks ', value = FALSE),
                                        conditionalPanel(condition = "input.rotateyticks" , 
                                                         sliderInput("yticksrotateangle", "Y axis ticks angle:", min=0, max=360, value=c(0),step=10),
                                                         sliderInput("ytickshjust", "Y axis ticks horizontal justification:", min=0, max=1, value=c(0.5),step=0.1),
                                                         sliderInput("yticksvjust", "Y axis ticks vertical justification:", min=0, max=1, value=c(0.5),step=0.1)
                                        ),
                                        conditionalPanel(condition = "input.rotatexticks" , 
                                                         sliderInput("xticksrotateangle", "X axis ticks angle:", min=0, max=360, value=c(20),step=10),
                                                         sliderInput("xtickshjust", "X axis ticks horizontal justification:", min=0, max=1, value=c(1),step=0.1),
                                                         sliderInput("xticksvjust", "X axis ticks vertical justification:", min=0, max=1, value=c(1),step=0.1)
                                        )
                                        
                             ),
                             tabPanel(  "Graph Size/Zoom",
                                        sliderInput("height", "Plot Height", min=1080/4, max=1080, value=480, animate = FALSE),
                                        h6("X Axis Zoom only works if facet x scales are not set to be free. The slider has limits between your x variable min/max otherwise select manual x values zoom to input your own."),
                                        uiOutput("xaxiszoom"),
                                        checkboxInput('userxzoom', 'Manual x values zoom', value = FALSE),
                                        conditionalPanel(condition = "input.userxzoom" ,uiOutput("lowerx"),uiOutput("upperx")),
                                        h6("Y Axis Zoom only works if you have one y variable and facet y scales are not set to be free. The slider has limits between your y variable min/max otherwise select manual yvalues zoom to input your own."),
                                        uiOutput("yaxiszoom"),
                                        checkboxInput('useryzoom', 'Manual y values zoom', value = FALSE),
                                        conditionalPanel(condition = "input.useryzoom" ,uiOutput("lowery"),uiOutput("uppery"))
                                        
                             ),
                             
                             tabPanel(  "Background Color and Legend(s)",
                                        selectInput('backgroundcol', label ='Background Color',
                                                    choices=c("Gray" ="gray97","White"="white","Dark Gray"="grey90"),
                                                    multiple=FALSE, selectize=TRUE,selected="white"),
                                        selectInput('legendposition', label ='Legend Position',
                                                    choices=c("left", "right", "bottom", "top","none"),
                                                    multiple=FALSE, selectize=TRUE,selected="bottom"),
                                        selectInput('legenddirection', label ='Layout of Items in Legends',
                                                    choices=c("horizontal", "vertical"),
                                                    multiple=FALSE, selectize=TRUE,selected="horizontal"),
                                        selectInput('legendbox', label ='Arrangement of Multiple Legends ',
                                                    choices=c("horizontal", "vertical"),
                                                    multiple=FALSE, selectize=TRUE,selected="vertical"),
                                        checkboxInput('sepguides', 'Separate Legend Items for Median/PI ',value = TRUE),       
                                        checkboxInput('labelguides', 'Hide the Names of the Legend Items ',value = FALSE),
                                        checkboxInput('customlegendtitle', 'Customization of Legend Titles, number of columns of items and reversing the legend items ',value = FALSE),
                                        conditionalPanel(condition = "input.customlegendtitle",
                                                         textInput("customcolourtitle", label ="Colour Legend Title",value="colour"),
                                                         numericInput("legendncolcol",label = "Colour Legend N columns",value =1,min=1,max =10) ,
                                                         checkboxInput('legendrevcol', 'Reverse Colour Legend ',value = FALSE),
                                                         checkboxInput('legendalphacol', 'Override Colour Transparency ',value = TRUE),
                                                         textInput("customfilltitle", label ="Fill Legend Title",value="fill"),
                                                         numericInput("legendncolfill",label = "Fill Legend N columns",value =1,min=1,max =10) ,
                                                         checkboxInput('legendrevfill', 'Reverse Fill Legend',value = FALSE),
                                                         checkboxInput('legendalphafill', 'Override Fill Transparency ',value = FALSE),
                                                         textInput("customsizetitle", label ="Size Legend Title",value="size"),
                                                         numericInput("legendncolsize",label = "Size Legend N columns",value =1,min=1,max =10) ,
                                                         checkboxInput('legendrevsize','Reverse Size Legend ',value = FALSE),
                                                         
                                                         selectizeInput('legendordering',
                                                                        label = paste("Drag/Drop to reorder","Colour, Fill, Size Legends"),
                                                                        choices = c("colour","fill","size"),
                                                                        selected = c("colour","fill","size"),
                                                                        multiple=TRUE,  options = list(
                                                                          plugins = list('drag_drop')
                                                                        )
                                                         ) 
                                        )
                             ),
                             tabPanel(  "Facets Options",
                                        
                                        uiOutput("facetscales"),
                                        selectInput('facetspace' ,'Facet Spaces:',c("fixed","free_x","free_y","free")),
                                        
                                        
                                        selectInput('facetordering' ,'Facet Ordering:',c(
                                          "Top to Bottom, Left to Right Ordering like a Table" ="table",
                                          "Bottom to Top, Left to Right Ordering like a Plot" ="plot"),
                                          selected="table"),
                                        
                                        
                                        conditionalPanel(condition = "!input.facetwrap" ,
                                                         selectizeInput(  "facetswitch", "Facet Switch to Near Axis:",
                                                                          choices = c("x","y","both"),
                                                                          options = list(  maxItems = 1 ,
                                                                                           placeholder = 'Please select an option',
                                                                                           onInitialize = I('function() { this.setValue(""); }')  )  ),
                                                         checkboxInput('facetmargin', 'Show a facet with all data (margins)'),
                                                         selectInput('facetlabeller' ,'Facet Label:',c(
                                                           "Variable(s) Name(s) and Value(s)" ="label_both",
                                                           "Value(s)"="label_value",
                                                           "Parsed Expression" ="label_parsed"),
                                                           selected="label_both")),
                                        checkboxInput('facetwrap', 'Use facet_wrap'),
                                        conditionalPanel(condition = "input.facetwrap" ,
                                                         checkboxInput('facetwrapmultiline', 'facet_wrap strip labels on multiple lines',value=FALSE) ),
                                        conditionalPanel(condition = "input.facetwrap" ,
                                                         checkboxInput('customncolnrow', 'Control N columns an N rows')),
                                        conditionalPanel(condition = "input.customncolnrow" ,
                                                         h6("An error (nrow*ncol >= n is not TRUE) will show up if the total number of facets/panels
                                                            is greater than the product of the specified  N columns x N rows. Increase the N columns and/or N rows to avoid the error.
                                                            The default empty values will use ggplot automatic algorithm."),        
                                                         numericInput("wrapncol",label = "N columns",value =NA,min=1,max =10) ,
                                                         numericInput("wrapnrow",label = "N rows",value = NA,min=1,max=10) 
                                                         )
                                        
                                        ) ,
                             
                             tabPanel(  "Reference Lines",
                                        checkboxInput('identityline', 'Identity Line')    ,   
                                        checkboxInput('horizontalzero', 'Horizontol Zero Line'),
                                        checkboxInput('customline1', 'Vertical Line'),
                                        conditionalPanel(condition = "input.customline1" , 
                                                         numericInput("vline",label = "",value = 1) ),
                                        checkboxInput('customline2', 'Horizontal Line'),
                                        conditionalPanel(condition = "input.customline2" , 
                                                         numericInput("hline",label = "",value = 1) )
                             ),
                             tabPanel(  "Additional Themes Options",
                                        sliderInput("themebasesize", "Theme Size (affects all text elements in the plot):", min=1, max=100, value=c(16),step=1),
                                        checkboxInput('themetableau', 'Use Tableau Colors and Fills  (maximum of 10 colours are provided)',value=TRUE),
                                        conditionalPanel(condition = "input.themetableau" ,
                                                         h6("If you have more than 10 color groups the plot will not work and you get /Error: Insufficient values in manual scale. ## needed but only 10 provided./  Uncheck Use Tableau Colors and Fills to use default ggplot2 colors.")),
                                        checkboxInput('themecolordrop', 'Keep All levels of Colors and Fills ',value=TRUE) , 
                                        
                                        checkboxInput('themebw', 'Use Black and White Theme ',value=TRUE), 
                                        checkboxInput('themeaspect', 'Use custom aspect ratio ')   ,  
                                        conditionalPanel(condition = "input.themeaspect" , 
                                                         numericInput("aspectratio",label = "Y/X ratio",
                                                                      value = 1,min=0.1,max=10,step=0.01)) 
                             ) #tabpanel
                 )#tabsetpanel
                 )
                 )
                 ), #sidebarPanel
    mainPanel(
      tabsetPanel(
        tabPanel("X/Y Plot"  , 
                 uiOutput('ui_plot'),
                 hr(),
                 uiOutput("clickheader"),
                 tableOutput("plot_clickedpoints"),
                 uiOutput("brushheader"),
                 tableOutput("plot_brushedpoints"),
                 
                 tabPanel("Types of Graphs",
                          tabsetPanel(id = "graphicaltypes",selected = "Color/Group/Split/Size/Fill Mappings ()",
                                      tabPanel(  "Plot types, Points, Lines",
                                                 
                                                 fluidRow(
                                                   
                                                   column (12, hr()),
                                                   column (3,
                                                           radioButtons("Points", "Points/Jitter:",
                                                                        c("Points" = "Points",
                                                                          "Jitter" = "Jitter",
                                                                          "None" = "None")),
                                                           conditionalPanel( " input.Points!= 'None' ",
                                                                             sliderInput("pointstransparency", "Points Transparency:", min=0, max=1, value=c(0.5),step=0.01),
                                                                             checkboxInput('pointignorecol', 'Ignore Mapped Color')
                                                           )),
                                                   column(3,
                                                          conditionalPanel( " input.Points!= 'None' ",
                                                                            sliderInput("pointsizes", "Points Size:", min=0, max=4, value=c(1),step=0.1),
                                                                            numericInput('pointtypes','Points Type:',16, min = 1, max = 25),
                                                                            conditionalPanel( " input.pointignorecol ",
                                                                                              selectInput('colpoint', label ='Points Color', choices=colors(),multiple=FALSE, selectize=TRUE, selected="black") 
                                                                            )
                                                          )
                                                   ),                  
                                                   column(3,
                                                          radioButtons("line", "Lines:",
                                                                       c("Lines" = "Lines",
                                                                         "None" = "None"),selected="None"),
                                                          conditionalPanel( " input.line== 'Lines' ",
                                                                            sliderInput("linestransparency", "Lines Transparency:", min=0, max=1, value=c(0.5),step=0.01),
                                                                            checkboxInput('lineignorecol', 'Ignore Mapped Color'),
                                                                            checkboxInput('lineignoresize', 'Ignore Mapped Size')
                                                          )
                                                          
                                                   ),
                                                   column(3,
                                                          conditionalPanel( " input.line== 'Lines' ",
                                                                            sliderInput("linesize", "Lines Size:", min=0, max=4, value=c(1),step=0.1),
                                                                            selectInput('linetypes','Lines Type:',c("solid","dotted")),
                                                                            conditionalPanel( " input.lineignorecol ",
                                                                                              selectInput('colline', label ='Lines Color', choices=colors(),multiple=FALSE, selectize=TRUE,selected="black") 
                                                                            )
                                                          )
                                                   ),
                                                   column (12, h6("Points and Lines Size will apply only if Size By: in the Color Group Split Size Fill Mappings are set to None"))
                                                   
                                                 )#fluidrow
                                      ), # tabpanel
                                      tabPanel(  "Color/Group/Split/Size/Fill Mappings",
                                                 fluidRow(
                                                   column (12, hr()),
                                                   column (3, uiOutput("colour"),uiOutput("group")),
                                                   column(3, uiOutput("facet_col"),uiOutput("facet_row")),
                                                   column (3, uiOutput("facet_col_extra"),uiOutput("facet_row_extra")),
                                                   column (3, uiOutput("pointsize"),uiOutput("fill")),
                                                   column (12, h6("Make sure not to choose a variable that is in the y variable(s) list otherwise you will get an error Variable not found. These variables are stacked and become yvars and yvalues.This ensures that colour/group/etc. are kept intact when you apply a new filter or recode a variable. When you combine variables all mappings will be updated so you can choose the newly formed variable and as such the previous state will be lost." ))
                                                   
                                                 )
                                      ),#tabpanel
                                      tabPanel(  "Boxplots",
                                                 fluidRow(
                                                   column (12, h6("Limited Boxplots support. Options are to be added as per users requests.")),
                                                   
                                                   column (4,
                                                           checkboxInput('boxplotaddition', 'Add a Boxplot  (makes sense if x variable is categorical and
                                                                         you Group By a sensible choice. By default the x variable is used for grouping)'),
                                                           checkboxInput('boxplotignoregroup', 'Ignore Mapped Group  (can me helpful to superpose a loess or median on top of the boxplot)',value = TRUE)
                                                   ),
                                                   column (4,
                                                           checkboxInput('boxplotvarwidh', "Boxes proportional to the square-roots of the number of observations " ),
                                                           checkboxInput('boxplotnotch', "Notched Boxes .
                                                                         Notches are used to compare groups; if the notches of two boxes do not overlap, this suggests that the medians are significantly different." ),
                                                           checkboxInput('boxplotshowlegend', "Show Legend ", value=TRUE)
                                                           ),
                                                   
                                                   column(4,
                                                          checkboxInput('boxplotignorecol', 'Ignore Mapped Color'),
                                                          conditionalPanel( " input.boxplotignorecol " ,
                                                                            selectInput('boxcolline', label ='Box Outlines Color', choices=colors(),multiple=FALSE, selectize=TRUE,selected="black")
                                                          )
                                                   )
                                                   
                                                 )#fluidrow 
                                                 
                                                   ),
                                      tabPanel(  "Histograms/Density",
                                                 fluidRow(
                                                   column (12, h6("A plot of the mapped x variable
                                                                  will be produced when no y variable(s) are selected.This is still very limited. Options are to be added as per users requests.")),
                                                   
                                                   column (3,
                                                           checkboxInput('histogramaddition', 'Add a Histogram ',value = FALSE),
                                                           checkboxInput('densityaddition', 'Add a Density Curve ',value = TRUE)
                                                   )
                                                   )
                                                 
                                      )
                                      
        )#tabsetPanel
        )#tabPanel
        
      ),#tabPanel1
      tabPanel("Download", 
               selectInput(
                 inputId = "downloadPlotType",
                 label   = h5("Select download file type"),
                 choices = list("PDF"  = "pdf","BMP"  = "bmp","JPEG" = "jpeg","PNG"  = "png")),
               
               # Allow the user to set the height and width of the plot download.
               h5(HTML("Set download image dimensions<br>(units are inches for PDF, pixels for all other formats)")),
               numericInput(
                 inputId = "downloadPlotHeight",label = "Height (inches)",value = 7,min = 1,max = 100),
               numericInput(
                 inputId = "downloadPlotWidth",label = "Width (inches)",value = 7,min = 1,max = 100),
               # Choose download filename.
               textInput(
                 inputId = "downloadPlotFileName",
                 label = h5("Enter file name for download")),
               
               # File downloads when this button is clicked.
               downloadButton(
                 outputId = "downloadPlot", 
                 label    = "Download Plot")
      ),
      
      tabPanel('Data',  dataTableOutput("mytablex") 
      )#tabPanel2
        )#tabsetPanel
      )#mainPanel
      )#sidebarLayout
    )#fluidPage
server <-  function(input, output, session) {
  filedata <- reactive({
    infile <- input$datafile
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    read.csv(infile$datapath,na.strings = c("NA","."))
    
    
  })
  
  
  
  myData <- reactive({
    df=filedata()
    if (is.null(df)) return(NULL)
  })
  
  
  output$ycol <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    selectizeInput("y", "y variable(s):",choices=items,selected = items[1],multiple=TRUE,
                   options = list(
                     plugins = list('remove_button')))
  })
  
  output$xcol <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    selectInput("x", "x variable:",items,selected=items[2])
    
  })
  
  outputOptions(output, "ycol", suspendWhenHidden=FALSE)
  outputOptions(output, "xcol", suspendWhenHidden=FALSE)
  
  output$catvar <- renderUI({
    df <-filedata()
    validate(       need(!is.null(df), ""))
    #if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    MODEDF <- sapply(df, function(x) is.numeric(x))
    NAMESTOKEEP2<- names(df)  [ MODEDF ]
    selectInput('catvarin',label = 'Recode into Binned Categories:',choices=NAMESTOKEEP2,multiple=TRUE)
  })
  
  
  output$ncuts <- renderUI({
    if (!is.null(input$catvarin)&length(input$catvarin ) <1)  return(NULL)
    sliderInput('ncutsin',label = 'N of Cut Breaks:', min=2, max=10, value=c(3),step=1)
  })
  
  output$catvar2 <- renderUI({
    df <-filedata()
    validate(       need(!is.null(df), ""))
    #if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    MODEDF <- sapply(df, function(x) is.numeric(x))
    NAMESTOKEEP2<- names(df)  [ MODEDF ]
    if (!is.null(input$catvarin)) {
      if (length(input$catvarin ) >=1) {
        NAMESTOKEEP2<-NAMESTOKEEP2 [ !is.element(NAMESTOKEEP2,input$catvarin) ]
      }  
    }
    selectInput('catvar2in',label = 'Treat as Categories:',choices=NAMESTOKEEP2,multiple=TRUE)
    
  })
  
  output$catvar3 <- renderUI({
    df <-filedata()
    validate(       need(!is.null(df), ""))
    # if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    MODEDF <- sapply(df, function(x) is.numeric(x))
    NAMESTOKEEP2<- names(df)  [ MODEDF ]
    if (!is.null(input$catvarin)&length(input$catvarin ) >=1) {
      NAMESTOKEEP2<-NAMESTOKEEP2 [ !is.element(NAMESTOKEEP2,input$catvarin) ]
    }
    if (!is.null(input$catvar2in)&length(input$catvar2in ) >=1) {
      NAMESTOKEEP2<-NAMESTOKEEP2 [ !is.element(NAMESTOKEEP2,input$catvar2in) ]
    }
    selectizeInput(  "catvar3in", 'Custom cuts of this variable, defaults to min, median, max before any applied filtering:',
                     choices =NAMESTOKEEP2 ,multiple=FALSE,
                     options = list(    placeholder = 'Please select a variable',
                                        onInitialize = I('function() { this.setValue(""); }')
                     )
    )
  })
  output$ncuts2 <- renderUI({
    df <-filedata()
    validate(       need(!is.null(df), ""))
    if (!is.null(input$catvar3in)&length(input$catvar3in ) <1)  return(NULL)
    if ( input$catvar3in!=""){
      textInput("xcutoffs", label =  paste(input$catvar3in,"Cuts"),
                value = as.character(paste(
                  min(df[,input$catvar3in] ,na.rm=T),
                  median(df[,input$catvar3in],na.rm=T),
                  max(df[,input$catvar3in],na.rm=T) ,sep=",")
                )
      )
    }
  })
  output$asnumeric <- renderUI({
    df <-filedata()
    validate(       need(!is.null(df), ""))
    #if (is.null(df)) return(NULL)
    if (!is.null(input$catvar3in)&length(input$catvar3in ) <1)  return(NULL)
    if ( input$catvar3in!=""){
      column(12,
             checkboxInput('asnumericin', 'Treat as Numeric (helpful to overlay a smooth/regression line on top of a boxplot or to convert a variable into 0/1 and overlay a logistic fit', value = FALSE)
             #,checkboxInput('useasxaxislabels', 'Use the Categories Names as x axis label (makes sense only if you really chose it as x axis variable)', value = FALSE), 
             #checkboxInput('useasyaxislabels', 'Use the Categories Names as y axis label (makes sense only if you really chose it as y axis variable)', value = FALSE) 
      )
    }
  })
  
  
  outputOptions(output, "catvar", suspendWhenHidden=FALSE)
  outputOptions(output, "ncuts", suspendWhenHidden=FALSE)
  outputOptions(output, "catvar2", suspendWhenHidden=FALSE)
  outputOptions(output, "catvar3", suspendWhenHidden=FALSE)
  outputOptions(output, "ncuts2", suspendWhenHidden=FALSE)
  outputOptions(output, "asnumeric", suspendWhenHidden=FALSE)
  
  
  
  
  recodedata1  <- reactive({
    df <- filedata() 
    validate(       need(!is.null(df), ""))
    # if (is.null(df)) return(NULL)
    if(!is.null(input$catvarin)&length(input$catvarin ) >=1) {
      for (i in 1:length(input$catvarin ) ) {
        varname<- input$catvarin[i]
        df[,varname] <- cut(df[,varname],input$ncutsin)
        df[,varname]   <- as.factor( df[,varname])
      }
    }
    df
  })
  
  
  recodedata2  <- reactive({
    df <- recodedata1()
    validate(       need(!is.null(df), ""))
    #if (is.null(df)) return(NULL)
    if(!is.null(input$catvar2in) ){
      if(length(input$catvar2in ) >=1) {
        for (i in 1:length(input$catvar2in ) ) {
          varname<- input$catvar2in[i]
          df[,varname]   <- as.factor( df[,varname])
        }
      }  
    }
    df
  })
  
  recodedata3  <- reactive({
    df <- recodedata2()
    validate(       need(!is.null(df), ""))
    #if (is.null(df)) return(NULL)
    if(input$catvar3in!="") {
      varname<- input$catvar3in
      xlimits <- input$xcutoffs 
      nxintervals <- length(as.numeric(unlist (strsplit(xlimits, ",")) )) -1
      df[,varname] <- cut( as.numeric ( as.character(  df[,varname])),
                           breaks=   as.numeric(unlist (strsplit(xlimits, ","))),include.lowest=TRUE)
      df[,"custombins"] <-   df[,varname] 
      
      if(input$asnumericin) {
        df[,varname] <- as.numeric(as.factor(df[,varname]) ) -1 
      }
    }
    df
  })
  output$bintext <- renderText({
    df <- recodedata3()
    validate(       need(!is.null(df), ""))
    #if (is.null(df)) return(NULL)
    bintextout <- ""
    if(input$catvar3in!="") {
      varname<- input$catvar3in
      if(!input$asnumericin){
        bintextout <- levels(df[,"custombins"] )
      }
      if(input$asnumericin){
        bintextout <- paste( sort(unique(as.numeric(as.factor(df[,varname]) ) -1))  ,levels(df[,"custombins"] ),sep="/") 
      }}
    bintextout   
  })   
  #  xaxislabels <-levels(cut( as.numeric ( as.character( dataedafilter$month_ss)), breaks=   as.numeric(unlist (strsplit(ageglimits, ",") )),include.lowest=TRUE))
  #+ scale_x_continuous(breaks=seq(0,length(xaxislabels)-1),labels=xaxislabels )   useasxaxislabels
  output$catvar4 <- renderUI({
    df <-recodedata3()
    validate(       need(!is.null(df), ""))
    #if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    MODEDF <- sapply(df, function(x) is.numeric(x))
    NAMESTOKEEP2<- names(df)  [! MODEDF ]
    selectizeInput(  "catvar4in", 'Change labels of this variable:',
                     choices =NAMESTOKEEP2 ,multiple=FALSE,
                     options = list(    placeholder = 'Please select a variable',
                                        onInitialize = I('function() { this.setValue(""); }')
                     )
    )
  })
  
  output$labeltext <- renderText({
    df <- recodedata3()
    validate(       need(!is.null(df), ""))
    #if (is.null(df)) return(NULL)
    labeltextout <- ""
    if(input$catvar4in!="") {
      varname<- input$catvar4in
      labeltextout <- c("Old labels",levels(df[,varname] ))
    }
    labeltextout   
  })   
  
  
  
  
  output$nlabels <- renderUI({
    df <-recodedata3()
    validate(       need(!is.null(df), ""))
    if (!is.null(input$catvar4in)&length(input$catvar4in ) <1)  return(NULL)
    if ( input$catvar4in!=""){
      nlevels <- length( unique( levels(as.factor( df[,input$catvar4in] ))))
      levelsvalues <- levels(as.factor( df[,input$catvar4in] ))
      textInput("customvarlabels", label =  paste(input$catvar4in,"requires",nlevels,"new labels,
                                                  type in a comma separated list below"),
                value = paste(as.character(levelsvalues),collapse=", ",sep="")
                
      )
    }
    
  })
  outputOptions(output, "labeltext", suspendWhenHidden=FALSE) 
  outputOptions(output, "catvar4", suspendWhenHidden=FALSE)
  outputOptions(output, "nlabels", suspendWhenHidden=FALSE)
  
  
  recodedata4  <- reactive({
    df <- recodedata3()
    validate(       need(!is.null(df), ""))
    #if (is.null(df)) return(NULL)
    if(input$catvar4in!="") {
      varname<- input$catvar4in
      xlabels <- input$customvarlabels 
      xlabels <- gsub("\\\\n", "\\\n", xlabels)
      nxxlabels <- length(as.numeric(unlist (strsplit(xlabels, ",")) )) -1
      df[,varname] <- as.factor(df[,varname])
      levels(df[,varname])  <-  unlist (strsplit(xlabels, ",") )
    }
    
    df
  })
  
  
  output$pastevar <- renderUI({
    df <- recodedata4()
    validate(       need(!is.null(df), ""))
    #if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    MODEDF <- sapply(df, function(x) is.numeric(x))
    yvariables <- input$y
    NAMESTOKEEP2<- names(df)  [! MODEDF ]
    NAMESTOKEEP2<- NAMESTOKEEP2[!NAMESTOKEEP2 %in% yvariables]
    selectizeInput("pastevarin", "Combine the categories of these two variables:", choices = NAMESTOKEEP2,multiple=TRUE,
                   options = list(
                     maxItems = 2 ,
                     placeholder = 'Please select two variables',
                     onInitialize = I('function() { this.setValue(""); }'),
                     plugins = list('remove_button', 'drag_drop')
                   )
    )
  })
  
  
  outputOptions(output, "pastevar", suspendWhenHidden=FALSE)
  outputOptions(output, "bintext", suspendWhenHidden=FALSE)
  
  output$maxlevels <- renderUI({
    df <-recodedata4()
    validate(       need(!is.null(df), ""))
    #if (is.null(df)) return(NULL)
    numericInput( inputId = "inmaxlevels",label = "Maximum number of unique values for Filter variable (1),(2),(3) (this is to avoid performance issues):",value = 500,min = 1,max = NA)
    
  })
  outputOptions(output, "maxlevels", suspendWhenHidden=FALSE)
  
  
  output$filtervar1 <- renderUI({
    df <-recodedata4()
    validate(       need(!is.null(df), ""))
    #if (is.null(df)) return(NULL)
    NUNIQUEDF <- sapply(df, function(x) length(unique(x)))
    NAMESTOKEEP<- names(df)  [ NUNIQUEDF  < input$inmaxlevels ]
    selectInput("infiltervar1" , "Filter variable (1):",c('None',NAMESTOKEEP ) )
  })
  
  output$filtervar2 <- renderUI({
    df <- recodedata4()
    validate(       need(!is.null(df), ""))
    #if (is.null(df)) return(NULL)
    NUNIQUEDF <- sapply(df, function(x) length(unique(x)))
    NAMESTOKEEP<- names(df)  [ NUNIQUEDF  < input$inmaxlevels ]
    #NAMESTOKEEP<-  NAMESTOKEEP[ NAMESTOKEEP!=input$infiltervar1 ]
    selectInput("infiltervar2" , "Filter variable (2):",c('None',NAMESTOKEEP ) )
  })
  
  output$filtervar3 <- renderUI({
    df <- recodedata4()
    validate(       need(!is.null(df), ""))
    #if (is.null(df)) return(NULL)
    NUNIQUEDF <- sapply(df, function(x) length(unique(x)))
    NAMESTOKEEP<- names(df)  [ NUNIQUEDF  < input$inmaxlevels ]
    #NAMESTOKEEP<-  NAMESTOKEEP[ NAMESTOKEEP!=input$infiltervar1 ]# allow nested filters
    #NAMESTOKEEP<-  NAMESTOKEEP[ NAMESTOKEEP!=input$infiltervar2 ]
    selectInput("infiltervar3" , "Filter variable (3):",c('None',NAMESTOKEEP ) )
  })
  
  
  output$filtervarcont1 <- renderUI({
    df <-recodedata4()
    validate(       need(!is.null(df), ""))
    #if (is.null(df)) return(NULL)
    NUNIQUEDF <- sapply(df, function(x) length(unique(x)))
    NAMESTOKEEP<- names(df)
    NAMESTOKEEP<- NAMESTOKEEP[ is.element ( NAMESTOKEEP,names(df[sapply(df,is.numeric)]))]
    selectInput("infiltervarcont1" , "Filter continuous (1):",c('None',NAMESTOKEEP ) )
  })
  output$filtervarcont2 <- renderUI({
    df <-recodedata4()
    validate(       need(!is.null(df), ""))
    #if (is.null(df)) return(NULL)
    NUNIQUEDF <- sapply(df, function(x) length(unique(x)))
    NAMESTOKEEP<- names(df)  
    NAMESTOKEEP<- NAMESTOKEEP[ is.element ( NAMESTOKEEP,names(df[sapply(df,is.numeric)]))]
    selectInput("infiltervarcont2" , "Filter continuous (2):",c('None',NAMESTOKEEP ) )
  })
  output$filtervarcont3 <- renderUI({
    df <-recodedata4()
    validate(       need(!is.null(df), ""))
    #if (is.null(df)) return(NULL)
    NUNIQUEDF <- sapply(df, function(x) length(unique(x)))
    NAMESTOKEEP<- names(df)  
    NAMESTOKEEP<- NAMESTOKEEP[ is.element ( NAMESTOKEEP,names(df[sapply(df,is.numeric)]))]
    selectInput("infiltervarcont3" , "Filter continuous (3):",c('None',NAMESTOKEEP ) )
  })
  output$filtervar1values <- renderUI({
    df <-recodedata4()
    validate(       need(!is.null(df), ""))
    #if (is.null(df)) return(NULL)
    if(is.null(input$infiltervar1)|input$infiltervar1=="None") {return(NULL)}
    if(!is.null(input$infiltervar1)&input$infiltervar1!="None" )  {
      choices <- levels(as.factor(df[,input$infiltervar1]))
      selectInput('infiltervar1valuesnotnull',
                  label = paste("Select values", input$infiltervar1),
                  choices = c(choices),
                  selected = choices,
                  multiple=TRUE, selectize=FALSE)   
    }
  }) 
  
  filterdata  <- reactive({
    df <-   recodedata4()
    validate(       need(!is.null(df), ""))
    #if (is.null(df)) return(NULL)
    if(is.null(input$infiltervar1)) {
      df <-  df 
    }
    if(!is.null(input$infiltervar1)&input$infiltervar1!="None") {
      
      df <-  df [ is.element(df[,input$infiltervar1],input$infiltervar1valuesnotnull),]
    }
    
    df
  })
  
  output$filtervar2values <- renderUI({
    df <- filterdata()
    validate(       need(!is.null(df), ""))
    #if (is.null(df)) return(NULL)
    if(input$infiltervar2=="None") {
      selectInput('infiltervar2valuesnull',
                  label ='No filter variable 2 specified', 
                  choices = list(""),multiple=TRUE, selectize=FALSE)   
    }
    if(input$infiltervar2!="None"&!is.null(input$infiltervar2) )  {
      choices <- levels(as.factor(as.character(df[,input$infiltervar2])))
      selectizeInput('infiltervar2valuesnotnull',
                     label = paste("Select values", input$infiltervar2),
                     choices = c(choices),
                     selected = choices,
                     multiple=TRUE,
                     options = list(
                       plugins = list('remove_button')
                     ))   
    }
  })
  
  filterdata2  <- reactive({
    df <- filterdata()
    validate(       need(!is.null(df), ""))
    #if (is.null(df)) return(NULL)
    if(!is.null(input$infiltervar2)&input$infiltervar2!="None") {
      df <-  df [ is.element(df[,input$infiltervar2],input$infiltervar2valuesnotnull),]
    }
    if(input$infiltervar2=="None") {
      df 
    }
    df
  }) 
  output$filtervar3values <- renderUI({
    df <- filterdata2()
    validate(       need(!is.null(df), ""))
    #if (is.null(df)) return(NULL)
    if(!is.null(input$infiltervar3)&input$infiltervar3=="None") {
      selectInput('infiltervar3valuesnull',
                  label ='No filter variable 2 specified', 
                  choices = list(""),multiple=TRUE, selectize=FALSE)   
    }
    if(input$infiltervar3!="None"&!is.null(input$infiltervar3) )  {
      choices <- levels(as.factor(as.character(df[,input$infiltervar3])))
      selectizeInput('infiltervar3valuesnotnull',
                     label = paste("Select values", input$infiltervar3),
                     choices = c(choices),
                     selected = choices,
                     multiple=TRUE,
                     options = list(
                       plugins = list('remove_button'))
      )   
    }
  })
  
  filterdata3  <- reactive({
    df <- filterdata2()
    validate(       need(!is.null(df), ""))
    #if (is.null(df)) return(NULL)
    if(!is.null(input$infiltervar3)&input$infiltervar3!="None") {
      df <-  df [ is.element(df[,input$infiltervar3],input$infiltervar3valuesnotnull),]
    }
    if(input$infiltervar3=="None") {
      df 
    }
    df
  })  
  
  output$fslider1 <- renderUI({ 
    df <-  filterdata3()
    validate(       need(!is.null(df), ""))
    #if (is.null(df)) return(NULL)
    xvariable<- input$infiltervarcont1
    if(input$infiltervarcont1=="None" ){
      return(NULL)  
    }
    if (!is.numeric(df[,xvariable]) ) return(NULL)
    if(input$infiltervarcont1!="None" ){
      sliderInput("infSlider1", paste("Select",xvariable,"Range"),
                  min=min(df[,xvariable],na.rm=T),
                  max=max(df[,xvariable],na.rm=T),
                  value=c(min(df[,xvariable],na.rm=T),max(df[,xvariable],na.rm=T)) 
      )
    }             
  })
  filterdata4  <- reactive({
    df <- filterdata3()
    validate(       need(!is.null(df), ""))
    #if (is.null(df)) return(NULL)
    if(input$infiltervarcont1!="None" ){
      if(is.numeric( input$infSlider1[1]) & is.numeric(df[,input$infiltervarcont1])) {
        df <- df [!is.na(df[,input$infiltervarcont1]),]
        df <-  df [df[,input$infiltervarcont1] >= input$infSlider1[1]&df[,input$infiltervarcont1] <= input$infSlider1[2],]
      }
    }
    
    df
  })
  output$fslider2 <- renderUI({ 
    df <-  filterdata4()
    validate(       need(!is.null(df), ""))
    #if (is.null(df)) return(NULL)
    xvariable<- input$infiltervarcont2
    if(input$infiltervarcont2=="None" ){
      return(NULL)  
    }
    if (!is.numeric(df[,xvariable]) ) return(NULL)
    if(input$infiltervarcont2!="None" ){
      sliderInput("infSlider2", paste("Select",xvariable,"Range"),
                  min=min(df[,xvariable],na.rm=T),
                  max=max(df[,xvariable],na.rm=T),
                  value=c(min(df[,xvariable],na.rm=T),max(df[,xvariable],na.rm=T)) 
      )
    }             
  })
  
  
  filterdata5  <- reactive({
    df <- filterdata4()
    validate(       need(!is.null(df), ""))
    #if (is.null(df)) return(NULL)
    if(input$infiltervarcont2!="None" ){
      if(is.numeric( input$infSlider2[1]) & is.numeric(df[,input$infiltervarcont2])) {
        df<- df [!is.na(df[,input$infiltervarcont2]),]
        df<-df [df[,input$infiltervarcont2] >= input$infSlider2[1]&df[,input$infiltervarcont2] <= input$infSlider2[2],]
      }
    }
    
    df
  })
  
  output$fslider3 <- renderUI({ 
    df <-  filterdata5()
    validate(       need(!is.null(df), ""))
    #if (is.null(df)) return(NULL)
    xvariable<- input$infiltervarcont3
    if(input$infiltervarcont3=="None" ){
      return(NULL)  
    }
    if (!is.numeric(df[,xvariable]) ) return(NULL)
    if(input$infiltervarcont3!="None" ){
      sliderInput("infSlider3", paste("Select",xvariable,"Range"),
                  min=min(df[,xvariable],na.rm=T),
                  max=max(df[,xvariable],na.rm=T),
                  value=c(min(df[,xvariable],na.rm=T),max(df[,xvariable],na.rm=T)) 
      )
    }             
  })
  
  
  filterdata6  <- reactive({
    df <- filterdata5()
    validate(       need(!is.null(df), ""))
    #if (is.null(df)) return(NULL)
    if(input$infiltervarcont3!="None" ){
      if(is.numeric( input$infSlider3[1]) & is.numeric(df[,input$infiltervarcont3])) {
        df<- df [!is.na(df[,input$infiltervarcont3]),]
        df<-df [df[,input$infiltervarcont3] >= input$infSlider3[1]&df[,input$infiltervarcont3] <= input$infSlider3[2],]
      }
    }
    
    df
  })
  
  outputOptions(output, "filtervar1", suspendWhenHidden=FALSE)
  outputOptions(output, "filtervar2", suspendWhenHidden=FALSE)
  outputOptions(output, "filtervar3", suspendWhenHidden=FALSE)
  
  outputOptions(output, "filtervarcont1", suspendWhenHidden=FALSE)
  outputOptions(output, "filtervarcont2", suspendWhenHidden=FALSE)
  outputOptions(output, "filtervarcont3", suspendWhenHidden=FALSE)
  outputOptions(output, "filtervar1values", suspendWhenHidden=FALSE)
  outputOptions(output, "filtervar2values", suspendWhenHidden=FALSE)
  outputOptions(output, "filtervar3values", suspendWhenHidden=FALSE)
  
  outputOptions(output, "fslider1", suspendWhenHidden=FALSE)
  outputOptions(output, "fslider2", suspendWhenHidden=FALSE)
  outputOptions(output, "fslider3", suspendWhenHidden=FALSE)
  
  
  
  
  
  stackdata <- reactive({
    
    df <- filterdata6() 
    validate(       need(!is.null(df), ""))
    #if (is.null(df)) return(NULL)
    if (!is.null(df)){
      validate(  need(!is.element(input$x,input$y) ,
                      "Please select a different x variable or remove the x variable from the list of y variable(s)"))
      
      if(!is.null(input$y) ){
        
        if(       all( sapply(df[,as.vector(input$y)], is.numeric)) )
        {
          tidydata <- df %>%
            gather_( "yvars", "yvalues", gather_cols=as.vector(input$y) ) #%>%
          # mutate(combinedvariable="Choose two variables to combine first")
        }
        if(       any( sapply(df[,as.vector(input$y)], is.factor)) |
                  any( sapply(df[,as.vector(input$y)], is.character)))
        {
          tidydata <- df %>%
            gather_( "yvars", "yvalues", gather_cols=as.vector(input$y) ) %>%
            mutate(yvalues=as.factor(as.character(yvalues) ))#%>%
          # mutate(combinedvariable="Choose two variables to combine first")
        } 
        
        if(       all( sapply(df[,as.vector(input$y)], is.factor)) |
                  all( sapply(df[,as.vector(input$y)], is.character)))
        {
          tidydata <- df %>%
            gather_( "yvars", "yvalues", gather_cols=as.vector(input$y) ) %>%
            mutate(yvalues=as.factor(as.character(yvalues) ))#%>%
          # mutate(combinedvariable="Choose two variables to combine first")
        }    
        
        
      }
      if(is.null(input$y) ){
        tidydata <- df
        tidydata$yvars <- "None"
        tidydata$yvalues <- NA
        
      }
      
      if( !is.null(input$pastevarin)   ) {
        if (length(input$pastevarin) > 1) {
          tidydata <- tidydata %>%
            #unite_("combinedvariable" , c(input$pastevarin[1], input$pastevarin[2] ),remove=FALSE)
            unite_(paste(as.character(input$pastevarin),collapse="_",sep="") ,
                   c(input$pastevarin[1], input$pastevarin[2] ),remove=FALSE)
          
        }
      }
      
      tidydata
    }
  })
  
  
  output$roundvar <- renderUI({
    df <- stackdata()
    validate(       need(!is.null(df), ""))
    #if (is.null(df)) return(NULL)
    if (!is.null(df)){
      items=names(df)
      names(items)=items
      MODEDF <- sapply(df, function(x) is.numeric(x))
      NAMESTOKEEP2<- names(df)  [MODEDF]
      selectizeInput(  "roundvarin", "Round the Values to the Specified N Digits:", choices = NAMESTOKEEP2,multiple=TRUE,
                       options = list(
                         placeholder = 'Please select some variables',
                         onInitialize = I('function() { this.setValue(""); }')
                       )
      )
    }
  }) 
  
  rounddata <- reactive({
    df <- stackdata()
    validate(       need(!is.null(df), ""))
    #if (is.null(df)) return(NULL)
    if(!is.null(input$roundvarin)&length(input$roundvarin ) >=1) {
      for (i in 1:length(input$roundvarin ) ) {
        varname<- input$roundvarin[i]
        df[,varname]   <- round( df[,varname],input$rounddigits)
      }
    }
    df
  })  
  
  
  output$reordervar <- renderUI({
    df <- rounddata()
    validate(       need(!is.null(df), ""))
    #if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    MODEDF <- sapply(df, function(x) is.numeric(x))
    NAMESTOKEEP2<- names(df)  [ !MODEDF ]
    selectizeInput(  "reordervarin", 'Reorder This Variable By:',
                     choices =NAMESTOKEEP2 ,multiple=FALSE,
                     options = list(    placeholder = 'Please select a variable',
                                        onInitialize = I('function() { this.setValue(""); }')
                     )
    )
  })
  
  
  
  output$variabletoorderby <- renderUI({
    df <-rounddata()
    validate(       need(!is.null(df), ""))
    #if (is.null(df)) return(NULL)
    if (!is.null(input$reordervarin)&length(input$reordervarin ) <1)  return(NULL)
    if ( input$reordervarin!=""){
      yinputs <- input$y
      items=names(df)
      names(items)=items
      MODEDF <- sapply(df, function(x) is.numeric(x))
      NAMESTOKEEP2<- names(df)  [ MODEDF ]
      selectInput('varreorderin',label = 'Of this Variable:', choices=NAMESTOKEEP2,multiple=FALSE,selected="yvalues")
    }
  })
  
  
  
  outputOptions(output, "roundvar", suspendWhenHidden=FALSE)
  outputOptions(output, "reordervar", suspendWhenHidden=FALSE)
  outputOptions(output, "variabletoorderby", suspendWhenHidden=FALSE)
  
  
  
  reorderdata <- reactive({
    df <- rounddata()
    validate(       need(!is.null(df), ""))
    #if (is.null(df)) return(NULL)
    if(!is.null(input$reordervarin)&length(input$reordervarin ) >=1 &
       length(input$varreorderin ) >=1 & input$reordervarin!=""  ) {
      varname<- input$reordervarin[1]
      if(input$functionordervariable=="Median" )  {
        df[,varname]   <- reorder( df[,varname],df[,input$varreorderin], FUN=function(x) median(x[!is.na(x)]))
      }
      if(input$functionordervariable=="Mean" )  {
        df[,varname]   <- reorder( df[,varname],df[,input$varreorderin],  FUN=function(x) mean(x[!is.na(x)]))
      }
      if(input$functionordervariable=="Minimum" )  {
        df[,varname]   <- reorder( df[,varname],df[,input$varreorderin],  FUN=function(x) min(x[!is.na(x)]))
      }
      if(input$functionordervariable=="Maximum" )  {
        df[,varname]   <- reorder( df[,varname],df[,input$varreorderin],  FUN=function(x) max(x[!is.na(x)]))
      }
      if(input$reverseorder )  {
        df[,varname] <- factor( df[,varname], levels=rev(levels( df[,varname])))
        
      }
    }
    df
  })  
  output$reordervar2 <- renderUI({
    df <- reorderdata()
    validate(       need(!is.null(df), ""))
    #if (is.null(df)) return(NULL)
    MODEDF <- sapply(df, function(x) is.numeric(x))
    NAMESTOKEEP<- names(df)  [ !MODEDF ]
    if(!is.null(input$reordervarin)&length(input$reordervarin ) >=1  ){
      NAMESTOKEEP<- NAMESTOKEEP  [ NAMESTOKEEP!=input$reordervarin ]
      
    }
    selectInput("reordervar2in" , "Custom Reorder this variable:",c('None',NAMESTOKEEP ) )
  })
  
  output$reordervar2values <- renderUI({
    df <- reorderdata()
    validate(       need(!is.null(df), ""))
    # if (is.null(df)) return(NULL)
    if(input$reordervar2in=="None") {
      selectInput('reordervar2valuesnull',
                  label ='No reorder variable specified', 
                  choices = list(""),multiple=TRUE, selectize=FALSE)   
    }
    if(input$reordervar2in!="None"&!is.null(input$reordervar2in) )  {
      choices <- levels(as.factor(as.character(df[,input$reordervar2in])))
      selectizeInput('reordervar2valuesnotnull',
                     label = paste("Drag/Drop to reorder",input$reordervar2in, "values"),
                     choices = c(choices),
                     selected = choices,
                     multiple=TRUE,  options = list(
                       plugins = list('drag_drop')
                     )
      )   
    }
  })
  outputOptions(output, "reordervar2", suspendWhenHidden=FALSE)
  outputOptions(output, "reordervar2values", suspendWhenHidden=FALSE)
  
  reorderdata2 <- reactive({
    df <- reorderdata()
    validate(       need(!is.null(df), ""))
    #if (is.null(df)) return(NULL)
    if(input$reordervar2in!="None"  ) {
      df [,input$reordervar2in] <- factor(df [,input$reordervar2in],
                                          levels = input$reordervar2valuesnotnull)
      
    }
    df
  })
  
  output$xaxiszoom <- renderUI({
    df <-reorderdata2()
    validate(       need(!is.null(df), ""))
    
    if (!is.numeric(df[,input$x] ) ) return(NULL)
    if (is.numeric(df[,input$x]) &
        input$facetscalesin!="free_x"&
        input$facetscalesin!="free"){
      xvalues <- df[,input$x][!is.na( df[,input$x])]
      xmin <- min(xvalues)
      xmax <- max(xvalues)
      xstep <- (xmax -xmin)/100
      sliderInput('xaxiszoomin',label = 'Zoom to X variable range:', min=xmin, max=xmax, value=c(xmin,xmax),step=xstep)
      
    }
    
    
  })
  outputOptions(output, "xaxiszoom", suspendWhenHidden=FALSE)
  
  output$lowerx <- renderUI({
    df <-reorderdata2()
    if (is.null(df)| !is.numeric(df[,input$x] ) ) return(NULL)
    if (is.numeric(df[,input$x]) &
        input$facetscalesin!="free_x"&
        input$facetscalesin!="free"){
      xvalues <- df[,input$x][!is.na( df[,input$x])]
      xmin <- min(xvalues)
      numericInput("lowerxin",label = "Lower X Limit",value = xmin,min=NA,max=NA,width='50%')
    }
  })
  output$upperx <- renderUI({
    df <-reorderdata2()
    if (is.null(df)| !is.numeric(df[,input$x] ) ) return(NULL)
    if (is.numeric(df[,input$x]) &
        input$facetscalesin!="free_x"&
        input$facetscalesin!="free"){
      xvalues <- df[,input$x][!is.na( df[,input$x])]
      xmax <- max(xvalues)
      numericInput("upperxin",label = "Upper X Limit",value = xmax,min=NA,max=NA,width='50%')
    }
  }) 
  outputOptions(output, "lowerx", suspendWhenHidden=FALSE)
  outputOptions(output, "upperx", suspendWhenHidden=FALSE)
  
  output$yaxiszoom <- renderUI({
    df <-reorderdata2()
    if ( is.null(input$y)  ) return(NULL)
    if ( !is.null(input$y)  ){
      if (is.null(df)| !is.numeric(df[,"yvalues"] ) | (length(input$y) > 1 ) ) return(NULL)
      if (is.numeric(df[,"yvalues"]) &  (length(input$y) < 2 ) &
          input$facetscalesin!="free_y"&
          input$facetscalesin!="free"){
        yvalues <- df[,"yvalues"][!is.na( df[,"yvalues"])]
        ymin <- min(yvalues)
        ymax <- max(yvalues)
        ystep <- (ymax -ymin)/100
        sliderInput('yaxiszoomin',label = 'Zoom to Y variable range:', min=ymin, max=ymax, value=c(ymin,ymax),step=ystep)
        
      }
    }
    
    
    
    
  })
  outputOptions(output, "yaxiszoom", suspendWhenHidden=FALSE)  
  
  output$lowery <- renderUI({
    df <-reorderdata2()
    if (is.null(df)| !is.numeric(df[,"yvalues"] ) | (length(input$y) > 1 ) ) return(NULL)
    if (is.numeric(df[,"yvalues"]) &  (length(input$y) < 2 ) &
        input$facetscalesin!="free_y"&
        input$facetscalesin!="free"){
      yvalues <- df[,"yvalues"][!is.na( df[,"yvalues"])]
      ymin <- min(yvalues)
      numericInput("loweryin",label = "Lower Y Limit",value = ymin,min=NA,max=NA,width='50%')
    }
  })
  output$uppery <- renderUI({
    df <-reorderdata2()
    if (is.null(df)| !is.numeric(df[,"yvalues"] ) | (length(input$y) > 1 ) ) return(NULL)
    if (is.numeric(df[,"yvalues"]) &  (length(input$y) < 2 ) &
        input$facetscalesin!="free_y"&
        input$facetscalesin!="free"){
      yvalues <- df[,"yvalues"][!is.na( df[,"yvalues"])]
      ymax <- max(yvalues)
      numericInput("upperyin",label = "Upper Y Limit",value = ymax,min=NA,max=NA,width='50%')
    }
  }) 
  outputOptions(output, "lowery", suspendWhenHidden=FALSE)
  outputOptions(output, "uppery", suspendWhenHidden=FALSE)
  
  
  
  
  output$catvar5 <- renderUI({
    df <-reorderdata2()
    validate(       need(!is.null(df), ""))
    # if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    MODEDF <- sapply(df, function(x) is.numeric(x))
    NAMESTOKEEP2<- names(df)  [! MODEDF ]
    
    selectizeInput(  "catvar5in", 'Change labels of this variable:',
                     choices =NAMESTOKEEP2 ,multiple=FALSE,
                     options = list(    placeholder = 'Please select a variable',
                                        onInitialize = I('function() { this.setValue(""); }')
                     )
    )
  })
  
  output$labeltext5 <- renderText({
    df <- reorderdata2()
    validate(       need(!is.null(df), ""))
    #if (is.null(df)) return(NULL)
    labeltext5out <- ""
    if(input$catvar5in!="") {
      varname<- input$catvar5in
      labeltext5out <- c("Old labels",levels(as.factor(df[,varname]) ))
    }
    labeltext5out   
  })   
  
  output$nlabels5 <- renderUI({
    df <-reorderdata2()
    validate(       need(!is.null(df), ""))
    
    if (!is.null(input$catvar5in)&length(input$catvar5in ) <1)  return(NULL)
    if ( input$catvar5in!=""){
      nlevels <- length( unique( levels(as.factor( df[,input$catvar5in] ))))
      levelsvalues <- levels(as.factor( df[,input$catvar5in] ))
      textInput("customvarlabels5", label =  paste(input$catvar5in,"requires",nlevels,"new labels,
                                                   type in a comma separated list below"),
                value =   paste(as.character(levelsvalues),collapse=", ",sep="")
      )
    }
    
  })
  
  outputOptions(output, "catvar5", suspendWhenHidden=FALSE)
  outputOptions(output, "nlabels5", suspendWhenHidden=FALSE)
  outputOptions(output, "labeltext5", suspendWhenHidden=FALSE)
  
  recodedata5  <- reactive({
    df <- reorderdata2()
    validate(       need(!is.null(df), ""))
    if (is.null(df)) return(NULL)
    if(input$catvar5in!="") {
      varname<- input$catvar5in
      xlabels <- input$customvarlabels5 
      xlabels <- gsub("\\\\n", "\\\n", xlabels)
      nxxlabels <- length(as.numeric(unlist (strsplit(xlabels, ",")) )) -1
      df[,varname] <- as.factor(df[,varname])
      levels(df[,varname])  <-  unlist (strsplit(xlabels, ",") )
    }
    df
  })
  
  output$colour <- renderUI({
    df <-filedata()
    validate(       need(!is.null(df), ""))
    #if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    items= items
    items= c("None",items, "yvars","yvalues") 
    if (!is.null(input$pastevarin)&length(input$pastevarin) >1 ){
      nameofcombinedvariables<- paste(as.character(input$pastevarin),collapse="_",sep="") 
      items= c(items,nameofcombinedvariables)
    }
    selectInput("colorin", "Colour By:",items) 
    
  })
  
  
  output$group <- renderUI({
    df <-filedata()
    validate(       need(!is.null(df), ""))
    #if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    items= items 
    items= c("None",items, "yvars","yvalues") 
    if (!is.null(input$pastevarin)&length(input$pastevarin) >1 ){
      nameofcombinedvariables<- paste(as.character(input$pastevarin),collapse="_",sep="") 
      items= c(items,nameofcombinedvariables)
    }
    selectInput("groupin", "Group By:",items)
  })
  outputOptions(output, "colour", suspendWhenHidden=FALSE)
  outputOptions(output, "group", suspendWhenHidden=FALSE)
  
  
  output$facet_col <- renderUI({
    df <-filedata()
    validate(       need(!is.null(df), ""))
    # if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    items= items 
    items =c(None='.',items,"yvars", "yvalues")
    if (!is.null(input$pastevarin)&length(input$pastevarin) >1 ){
      nameofcombinedvariables<- paste(as.character(input$pastevarin),collapse="_",sep="") 
      items= c(items,nameofcombinedvariables)
    }
    selectInput("facetcolin", "Column Split:",items)
  })
  output$facet_row <- renderUI({
    df <-filedata()
    validate(       need(!is.null(df), ""))
    #if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    items= items 
    items =c(None='.',items,"yvars", "yvalues")
    if (!is.null(input$pastevarin)&length(input$pastevarin) >1 ){
      nameofcombinedvariables<- paste(as.character(input$pastevarin),collapse="_",sep="") 
      items= c(items,nameofcombinedvariables)
    }
    selectInput("facetrowin", "Row Split:", items)
  })
  
  output$facet_col_extra <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    items= items
    items =c(None='.',items,"yvars", "yvalues")
    if (!is.null(input$pastevarin)&length(input$pastevarin) >1 ){
      nameofcombinedvariables<- paste(as.character(input$pastevarin),collapse="_",sep="") 
      items= c(items,nameofcombinedvariables)
    }
    selectInput("facetcolextrain", "Extra Column Split:",items)
  })
  output$facet_row_extra <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    items= items
    
    if (length(input$y) < 2 ){
      items= c(None=".",items,"yvars", "yvalues")    
      if (!is.null(input$pastevarin)&length(input$pastevarin) >1 ){
        nameofcombinedvariables<- paste(as.character(input$pastevarin),collapse="_",sep="") 
        items= c(items,nameofcombinedvariables)    
      }
    }
    if (length(input$y) > 1 ){
      items= c("yvars",None=".",items, "yvalues")    
      if (!is.null(input$pastevarin)&length(input$pastevarin) >1 ){
        items= c(items,nameofcombinedvariables)    
      }
    }
    
    selectInput("facetrowextrain", "Extra Row Split:",items)
  })
  
  output$facetscales <- renderUI({
    if (!is.null(input$y)&length(input$y) > 1 ){
      items= c("free_y","fixed","free_x","free")    
    }
    if (!is.null(input$y)&length(input$y) < 2 ){
      items= c("fixed","free_x","free_y","free")   
    }
    selectInput('facetscalesin','Facet Scales:',items)
  })
  
  outputOptions(output, "facet_row_extra", suspendWhenHidden=FALSE)
  outputOptions(output, "facet_col_extra", suspendWhenHidden=FALSE)
  outputOptions(output, "facet_row", suspendWhenHidden=FALSE)
  outputOptions(output, "facet_col", suspendWhenHidden=FALSE)
  outputOptions(output, "facetscales", suspendWhenHidden=FALSE)
  
  output$pointsize <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    items= items 
    items= c("None",items, "yvars","yvalues") 
    if (!is.null(input$pastevarin)&length(input$pastevarin) >1 ){
      nameofcombinedvariables<- paste(as.character(input$pastevarin),collapse="_",sep="") 
      items= c(items,nameofcombinedvariables)
    }
    selectInput("pointsizein", "Size By:",items )
    
  })
  
  output$fill <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    items= items
    items= c("None",items, "yvars","yvalues") 
    if (!is.null(input$pastevarin)&length(input$pastevarin) >1 ){
      nameofcombinedvariables<- paste(as.character(input$pastevarin),collapse="_",sep="") 
      items= c(items,nameofcombinedvariables)
    }
    selectInput("fillin", "Fill By:"    ,items )
  })
  
  output$weight <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    items= items
    if (!is.null(input$pastevarin)&length(input$pastevarin) >1 ){
      nameofcombinedvariables<- paste(as.character(input$pastevarin),collapse="_",sep="") 
      items= c(items,nameofcombinedvariables)
    }
    selectInput("weightin", "Weight By:",items )
  })
  outputOptions(output, "pointsize", suspendWhenHidden=FALSE)
  outputOptions(output, "fill", suspendWhenHidden=FALSE)
  outputOptions(output, "weight", suspendWhenHidden=FALSE)
  
  
  output$mytablex = renderDataTable({
    df <- recodedata5() 
    validate(       need(!is.null(df), ""))
    
    datatable(df ,
              extensions = c('ColReorder','Buttons','FixedColumns'),
              options = list(dom = 'Bfrtip',
                             searchHighlight = TRUE,
                             pageLength=5 ,
                             lengthMenu = list(c(5, 10, 15, -1), c('5','10', '15', 'All')),
                             colReorder = list(realtime = TRUE),
                             buttons = 
                               list('colvis', 'pageLength','print','copy', list(
                                 extend = 'collection',
                                 buttons = list(
                                   list(extend='csv'  ,filename = 'plotdata'),
                                   list(extend='excel',filename = 'plotdata'),
                                   list(extend='pdf'  ,filename = 'plotdata')),
                                 text = 'Download'
                               )),
                             scrollX = TRUE,scrollY = 400,
                             fixedColumns = TRUE
              ), 
              filter = 'bottom',
              style = "bootstrap")
  })
  
  
  
  plotObject <- reactive({
    plotdata <- recodedata5()
    validate(need(!is.null(plotdata), "") )
    
    if(!is.null(plotdata)) {
      
      if (input$themetableau){
        scale_colour_discrete <- function(...) 
          scale_colour_manual(..., values = tableau10,drop=!input$themecolordrop)
        scale_fill_discrete <- function(...) 
          scale_fill_manual(..., values = tableau10,drop=!input$themecolordrop)
      }
      
      
      if (!is.null(input$y) ){
        
        p <- ggplot(plotdata, aes_string(x=input$x, y="yvalues")) 
        
        if (input$colorin != 'None')
          p <- p + aes_string(color=input$colorin)
        if (input$fillin != 'None')
          p <- p + aes_string(fill=input$fillin)
        if (input$pointsizein != 'None')
          p <- p  + aes_string(size=input$pointsizein)
        
        # if (input$groupin != 'None' & !is.factor(plotdata[,input$x]))
        if (input$groupin != 'None')
          p <- p + aes_string(group=input$groupin)
        if (input$groupin == 'None' & !is.numeric(plotdata[,input$x]) 
            & input$colorin == 'None')
          p <- p + aes(group=1)
        
        if (input$Points=="Points"&input$pointsizein == 'None'&!input$pointignorecol)
          p <- p + geom_point(,alpha=input$pointstransparency,shape=input$pointtypes,size=input$pointsizes)  
        if (input$Points=="Points"&input$pointsizein != 'None'&!input$pointignorecol)
          p <- p + geom_point(,alpha=input$pointstransparency,shape=input$pointtypes)
        
        if (input$Points=="Jitter"&input$pointsizein == 'None'&!input$pointignorecol)
          p <- p + geom_jitter(,alpha=input$pointstransparency,shape=input$pointtypes,size=input$pointsizes)
        if (input$Points=="Jitter"&input$pointsizein != 'None'&!input$pointignorecol)
          p <- p + geom_jitter(,alpha=input$pointstransparency,shape=input$pointtypes)
        
        
        if (input$Points=="Points"&input$pointsizein == 'None'&input$pointignorecol)
          p <- p + geom_point(,alpha=input$pointstransparency,shape=input$pointtypes,size=input$pointsizes,colour=input$colpoint)  
        if (input$Points=="Points"&input$pointsizein != 'None'&input$pointignorecol)
          p <- p + geom_point(,alpha=input$pointstransparency,shape=input$pointtypes,colour=input$colpoint)
        
        if (input$Points=="Jitter"&input$pointsizein == 'None'&input$pointignorecol)
          p <- p + geom_jitter(,alpha=input$pointstransparency,shape=input$pointtypes,size=input$pointsizes,colour=input$colpoint)
        if (input$Points=="Jitter"&input$pointsizein != 'None'&input$pointignorecol)
          p <- p + geom_jitter(,alpha=input$pointstransparency,shape=input$pointtypes,colour=input$colpoint)
        
        
        
        if (input$line=="Lines"&input$pointsizein == 'None'& !input$lineignorecol)
          p <- p + geom_line(,size=input$linesize,alpha=input$linestransparency,linetype=input$linetypes)
        if (input$line=="Lines"&input$pointsizein != 'None'& !input$lineignorecol& !input$lineignoresize)
          p <- p + geom_line(,alpha=input$linestransparency,linetype=input$linetypes)
        if (input$line=="Lines"&input$pointsizein != 'None'& !input$lineignorecol& input$lineignoresize)
          p <- p + geom_line(size=input$linesize,alpha=input$linestransparency,linetype=input$linetypes)
        
        if (input$line=="Lines"&input$pointsizein == 'None'&input$lineignorecol)
          p <- p + geom_line(,size=input$linesize,alpha=input$linestransparency,linetype=input$linetypes,colour=input$colline)
        if (input$line=="Lines"&input$pointsizein != 'None'& input$lineignorecol& !input$lineignoresize)
          p <- p + geom_line(,alpha=input$linestransparency,linetype=input$linetypes,colour=input$colline)
        if (input$line=="Lines"&input$pointsizein != 'None'& input$lineignorecol & input$lineignoresize )
          p <- p + geom_line(size=input$linesize,alpha=input$linestransparency,linetype=input$linetypes,colour=input$colline)
        
        #### Boxplot Section START
        
        if (input$boxplotaddition){
          if (input$groupin != 'None'& !input$boxplotignoregroup ){
            if (!input$boxplotignorecol){
              p <- p + aes_string(group=input$groupin)
              p <- p + geom_boxplot(varwidth = input$boxplotvarwidh,notch = input$boxplotnotch,show.legend=input$boxplotshowlegend)
              
            }
            if (input$boxplotignorecol){
              p <- p + aes_string(group=input$groupin)
              p <- p + geom_boxplot(varwidth = input$boxplotvarwidh,notch = input$boxplotnotch,show.legend=input$boxplotshowlegend,col=input$boxcolline)
              
            }
          }
          if (input$groupin == 'None'){
            if (!input$boxplotignorecol){
              p <- p + geom_boxplot(aes(group=NULL),varwidth = input$boxplotvarwidh,notch = input$boxplotnotch,show.legend=input$boxplotshowlegend)
            }
            if (input$boxplotignorecol){
              p <- p + geom_boxplot(aes(group=NULL),varwidth = input$boxplotvarwidh,notch = input$boxplotnotch,show.legend=input$boxplotshowlegend,col=input$boxcolline)
            } 
          }
          
          
          if (input$boxplotignoregroup ){
            if (!input$boxplotignorecol){
              p <- p + geom_boxplot(aes(group=NULL),varwidth = input$boxplotvarwidh,notch = input$boxplotnotch,show.legend=input$boxplotshowlegend)
            }
            if (input$boxplotignorecol){
              p <- p + geom_boxplot(aes(group=NULL),varwidth = input$boxplotvarwidh,notch = input$boxplotnotch,show.legend=input$boxplotshowlegend,col=input$boxcolline)
            }
          }
          
          
        }
        #### Boxplot Section END

      } 
      ###### Univariate SECTION START
      
      if (is.null(input$y) ) {
        
        validate(need(is.numeric(plotdata[,input$x]), "Please select a numeric x variable"))
        
        p <- ggplot(plotdata, aes_string(x=input$x))
        if (input$colorin != 'None')
          p <- p + aes_string(color=input$colorin)
        if (input$fillin != 'None')
          p <- p + aes_string(fill=input$fillin)
        if (input$groupin != 'None')
          p <- p + aes_string(group=input$groupin)
        if (input$groupin == 'None' & !is.numeric(plotdata[,input$x]) 
            & input$colorin == 'None')
          p <- p + aes(group=1)
        
        if ( input$histogramaddition){
          p <- p+ aes(y=..density..)+
            geom_histogram(alpha=0.2)
        }
        if ( input$densityaddition){
          p <- p+
            geom_density(alpha=0.1)
          
        }
      }
      
      ###### Univariate SECTION END
      
      
      facets <- paste(input$facetrowin,'~', input$facetcolin)
      
      if (input$facetrowextrain !="."&input$facetrowin !="."){
        facets <- paste(input$facetrowextrain ,"+", input$facetrowin, '~', input$facetcolin)
      }  
      if (input$facetrowextrain !="."&input$facetrowin =="."){
        facets <- paste( input$facetrowextrain, '~', input$facetcolin)
      }  
      
      if (input$facetcolextrain !="."){
        facets <- paste( facets, "+",input$facetcolextrain)
      }  
      ASTABLE <- ifelse( input$facetordering=="table",TRUE,FALSE)
      
      if (facets != '. ~ .')
        p <- p + facet_grid(facets,scales=input$facetscalesin,space=input$facetspace
                            ,labeller=input$facetlabeller,margins=input$facetmargin,as.table=ASTABLE )
      
      if (facets != '. ~ .' & input$facetswitch!="" )
        
        p <- p + facet_grid(facets,scales=input$facetscalesin,space=input$facetspace,
                            switch=input$facetswitch
                            , labeller=input$facetlabeller,
                            margins=input$facetmargin ,as.table=ASTABLE)
      
      if (facets != '. ~ .'&input$facetwrap) {
        multiline <-  input$facetwrapmultiline
        
        p <- p + facet_wrap(    c(input$facetrowextrain ,input$facetrowin,input$facetcolin,input$facetcolextrain ) [
          c(input$facetrowextrain ,input$facetrowin,input$facetcolin,input$facetcolextrain )!="."]
          ,scales=input$facetscalesin,
          labeller=label_wrap_gen(width = 25, multi_line = multiline),as.table=ASTABLE)
        
        if (input$facetwrap&input$customncolnrow) {
          multiline <-  input$facetwrapmultiline
          p <- p + facet_wrap(    c(input$facetrowextrain ,input$facetrowin,input$facetcolin,input$facetcolextrain ) [
            c(input$facetrowextrain ,input$facetrowin,input$facetcolin,input$facetcolextrain )!="."]
            ,scales=input$facetscalesin,ncol=input$wrapncol,nrow=input$wrapnrow,
            labeller=label_wrap_gen(width = 25, multi_line = multiline ),as.table=ASTABLE)
        }
      }
      
      
      
      
      if (input$logy& is.numeric(plotdata[,"yvalues"]))
        p <- p + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                               labels = trans_format("log10", math_format(10^.x))) 
      
      if (input$logx&  is.numeric(plotdata[,input$x]))
        p <- p + scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                               labels = trans_format("log10", math_format(10^.x))) 
      
      
      
      if (input$scientificy & is.numeric(plotdata[,"yvalues"]) )
        p <- p  + 
        scale_y_continuous(labels=comma )
      
      if (input$scientificx  &  is.numeric(plotdata[,input$x]) )
        p <- p  + 
        scale_x_continuous(labels=comma) 
      
      
      
      
      if (!is.null(input$y) & length(input$y) >= 2 & input$ylab=="" ){
        p <- p + ylab("Y variable(s)")
      }
      if (!is.null(input$y) & length(input$y) < 2 & input$ylab=="" ){
        p <- p + ylab(input$y)
      }
      xlablinebreak <- gsub("\\\\n", "\\\n", input$xlab)
      ylablinebreak <- gsub("\\\\n", "\\\n", input$ylab)
      
      
      
      if (input$xlab!="")
        p <- p + xlab(xlablinebreak)
      if (input$ylab!="")
        p <- p + ylab(ylablinebreak)
      
      
      if (input$horizontalzero)
        p <-    p+
        geom_hline(aes(yintercept=0))
      
      if (input$customline1)
        p <-    p+
        geom_vline(xintercept=input$vline)
      
      
      if (input$customline2)
        p <-    p+
        geom_hline(yintercept=input$hline)
      
      
      
      if (input$identityline)
        p <-    p+ geom_abline(intercept = 0, slope = 1)
      
      
      
      if (input$customlegendtitle){
        colourpos<-  which( input$legendordering=="colour")
        fillpos  <-  which( input$legendordering=="fill")
        sizepos  <-  which( input$legendordering=="size")
        collegend <-  gsub("\\\\n", "\\\n", input$customcolourtitle)
        filllegend <- gsub("\\\\n", "\\\n", input$customfilltitle)
        sizelegend <- gsub("\\\\n", "\\\n", input$customsizetitle)
        # to do list by row by row etc.
        
        if (input$legendalphacol){
          gcol  <- guide_legend(collegend,ncol=input$legendncolcol,reverse=input$legendrevcol,
                                override.aes = list(alpha = 1))
          if( length(colourpos)!=0) {
            gcol  <- guide_legend(collegend,ncol=input$legendncolcol,reverse=input$legendrevcol,
                                  order= colourpos,override.aes = list(alpha = 1))
          }
        }
        if (!input$legendalphacol){
          gcol  <- guide_legend(collegend,ncol=input$legendncolcol,reverse=input$legendrevcol)
          if( length(colourpos)!=0) {
            gcol  <- guide_legend(collegend,ncol=input$legendncolcol,reverse=input$legendrevcol,
                                  order= colourpos)
          }
        }
        if (input$legendalphafill){
          gfill <- guide_legend(filllegend,ncol=input$legendncolfill,reverse=input$legendrevfill,override.aes = list(alpha = 1))
          if( length(fillpos)!=0) {
            gfill <- guide_legend(filllegend,ncol=input$legendncolfill,reverse=input$legendrevfill,
                                  order = fillpos,override.aes = list(alpha = 1))
          } 
        }
        
        if (!input$legendalphafill){
          gfill <- guide_legend(filllegend,ncol=input$legendncolfill,reverse=input$legendrevfill)
          if( length(fillpos)!=0) {
            gfill <- guide_legend(filllegend,ncol=input$legendncolfill,reverse=input$legendrevfill,
                                  order = fillpos)
          } 
        }
        
        gsize <- guide_legend(sizelegend,ncol=input$legendncolsize,reverse=input$legendrevsize)
        if( length(sizepos)!=0) {
          gsize <- guide_legend(sizelegend,ncol=input$legendncolsize,reverse=input$legendrevsize,
                                order = sizepos)
        }
        
        p <-  p + guides(colour = gcol, size = gsize, fill = gfill)
        
      }
      
      
      if (input$themebw) {
        p <-    p+
          theme_bw(base_size=input$themebasesize)     
      }
      
      
      if (!input$themebw){
        p <- p +
          theme_gray(base_size=input$themebasesize)+
          theme(  
            #axis.title.y = element_text(size = rel(1.5)),
            #axis.title.x = element_text(size = rel(1.5))#,
            #strip.text.x = element_text(size = 16),
            #strip.text.y = element_text(size = 16)
          )
      }
      
      
      p <-    p+theme(
        legend.position=input$legendposition,
        legend.box=input$legendbox,
        legend.direction=input$legenddirection,
        panel.background = element_rect(fill=input$backgroundcol))
      
      if (input$labelguides)
        p <-    p+
        theme(legend.title=element_blank())
      if (input$themeaspect)
        p <-    p+
        theme(aspect.ratio=input$aspectratio)
      if (!input$themetableau){
        p <-  p +
          scale_colour_hue(drop=!input$themecolordrop)+
          scale_fill_hue(drop=!input$themecolordrop)
      }
      
      if (grepl("^\\s+$", input$ylab) ){
        p <- p + theme(
          axis.title.y=element_blank())
      }
      if (grepl("^\\s+$", input$xlab) ){
        p <- p + theme(
          axis.title.x=element_blank())
      }
      
      if (input$rotatexticks ){
        p <-  p+
          theme(axis.text.x = element_text(angle = input$xticksrotateangle,
                                           hjust = input$xtickshjust,
                                           vjust = input$xticksvjust) )
        
      }
      if (input$rotateyticks ){
        p <-  p+
          theme(axis.text.y = element_text(angle = input$yticksrotateangle,
                                           hjust = input$ytickshjust,
                                           vjust = input$yticksvjust) )                              
      }    
      
      
      
      
      if (!is.null(input$xaxiszoomin[1])&!is.numeric(plotdata[,"yvalues"])&
          is.numeric(plotdata[,input$x] )&
          input$facetscalesin!="free_x"&
          input$facetscalesin!="free"
      ){
        if(input$userxzoom){
          p <- p +
            coord_cartesian(xlim= c(input$lowerxin,input$upperxin))
        }
        if(!input$userxzoom){
          p <- p +
            coord_cartesian(xlim= c(input$xaxiszoomin[1],input$xaxiszoomin[2])  )
        }
        
      }
      
      if (!is.null(input$yaxiszoomin[1])&!is.numeric(plotdata[,input$x])&
          is.numeric(plotdata[,"yvalues"] )&
          input$facetscalesin!="free_y"&
          input$facetscalesin!="free"
      ){
        if(input$useryzoom){
          p <- p +
            coord_cartesian(ylim= c(input$loweryin,input$upperyin) )
        }
        if(!input$useryzoom){
          p <- p +
            coord_cartesian(
              ylim= c(input$yaxiszoomin[1],input$yaxiszoomin[2]))
        }
        
      }
      
      
      if (!is.null(input$xaxiszoomin[1])&!is.null(input$yaxiszoomin[1])&
          is.numeric(plotdata[,input$x] )&is.numeric(plotdata[,"yvalues"] )&
          input$facetscalesin!="free_x"&input$facetscalesin!="free_y"&
          input$facetscalesin!="free"
      ){
        
        if (input$userxzoom&input$useryzoom){
          p <- p +
            coord_cartesian(xlim= c(input$lowerxin,input$upperxin),
                            ylim= c(input$loweryin,input$upperyin)  )
        }
        if (input$userxzoom&!input$useryzoom){
          p <- p +
            coord_cartesian(xlim= c(input$lowerxin,input$upperxin),
                            ylim= c(input$yaxiszoomin[1],input$yaxiszoomin[2])  )
        }
        if (!input$userxzoom&input$useryzoom){
          p <- p +
            coord_cartesian(xlim= c(input$xaxiszoomin[1],input$xaxiszoomin[2]),
                            ylim= c(input$loweryin,input$upperyin)  )
        }
        if (!input$userxzoom&!input$useryzoom){
          p <- p +
            coord_cartesian(xlim= c(input$xaxiszoomin[1],input$xaxiszoomin[2]),
                            ylim= c(input$yaxiszoomin[1],input$yaxiszoomin[2])  )
        }
      }
      
      
      
      
      #p <- ggplotly(p)
      p
    }
  })
  
  output$plot <- renderPlot({
    plotObject()
  })
  
  
  output$ui_plot <-  renderUI({                 
    plotOutput('plot',  width = "100%" ,height = input$height,
               click = "plot_click",
               hover = hoverOpts(id = "plot_hover", delayType = "throttle"),
               brush = brushOpts(id = "plot_brush"))
  })
  
  output$plotinfo <- renderPrint({
    df<- recodedata5()  
    if (is.null(df)) return(NULL)
    nearPoints( reorderdata2(), input$plot_click, threshold = 5, maxpoints = 5,
                addDist = TRUE) #,xvar=input$x, yvar=input$y
  })
  
  
  output$clickheader <-  renderUI({
    df <-recodedata5()
    if (is.null(df)) return(NULL)
    h4("Clicked points")
  })
  
  output$brushheader <-  renderUI({
    df <- recodedata5()
    if (is.null(df)) return(NULL)
    h4("Brushed points")
    
  })
  
  output$plot_clickedpoints <- renderTable({
    # For base graphics, we need to specify columns, though for ggplot2,
    # it's usually not necessary.
    df<- recodedata5()  
    if (is.null(df)) return(NULL)
    
    res <- nearPoints(recodedata5(), input$plot_click, input$x, "yvalues")
    if (nrow(res) == 0|is.null(res))
      return(NULL)
    res
  })
  output$plot_brushedpoints <- renderTable({
    df<- recodedata5()  
    if (is.null(df)) return(NULL)
    res <- brushedPoints(recodedata5(), input$plot_brush, input$x,"yvalues")
    if (nrow(res) == 0|is.null(res))
      return(NULL)
    res
  })
  
  
  
  
  downloadPlotType <- reactive({
    input$downloadPlotType  
  })
  
  observe({
    plotType    <- input$downloadPlotType
    plotTypePDF <- plotType == "pdf"
    plotUnit    <- ifelse(plotTypePDF, "inches", "pixels")
    plotUnitDef <- ifelse(plotTypePDF, 7, 480)
    
    updateNumericInput(
      session,
      inputId = "downloadPlotHeight",
      label = sprintf("Height (%s)", plotUnit),
      value = plotUnitDef)
    
    updateNumericInput(
      session,
      inputId = "downloadPlotWidth",
      label = sprintf("Width (%s)", plotUnit),
      value = plotUnitDef)
    
  })
  
  
  # Get the download dimensions.
  downloadPlotHeight <- reactive({
    input$downloadPlotHeight
  })
  
  downloadPlotWidth <- reactive({
    input$downloadPlotWidth
  })
  
  # Get the download file name.
  downloadPlotFileName <- reactive({
    input$downloadPlotFileName
  })
  
  # Include a downloadable file of the plot in the output list.
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste(downloadPlotFileName(), downloadPlotType(), sep=".")   
    },
    # The argument content below takes filename as a function
    # and returns what's printed to it.
    content = function(con) {
      # Gets the name of the function to use from the 
      # downloadFileType reactive element. Example:
      # returns function pdf() if downloadFileType == "pdf".
      plotFunction <- match.fun(downloadPlotType())
      plotFunction(con, width = downloadPlotWidth(), height = downloadPlotHeight())
      print(plotObject())
      dev.off(which=dev.cur())
    }
  )
  
  
  }

shinyApp(ui = ui, server = server,  options = list(height = 3000))