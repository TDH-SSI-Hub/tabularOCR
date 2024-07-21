

library('shiny')
library('plotly')
library('magick')
library('tesseract')
library('formattable')
#library('rstudioapi')
library('data.table')
library('png')
library('stringr')

#folder<-dirname(rstudioapi::getSourceEditorContext()$path)
#setwd(folder)

base.code<-readLines('OCR template tabular.r')
source('OCR functions tabular.r')


# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("OCR generator"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(width = 4
                     ,tabsetPanel(id = 'sidetab'
                                  , tabPanel(title='Setup'
                                             
                                             ,selectInput('file','File',choices = list.files(pattern = '.pdf'))
                                             , actionButton('rotate','Rotate')
                                             ,splitLayout(textInput('c1','Search for'),textInput('c1name','Name'),cellWidths = '50%')
                                             ,splitLayout(textInput('c2',''),textInput('c2name',''),cellWidths = '50%')
                                             ,splitLayout(textInput('c3',''),textInput('c3name',''),cellWidths = '50%')
                                             ,splitLayout(textInput('c4',''),textInput('c4name',''),cellWidths = '50%')
                                             ,splitLayout(textInput('c5',''),textInput('c5name',''),cellWidths = '50%')
                                             ,actionButton('Classify','Classify')
                                             ,textOutput('groups')
                                  )
                                  , tabPanel(title='Boxes'
                                             ,textInput('boxLabel','Label')
                                             ,checkboxGroupInput('boxengine','Allowed Characters',  choices = engines, inline = T)
                                             , textInput('box.other.chars','Also allow')
                                             ,actionButton('boxOCR','Read')
                                             ,actionButton('boxCreate','Save')
                                             , tableOutput('boxTable')
                                             ,textOutput('box_out')
                                             )
                                  ,tabPanel(title='Lines'
                                            ,tabsetPanel(id = 'LinesTab'
                                                         , tabPanel(title='Lines'
                                                                    , selectInput('form','Form',choices = c('c1','c2','c3','c4','c5'))
                                                                    ,uiOutput('page')
                                                                    
                                                                    ,splitLayout(
                                                                        numericInput('Vline.num','Vline.num',1, 1,10)
                                                                        ,actionButton('m5','-5')
                                                                        ,actionButton('p5','+5')
                                                                        , cellWidths = '33%'
                                                                    )
                                                                    ,splitLayout( numericInput('row.h','Row Height', 45, 0, 300, step = 1)
                                                                                  ,numericInput('top.margin','Top Margin',min=0, value = 0)
                                                                                  , uiOutput('bot.margin')
                                                                                  ,numericInput('cell.buffer','Cell Buffer',min=1, value = 1)
                                                                                  , cellWidths = '25%'
                                                                    )
                                                                    ,actionButton('initialize','Inititialize')
                                                                    ,actionButton('detect.lines','Detect Lines')
                                                                    , textOutput('selected.point')
                                                         )
                                                         
                                                         , tabPanel(title='Columns'
                                                                    , splitLayout( uiOutput('column.line.sel')
                                                                                   , selectInput('column.name','Column Name', choices = initial.columns )
                                                                                   , cellWidths = '50%'
                                                                                   ,tags$head(tags$style(HTML("
                              .shiny-split-layout > div {
                                overflow: visible;
                              }
                              "))))
                              ,checkboxGroupInput('engine','Allowed Characters',  choices = engines, inline = T)
                              , textInput('other.chars','Also allow')
                              #, actionButton('browser','Browser')
                              , actionButton('ocr.col','OCR')
                              , textOutput('ocr.col.out')
                              
                                                         )
                                            )
                                  )
                              
                              
                              ,tabPanel(title='OCR'
                                        ,uiOutput('column.select')
                                        ,actionButton('read.page','Read')
                                        ,actionButton('Add','Add to Report')
                                        ,textOutput('brushed')
                                        ,actionButton('read.form','Read Form')
                                        ,actionButton('reset.column','Reset Column')
                                        ,formattableOutput('report')
                                        ,textInput('colname.ar','Column Name')
                                        ,actionButton('add.column','Add Column')
                                        ,actionButton('rem.column','Remove Column')
                              )
                              ,tabPanel(title='Code'
                                        ,textInput('filename','File Name', value = 'Generated OCR output')
                                        ,actionButton('Code','Generate Code')
                                        ,actionButton('Run','Run Code')
                                        ,actionButton('write.map','Write Map')
                                        ,actionButton('save.inputs','Save Inputs')
                              )
                     )
        ),
        
        # Show a plot of the generated distribution
        mainPanel(width = 8
                  ,plotlyOutput("distPlot", height = '1100px',width = '850px' )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    values<-reactiveValues()
    values$form.options<-1
    values$lab.elements<-lab.elements
    values$test<-image_read_pdf(list.files(pattern = '.pdf')[1], density = 400, pages=1)
    values$trim.area<-"100x200+100+100"
    init.col.lines<-rep(0,10)
    values$vlines<-init.col.lines
    values$vlines.all<-list(c1=init.col.lines,c2=init.col.lines,c3=init.col.lines,c4=init.col.lines,c5=init.col.lines)
    
    values$boxTable<-data.frame(Label=character(0),geom=character(0),engine=character(0),ocr=character(0))
    observeEvent(input$boxCreate,{
      values$boxTable<-rbind(values$boxTable, data.frame(Label=input$boxLabel
                                                         ,geom=values$read.area
                                                         ,engine=paste(input$boxengine,collapse = '')
                                                         ,ocr=read.element(values$test,values$read.area, paste(input$boxengine,collapse = '')) ))
    })
    output$boxTable<-renderTable(values$boxTable[c('Label','geom','ocr')])
    
    init.row.lines<-rep(0,10)
    values$hlines<-init.row.lines
    values$hlines.all<-list(c1=init.row.lines,c2=init.row.lines,c3=init.row.lines,c4=init.row.lines,c5=init.row.lines)
    
    values$row.height<-45
    values$moveline<-1
    values$top.margin<-0
    values$top.margin.all<-list(c1=0,c2=0,c3=0,c4=0,c5=0)
    values$bot.margin<-3300
    values$bot.margin.all<-list(c1=3300,c2=3300,c3=3300,c4=3300,c5=3300)
    values$cell.buffer<-1
    values$col.lines<-c()
    values$col.lines.all<-list(c1=c(),c2=c(),c3=c(),c4=c(),c5=c())
    ocr.df<-as.data.frame(matrix('',ncol=length(initial.columns)))
    colnames(ocr.df)<-initial.columns
    values$ocr.output<- ocr.df
    values$form.pages<-list(c1='',c2='',c3='',c4='',c5='')
    values$active.forms<-c()
    
    observe(values$test.all<-image_read_pdf(input$file))
    observeEvent(input$rotate,{values$test.all<-image_rotate(values$test.all,90)})
    
    observe(values$test<-values$test.all[1])
    
    observe(values$test.raster<-image_convert(values$test,format = 'raster'))
    observe(values$test.info<-image_info(values$test))
    observe(values$img.width<-as.integer(values$test.info[2]))
    observe(values$img.height<-as.integer(values$test.info[3]))
    observe({values$test.df<-data.frame(x=seq(0,values$img.width,10),y=c(1000))})
    observe(values$cell.buffer<-input$cell.buffer)
    
    
    observeEvent(input$initialize,{
        values$cell.buffer<-input$cell.buffer 
        values$top.margin.all[[input$form]]<-input$top.margin
        values$bot.margin.all[[input$form]]<-input$bot.margin
        values$row.height<-input$row.h
    })
    #observe(values$moveline<-as.numeric(input$adjust))
    
    output$page<-renderUI(selectInput('page','Page', choices = seq(1,length(values$test.all),1), selected = 1))
    observeEvent(input$page,{values$test<-values$test.all[as.numeric(input$page)]})
    
    col.geom.all<-vector('list', length(initial.columns))
    names(col.geom.all)<-initial.columns
    #values$col.geom.all<-col.geom.all
    values$col.geom.all<-list(c1=col.geom.all,c2=col.geom.all,c3=col.geom.all,c4=col.geom.all,c5=col.geom.all)
    
    col.engine.all<-vector('list', length(initial.columns))
    names(col.engine.all)<-initial.columns
    #values$col.engine.all<-col.engine.all
    values$col.engine.all<-list(c1=col.engine.all,c2=col.engine.all,c3=col.engine.all,c4=col.engine.all,c5=col.engine.all)
    
    
    observeEvent(input$ocr.col,{
        values$col.geom.all[[input$form]][[input$column.name]]<-create.geom.all(left=values$col.lines[input$column.line.sel]
                                                                                ,right=values$col.lines[input$column.line.sel+1]
                                                                                ,rows=values$hlines
                                                                                , buffer = input$cell.buffer
        )
        values$col.engine.all[[input$form]][[input$column.name]]<-paste0(c(input$engine,input$other.chars), collapse = '')
        example.ocr<-read.col(values$test
                              ,values$col.geom.all[[input$form]][[input$column.name]]
                              ,values$col.engine.all[[input$form]][[input$column.name]])
        
        output$ocr.col.out<-renderText(example.ocr)
    })
    
 
    observeEvent(input$read.page,{
        report.df<-read.page(values$test,input$form,input,values$col.geom.all,values$col.engine.all)
        output$report<-renderFormattable(formattable(report.df))
        
    })
   
    observeEvent(input$read.form,{
        #browser()
        report.df<-read.form(pdf=values$test.all,form=input$form,classes=input,pages=values$form.pages,geoms=values$col.geom.all,engine=values$col.engine.all)
        rownames(report.df)<-NULL
        output$report<-renderFormattable(formattable(report.df))
        
    })  
    

    observeEvent(input$write.map,{
        #browser()
        write.csv(read.pdf(values$test.all,values$active.forms,input,values$form.pages, values$col.geom.all,values$col.engine.all )
                  , paste0(input$filename,'.csv'))
    }) 
    
    observeEvent(input$save.inputs,{
        classes<-list(input$c1name,input$c2name,input$c3name,input$c4name,input$c5name)
        saveRDS(list(
            active.forms=values$active.forms
            ,classes=classes
            ,form.pages=values$form.pages
            ,col.geom.all=values$col.geom.all
            ,col.engine.all=values$col.engine.all
        ), paste0(input$filename,'.rds'))
        saveRDS(list(values$test.all,values$active.forms
                     ,classes
                     ,values$form.pages, values$col.geom.all,values$col.engine.all),'inputs.RDS')
    })
    
    observeEvent(input$Classify,{
        page.ocr<-pdftools::pdf_ocr_text(input$file)
        values$active.forms<-sapply(paste0('c',1:5), function(i) ifelse(input[[i]]!='',i,''))
        values$active.forms<-values$active.forms[values$active.forms!='']
        
        for (c in values$active.forms){
            if(input[[c]]!='') values$form.pages[[c]]<-grep(input[[c]],page.ocr)
        }
        
        output$groups<-renderText(paste0('C1:',paste0(values$form.pages$c1,collapse = '')
                                         ,'\nC2:',paste0(values$form.pages$c2,collapse = '')
                                         ,'\nC3:',paste0(values$form.pages$c3,collapse = '')
                                         ,'\nC4:',paste0(values$form.pages$c4,collapse = '')
                                         ,'\nC5:',paste0(values$form.pages$c5,collapse = '')))
    })
    
    observeEvent(input$m5,{values$vlines[input$Vline.num]<-values$vlines[input$Vline.num]-5})
    observeEvent(input$p5,{values$vlines[input$Vline.num]<-values$vlines[input$Vline.num]+5})
    
    observe(values$row.height<-input$row.h)
    
    
    observeEvent(input$top.margin, values$top.margin.all[[input$form]]<-input$top.margin)
    observe(values$top.margin<-values$top.margin.all[[input$form]])
    
    observeEvent(input$bot.margin,values$bot.margin.all[[input$form]]<-input$bot.margin)
    observe(values$bot.margin<-values$bot.margin.all[[input$form]])
    
    #observe(values$bot.margin<-input$bot.margin)
    observe(values$col.lines<-values$vlines[values$vlines!=0])
    
    output$bot.margin<-renderUI(numericInput('bot.margin','Bot Margin',values$img.height,min=0,max=values$img.height))
    output$column.line.sel<-renderUI({
        numericInput('column.line.sel','Column Select',min=1, max=length(values$col.lines), value = 1)
    })
    observeEvent(input$column.line.sel,{
        req(length(values$col.lines)>1)
        output$col.borders<-renderText(paste0(values$col.lines[(input$column.line.sel)],'-',values$col.lines[input$column.line.sel+1]))
    })
    
    observe(values$report<-data.frame(colname=values$lab.elements,crop=NA,engine=NA,value=NA))
    #output$report<-renderFormattable(formattable(output$ocr.output))
    
    output$selected.point<-renderText(as.character(event_data("plotly_click",source="mysource")))
    
    observeEvent(event_data("plotly_click",source="mysource"),{
        values$vlines.all[[input$form]][input$Vline.num]<-as.numeric(event_data("plotly_click",source="mysource"))[3]
    })
    
    observeEvent(event_data("plotly_brushed",source="mysource"),{
      output$brushed_area<-renderText(unlist(event_data("plotly_brushed",source="mysource")))
    })
    
    toListen <- reactive({
        list(input$row.h,input$top.margin,input$bot.margin)
    })
    
    observeEvent(toListen(),{values$hlines.all[[input$form]]<-seq(values$top.margin,values$bot.margin,by=values$row.height)
    })
    
    observeEvent(input$detect.lines,{values$hlines.all[[input$form]]<-detect.lines(values$test, input$top.margin, input$bot.margin)})
    
    observe(values$vlines<-values$vlines.all[[input$form]])
    observe(values$hlines<-values$hlines.all[[input$form]])
    
    observeEvent(event_data("plotly_brushed",source="mysource"),{
        values$xmin<-event_data("plotly_brushed",source="mysource")$x[1] %>% ifelse(.<0,0,.) %>% round(0)
        values$xmax<-event_data("plotly_brushed",source="mysource")$x[2] %>% ifelse(.>values$img.width,values$img.width,.) %>% round(0)
        values$ymin<-event_data("plotly_brushed",source="mysource")$y[1] %>% ifelse(.<0,0,.) %>% round(0)
        values$ymax<-event_data("plotly_brushed",source="mysource")$y[2] %>% ifelse(.>values$img.height,values$img.height,.) %>% round(0)

        values$read.area<-paste0(values$xmax-values$xmin,'x',values$ymax-values$ymin,'+',values$xmin,'+',values$ymin)
    })
    
    
    output$column.select<-renderUI({selectInput('colname','Column Name',choices = values$lab.elements)})
    
    observeEvent(input$Read,{values$ocr<-read.element(values$test,values$read.area,paste(input$engine,collapse = ''))})
    
    observeEvent(input$boxOCR,{
      #browser()
      values$boxocr<-read.element(values$test,values$read.area, paste(input$boxengine,collapse = ''))
      })
    
    output$box_out<-renderText(values$boxocr)
    
    observeEvent(input$report,{values$report$value<-unlist(sapply(1:length(values$report$crop), function(c) read.element(values$test,values$report$crop[c],values$report$engine[c])))})
    
    
    observeEvent(input$Code,{
        base.code[1]<-paste0("column.names<-c(",get.string(values$report$colname),")")
        base.code[2]<-paste0("geometries<-c(",get.string(values$report$crop),")")
        base.code[3]<-paste0("engines<-c(",get.string(values$report$engine),")")
        file.create(paste0(input$filename,'.r'))
        file.chosen<-file(paste0(input$filename,'.r'))
        write.csv(data.frame(column=values$report$colname
                             ,geometries=values$report$crop
                             ,engines=values$report$engine)
                  ,'Tabular Parameters.csv',row.names = F)
        writeLines(base.code,file.chosen)
    })
    
    observeEvent(input$Run,{
        if(file.exists(paste0(input$filename,'.r'))){
            source(paste0(input$filename,'.r'))
        }
    })
    
    output$brushed <- renderText({ values$ocr})
    
    output$distPlot <- renderPlotly({
        pdata<-rbind(values$test.df,data.frame(x=0,y=c(0,values$img.height)))
        plot_ly(x = pdata$x, y = pdata$y, type = 'scatter', mode = 'markers',source="mysource") %>%
            layout(shapes = unlist(list(lapply(values$hlines,hline, size=input$cell.buffer), lapply(values$vlines, vline, size=input$cell.buffer)), recursive = F),images = list(list(source =  raster2uri(values$test.raster),xref = "x",yref = "y",x = 0,y = 0,sizex = values$img.width,sizey = values$img.height,sizing = "stretch",opacity = 1,layer = "below"))) %>%  
            layout(dragmode = "select",yaxis = list(autorange = "reversed")) %>%
            event_register("plotly_brushed") %>% event_register("plotly_click")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
