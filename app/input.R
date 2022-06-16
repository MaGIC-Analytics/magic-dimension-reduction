output$data_selector <- renderUI({
    if (is.null(input$counts_file) || is.null(input$metadata_file)) return()
    tagList(
        h3('Select which columns will be the labelling data for the plot.'),
        hr(),
        selectInput("Colby", label="Choose metadata column", choices=NULL),
        p("Generally this would be your grouping variable"),
        hr(),
        actionButton('submit',"Submit Data",class='btn btn-info'),
        p("Once the data is submitted, you can view the plot on the various other plot tabs")
    )
})

output$data_demo <- renderUI({
    tagList(
        h3('Select which demo columns will be the labelling data for the plot.'),
        hr(),
        selectInput("DemoColby", label="Choose metadata column", choices=c('Group','SampleNum'), selected='Group'),
        p("Generally this would be your grouping variable"),
        hr(),
        actionButton('demo_submit',"Submit Demo Data",class='btn btn-info'),
        p("Play with some demo data!")
    )
})

observe({
    col_options=colnames(InputReactive()$meta_data)
    updateSelectInput(session, "Colby", choices=col_options)
})

InputReactive <- reactive({

    if(input$DemoData==TRUE){
        shiny::validate(
            need((!is.null(input$counts_file) && !is.null(input$metadata_file)),
            message='Please enter a valid csv or tsv file for the counts and metadata files')
        )
        shiny::validate(
            need(colnames(as.matrix(fread(input$counts_file$datapath, header=T), rownames=1))==rownames(as.matrix(fread(input$metadata_file$datapath, header=T), rownames=1)),
            message='The columns in your data do not match the sample names in the metadata')
        )
        
        tryCatch({
            count_data <- as.data.frame(as.matrix(fread(input$counts_file$datapath, header=T), rownames=1))
            meta_data <- as.data.frame(as.matrix(fread(input$metadata_file$datapath, header=T), rownames=1))
        },
        error=function(e)
        {
            showNotification(paste(e), type='error', duration=NULL)
        })

    
    } else if(input$DemoData==FALSE){
        tryCatch({
            count_data <- as.data.frame(as.matrix(fread('www/demo.csv', header=T), rownames=1))
            meta_data <- as.data.frame(as.matrix(fread('www/metadata_demo.csv', header=T), rownames=1))
        },
        error=function(e)
        {
            showNotification(paste(e), type='error', duration=NULL)
        })

    
    }
    return(list(
        'count_data'=count_data,
        'meta_data'=meta_data
    ))

})

output$input_table <- DT::renderDataTable({
    DT::datatable(InputReactive()$count_data, style='bootstrap',options=list(pageLength = 15,scrollX=TRUE))
})
output$meta_table <- DT::renderDataTable({
    DT::datatable(InputReactive()$meta_data, style='bootstrap',options=list(pageLength = 15,scrollX=TRUE))
})