# UMAP
############################################################################

observe({
    sel <- input$Colby %||% input$DemoColby
    updateSelectInput(session, "UMAPColor",  choices=colnames(InputReactive()$meta_data), selected=sel)
    updateSelectInput(session, "ULabChoice", choices=colnames(InputReactive()$meta_data), selected=sel)
})

# Cap the neighbors slider to what the dataset can support (n_neighbors < n)
observe({
    req(InputReactive()$count_data)
    n <- ncol(InputReactive()$count_data)
    updateSliderInput(session, "uneighbors", max = max(2, n - 1))
})

umap_plotter <- reactive({
    tryCatch({
        set.seed(as.numeric(input$useed))
        mat <- InputReactive()$count_data %>% t() %>% as.data.frame() %>% distinct(.keep_all=TRUE)
        nb  <- safe_neighbors(input$uneighbors, nrow(mat))
        if (nb != input$uneighbors) {
            showNotification(
                sprintf("n_neighbors capped to %s for %s samples (UMAP needs n_neighbors < n).", nb, nrow(mat)),
                type='warning')
        }
        umap_fit <- mat %>% umap(n_components=3, n_neighbors=nb, n_epochs=input$uepochs, metric=input$umetric, min_dist=input$udist, set_op_mix_ratio=input$uopmix, local_connectivity=input$ulocal, bandwidth=input$uband, alpha=input$ualpha, gamma=input$ugamma, negative_sample_rate=input$uneg, spread=input$uspread)
        umap_df <- umap_fit$layout %>% as.data.frame() %>% rename(UMAP1="V1", UMAP2="V2",UMAP3="V3") %>% mutate(ID=row_number()) %>%
            inner_join(InputReactive()$meta_data %>% mutate(ID=row_number()), by="ID")

    },
    error=function(e)
    {
        showNotification(paste(e), type='error', duration=NULL)
    })

    return(umap_df)

})

# Build the 2D UMAP ggplot (shared by the live plot and the downloads)
build_umap_2d <- function(umap_data) {
    p <- umap_data %>%
        ggplot(aes(x=UMAP1, y=UMAP2, color=eval(as.name(input$UMAPColor)))) +
        geom_point(size=input$UMAPPointSize) + theme_bw() + theme(
            legend.position=input$ULegendPos,
            legend.text=element_text(size=input$ULegLabSize),
            legend.title = element_blank(),
            axis.text=element_text(size=input$UAxisSize)
        )
    if (isTRUE(input$ULabels)) {
        p <- p + geom_label_repel(aes(label=eval(as.name(input$ULabChoice))), size=input$ULabSize)
    }
    add_overlay_2d(p, umap_data, "UMAP1", "UMAP2", input$UMAPColor,
        mode  = input$UOverlay %||% "none",
        fill  = input$UFEncirc %||% TRUE,
        etype = input$UEType %||% "norm")
}

observe({
    output$umap2d_out <- renderPlot({
        build_umap_2d(umap_plotter())
    }, height=input$UHeight, width=input$UWidth)
})

observe({
    output$umap3d_out <- renderPlotly({
        umap_data <- umap_plotter()
        plot_ly(umap_data,
            x=~UMAP1,
            y=~UMAP2,
            z=~UMAP3,
            color=~InputReactive()$meta_data[[paste(input$UMAPColor)]],
            height=input$UHeight,
            width=input$UWidth,
            marker= list(
                size=input$UMAPPointSize
                )
            )
    })
})


output$DownloadU <- downloadHandler(
    filename=function(){
        paste('UMAP',input$DownUFormat,sep='.')
    },
    content=function(file){
        if(input$DownUFormat=='jpeg'){
            jpeg(file, height=input$UHeight, width=input$UWidth)
        } else if(input$DownUFormat=='png'){
            png(file, height=input$UHeight, width=input$UWidth)
        } else if(input$DownUFormat=='tiff'){
            tiff(file, height=input$UHeight, width=input$UWidth, res=1000)
        }
        print(build_umap_2d(umap_plotter()))
        dev.off()
    }
)
