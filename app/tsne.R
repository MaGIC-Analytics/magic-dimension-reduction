# tSNE
############################################################################

observe({
    sel <- input$Colby %||% input$DemoColby
    updateSelectInput(session, "tSNEColor",  choices=colnames(InputReactive()$meta_data), selected=sel)
    updateSelectInput(session, "TLabChoice", choices=colnames(InputReactive()$meta_data), selected=sel)
})

# Cap the perplexity slider to what the dataset can support (3*perplexity < n-1)
observe({
    req(InputReactive()$count_data)
    n <- ncol(InputReactive()$count_data)
    updateSliderInput(session, "tperplex", max = max(1, floor((n - 1) / 3)))
})

tsne_plotter <- reactive({
    tryCatch({
        set.seed(as.numeric(input$tseed))
        mat  <- InputReactive()$count_data %>% t() %>% as.data.frame() %>% distinct(.keep_all=TRUE)
        n    <- nrow(mat)
        perp <- safe_perplexity(input$tperplex, n)
        dims <- min(input$tdims, n - 1)
        if (perp != input$tperplex) {
            showNotification(
                sprintf("Perplexity capped to %s for %s samples (tSNE needs 3*perplexity < n-1).", perp, n),
                type='warning')
        }
        tsne_fit <- mat %>% Rtsne(dims=3, theta=input$ttheta, initial_dims=dims, perplexity=perp, pca_center=input$tcenter, pca_scale=input$tscale, normalize=input$tnorm, momentum=input$tmomentum, final_momentum=input$tmomentumfin, stop_lying_iter=input$tstoplie, mom_switch_iter=input$tmomentumswitch, eta=input$teta, exaggeration_factor=input$texag, max_iter=input$titer)
        tsne_df <- tsne_fit$Y %>% as.data.frame() %>% rename(tSNE1='V1',tSNE2="V2",tSNE3="V3") %>% mutate(ID=row_number())
        tsne_df <- tsne_df %>% inner_join(InputReactive()$meta_data %>% mutate(ID=row_number()), by='ID')

    },
    error=function(e)
    {
        showNotification(paste(e), type='error', duration=NULL)
    })

    return(tsne_df)

})

# Build the 2D tSNE ggplot (shared by the live plot and the downloads)
build_tsne_2d <- function(tsne_data) {
    p <- tsne_data %>%
        ggplot(aes(x=tSNE1, y=tSNE2, color=eval(as.name(input$tSNEColor)))) +
        geom_point(size=input$tSNEPointSize) + theme_bw() + theme(
            legend.position=input$TLegendPos,
            legend.text=element_text(size=input$TLegLabSize),
            legend.title = element_blank(),
            axis.text=element_text(size=input$TAxisSize)
        )
    if (isTRUE(input$TLabels)) {
        p <- p + geom_label_repel(aes(label=eval(as.name(input$TLabChoice))), size=input$TLabSize)
    }
    add_overlay_2d(p, tsne_data, "tSNE1", "tSNE2", input$tSNEColor,
        mode  = input$TOverlay %||% "none",
        fill  = input$TFEncirc %||% TRUE,
        etype = input$TEType %||% "norm")
}

observe({
    output$tsne2d_out <- renderPlot({
        build_tsne_2d(tsne_plotter())
    }, height=input$THeight, width=input$TWidth)
})

observe({
    output$tsne3d_out <- renderPlotly({
        tsne_data <- tsne_plotter()
        plot_ly(tsne_data,
            x=~tSNE1,
            y=~tSNE2,
            z=~tSNE3,
            color=~InputReactive()$meta_data[[paste(input$tSNEColor)]],
            height=input$THeight,
            width=input$TWidth,
            marker= list(
                size=input$tSNEPointSize
                )
            )
    })
})

output$DownloadT <- downloadHandler(
    filename=function(){
        paste('tSNE',input$DownTFormat,sep='.')
    },
    content=function(file){
        if(input$DownTFormat=='jpeg'){
            jpeg(file, height=input$THeight, width=input$TWidth)
        } else if(input$DownTFormat=='png'){
            png(file, height=input$THeight, width=input$TWidth)
        } else if(input$DownTFormat=='tiff'){
            tiff(file, height=input$THeight, width=input$TWidth, res=1000)
        }
        print(build_tsne_2d(tsne_plotter()))
        dev.off()
    }
)
