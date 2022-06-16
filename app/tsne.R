# tSNE
############################################################################

observe({
    updateSelectInput(session, "tSNEColor", choices=colnames(InputReactive()$meta_data), selected=input$Colby)
})
observe({
    updateSelectInput(session, "TLabChoice", choices=colnames(InputReactive()$meta_data), selected=input$Colby)
})

tsne_plotter <- reactive({
    tryCatch({
        set.seed(as.numeric(input$tseed))
        tsne_fit <- InputReactive()$count_data %>% t() %>% as.data.frame() %>% distinct(.keep_all=TRUE) %>% Rtsne(dims=3, theta=input$ttheta, initial_dims=input$tdims, perplexity=input$tperplex, pca_center=input$tcenter, pca_scale=input$tscale, normalize=input$tnorm, momentum=input$tmomentum, final_momentum=input$tmomentumfin, stop_lying_iter=input$tstoplie, mom_switch_iter=input$tmomentumswitch, eta=input$teta, exaggeration_factor=input$texag,max_iter=input$titer)
        tsne_df <- tsne_fit$Y %>% as.data.frame() %>% rename(tSNE1='V1',tSNE2="V2",tSNE3="V3") %>% mutate(ID=row_number())
        tsne_df <- tsne_df %>% inner_join(InputReactive()$meta_data %>% mutate(ID=row_number()), by='ID')

    },
    error=function(e)
    {
        showNotification(paste(e), type='error', duration=NULL)
    })

    return(tsne_df)

})

observe({
    output$tsne2d_out <- renderPlot({
        tsne_data <- tsne_plotter()
        if(input$TLabels==TRUE){
            tsne_data %>% 
            ggplot(aes(x=tSNE1,
            y=tSNE2,
            color=eval(as.name(input$tSNEColor)))) + geom_point(size=input$tSNEPointSize) + geom_label_repel(
                aes(label=eval(as.name(input$TLabChoice))), size=input$TLabSize
                )+ theme_bw() + theme(
                legend.position=input$TLegendPos,
                legend.text=element_text(size=input$TLegLabSize),
                legend.title = element_blank(),
                axis.text=element_text(size=input$TAxisSize)
            )

        } else {
            tsne_data %>% 
            ggplot(aes(x=tSNE1,
            y=tSNE2,
            color=eval(as.name(input$tSNEColor)))) + geom_point(size=input$tSNEPointSize) + theme_bw() + theme(
                legend.position=input$TLegendPos,
                legend.text=element_text(size=input$TLegLabSize),
                legend.title = element_blank(),
                axis.text=element_text(size=input$TAxisSize)
            )
        }

       
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
            tsne_data <- tsne_plotter()
            if(input$TLabels==TRUE){
                plot <- tsne_data %>% 
                ggplot(aes(x=tSNE1,
                y=tSNE2,
                color=eval(as.name(input$tSNEColor)))) + geom_point(size=input$tSNEPointSize) + geom_label_repel(
                    aes(label=eval(as.name(input$TLabChoice))), size=input$TLabSize
                    )+ theme_bw() + theme(
                    legend.position=input$TLegendPos,
                    legend.text=element_text(size=input$TLegLabSize),
                    legend.title = element_blank(),
                    axis.text=element_text(size=input$TAxisSize)
                )

            } else {
                plot <- tsne_data %>% 
                ggplot(aes(x=tSNE1,
                y=tSNE2,
                color=eval(as.name(input$tSNEColor)))) + geom_point(size=input$tSNEPointSize) + theme_bw() + theme(
                    legend.position=input$TLegendPos,
                    legend.text=element_text(size=input$TLegLabSize),
                    legend.title = element_blank(),
                    axis.text=element_text(size=input$TAxisSize)
                )
            }
            print(plot)
            dev.off()
        }
        if(input$DownTFormat=='png'){
            png(file, height=input$THeight, width=input$TWidth)
            tsne_data <- tsne_plotter()
            if(input$TLabels==TRUE){
                plot <- tsne_data %>% 
                ggplot(aes(x=tSNE1,
                y=tSNE2,
                color=eval(as.name(input$tSNEColor)))) + geom_point(size=input$tSNEPointSize) + geom_label_repel(
                    aes(label=eval(as.name(input$TLabChoice))), size=input$TLabSize
                    )+ theme_bw() + theme(
                    legend.position=input$TLegendPos,
                    legend.text=element_text(size=input$TLegLabSize),
                    legend.title = element_blank(),
                    axis.text=element_text(size=input$TAxisSize)
                )

            } else {
                plot <- tsne_data %>% 
                ggplot(aes(x=tSNE1,
                y=tSNE2,
                color=eval(as.name(input$tSNEColor)))) + geom_point(size=input$tSNEPointSize) + theme_bw() + theme(
                    legend.position=input$TLegendPos,
                    legend.text=element_text(size=input$TLegLabSize),
                    legend.title = element_blank(),
                    axis.text=element_text(size=input$TAxisSize)
                )
            }
            print(plot)
            dev.off()
        }
        if(input$DownTFormat=='tiff'){
            tiff(file, height=input$THeight, width=input$TWidth, res=1000)
            tsne_data <- tsne_plotter()
            if(input$TLabels==TRUE){
                plot <- tsne_data %>% 
                ggplot(aes(x=tSNE1,
                y=tSNE2,
                color=eval(as.name(input$tSNEColor)))) + geom_point(size=input$tSNEPointSize) + geom_label_repel(
                    aes(label=eval(as.name(input$TLabChoice))), size=input$TLabSize
                    )+ theme_bw() + theme(
                    legend.position=input$TLegendPos,
                    legend.text=element_text(size=input$TLegLabSize),
                    legend.title = element_blank(),
                    axis.text=element_text(size=input$TAxisSize)
                )

            } else {
                plot <- tsne_data %>% 
                ggplot(aes(x=tSNE1,
                y=tSNE2,
                color=eval(as.name(input$tSNEColor)))) + geom_point(size=input$tSNEPointSize) + theme_bw() + theme(
                    legend.position=input$TLegendPos,
                    legend.text=element_text(size=input$TLegLabSize),
                    legend.title = element_blank(),
                    axis.text=element_text(size=input$TAxisSize)
                )
            }
            print(plot)
            dev.off()
        }
    }
)