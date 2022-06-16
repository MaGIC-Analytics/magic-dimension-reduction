# UMAP
############################################################################

observe({
    updateSelectInput(session, "UMAPColor", choices=colnames(InputReactive()$meta_data), selected=input$Colby)
})
observe({
    updateSelectInput(session, "ULabChoice", choices=colnames(InputReactive()$meta_data), selected=input$Colby)
})

umap_plotter <- reactive({
    tryCatch({
        set.seed(as.numeric(input$useed))
        umap_fit <- InputReactive()$count_data %>% t() %>% as.data.frame() %>% distinct(.keep_all=TRUE) %>% umap(n_components=3, n_neighbors=input$uneighbors,n_epochs=input$uepochs, metric=input$umetric, min_dist=input$udist, set_op_mix_ratio=input$uopmix, local_connectivity=input$ulocal, bandwidth=input$uband, alpha=input$ualpha, gamma=input$ugamma, negative_sample_rate=input$uneg, spread=input$uspread)
        umap_df <- umap_fit$layout %>% as.data.frame() %>% rename(UMAP1="V1", UMAP2="V2",UMAP3="V3") %>% mutate(ID=row_number()) %>%
            inner_join(InputReactive()$meta_data %>% mutate(ID=row_number()), by="ID") 

    },
    error=function(e)
    {
        showNotification(paste(e), type='error', duration=NULL)
    })

    return(umap_df)

})

observe({
    output$umap2d_out <- renderPlot({
        umap_data <- umap_plotter()
        if(input$ULabels==TRUE){
            umap_data %>% 
            ggplot(aes(x=UMAP1,
            y=UMAP2,
            color=eval(as.name(input$UMAPColor)))) + geom_point(size=input$UMAPPointSize) + geom_label_repel(
                aes(label=eval(as.name(input$ULabChoice))), size=input$ULabSize
                )+ theme_bw() + theme(
                legend.position=input$ULegendPos,
                legend.text=element_text(size=input$ULegLabSize),
                legend.title = element_blank(),
                axis.text=element_text(size=input$UAxisSize)
            )

        } else {
            umap_data %>% 
            ggplot(aes(x=UMAP1,
            y=UMAP2,
            color=eval(as.name(input$UMAPColor)))) + geom_point(size=input$UMAPPointSize) + theme_bw() + theme(
                legend.position=input$ULegendPos,
                legend.text=element_text(size=input$ULegLabSize),
                legend.title = element_blank(),
                axis.text=element_text(size=input$UAxisSize)
            )
        }

       
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
            umap_data <- umap_plotter()
            if(input$ULabels==TRUE){
                plot <- umap_data %>% 
                ggplot(aes(x=UMAP1,
                y=UMAP2,
                color=eval(as.name(input$UMAPColor)))) + geom_point(size=input$UMAPPointSize) + geom_label_repel(
                    aes(label=eval(as.name(input$ULabChoice))), size=input$ULabSize
                    )+ theme_bw() + theme(
                    legend.position=input$ULegendPos,
                    legend.text=element_text(size=input$ULegLabSize),
                    legend.title = element_blank(),
                    axis.text=element_text(size=input$UAxisSize)
                )

            } else {
                plot <- umap_data %>% 
                ggplot(aes(x=UMAP1,
                y=UMAP2,
                color=eval(as.name(input$UMAPColor)))) + geom_point(size=input$UMAPPointSize) + theme_bw() + theme(
                    legend.position=input$ULegendPos,
                    legend.text=element_text(size=input$ULegLabSize),
                    legend.title = element_blank(),
                    axis.text=element_text(size=input$UAxisSize)
                )
            }
            print(plot)
            dev.off()
        }
        if(input$DownUFormat=='png'){
            png(file, height=input$UHeight, width=input$UWidth)
            umap_data <- umap_plotter()
            if(input$ULabels==TRUE){
                plot <- umap_data %>% 
                ggplot(aes(x=UMAP1,
                y=UMAP2,
                color=eval(as.name(input$UMAPColor)))) + geom_point(size=input$UMAPPointSize) + geom_label_repel(
                    aes(label=eval(as.name(input$ULabChoice))), size=input$ULabSize
                    )+ theme_bw() + theme(
                    legend.position=input$ULegendPos,
                    legend.text=element_text(size=input$ULegLabSize),
                    legend.title = element_blank(),
                    axis.text=element_text(size=input$UAxisSize)
                )

            } else {
                plot <- umap_data %>% 
                ggplot(aes(x=UMAP1,
                y=UMAP2,
                color=eval(as.name(input$UMAPColor)))) + geom_point(size=input$UMAPPointSize) + theme_bw() + theme(
                    legend.position=input$ULegendPos,
                    legend.text=element_text(size=input$ULegLabSize),
                    legend.title = element_blank(),
                    axis.text=element_text(size=input$UAxisSize)
                )
            }
            print(plot)
            dev.off()
        }
        if(input$DownUFormat=='tiff'){
            tiff(file, height=input$UHeight, width=input$UWidth, res=1000)
            umap_data <- umap_plotter()
            if(input$ULabels==TRUE){
                plot <- umap_data %>% 
                ggplot(aes(x=UMAP1,
                y=UMAP2,
                color=eval(as.name(input$UMAPColor)))) + geom_point(size=input$UMAPPointSize) + geom_label_repel(
                    aes(label=eval(as.name(input$ULabChoice))), size=input$ULabSize
                    )+ theme_bw() + theme(
                    legend.position=input$ULegendPos,
                    legend.text=element_text(size=input$ULegLabSize),
                    legend.title = element_blank(),
                    axis.text=element_text(size=input$UAxisSize)
                )

            } else {
                plot <- umap_data %>% 
                ggplot(aes(x=UMAP1,
                y=UMAP2,
                color=eval(as.name(input$UMAPColor)))) + geom_point(size=input$UMAPPointSize) + theme_bw() + theme(
                    legend.position=input$ULegendPos,
                    legend.text=element_text(size=input$ULegLabSize),
                    legend.title = element_blank(),
                    axis.text=element_text(size=input$UAxisSize)
                )
            }
            print(plot)
            dev.off()
        }
    }
)