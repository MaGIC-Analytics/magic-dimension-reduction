# PCA BiPlots 
############################################################################

observe({
    updateSelectInput(session, "PCAColor", choices=colnames(InputReactive()$meta_data), selected=input$Colby)
    updateSelectInput(session, "PCAShape", choices=c("None",colnames(InputReactive()$meta_data)), selected='None')
})

pca_plotter <- reactive({
    tryCatch({
        p <- pca(InputReactive()$count_data, metadata=InputReactive()$meta_data, removeVar=0.1)
    },
    error=function(e)
    {
        showNotification(paste(e), type='error', duration=NULL)
    })
    
    
    if(input$PCAShape=='None'){
        shapeby=NULL
    } else {
        shapeby=input$PCAShape
    }

    pca_plot <- biplot(p, colby=input$PCAColor, shape=shapeby,
        legendPosition=input$PCALegendPos, legendLabSize=input$PCALegLabSize, legendIconSize=input$PCALegIconSize, 
        pointSize=input$PCAPointSize, labSize=input$PCALabelSize,
        encircle=input$Encirc, encircleFill=input$FEncirc,
        ellipse=input$Ellipse, ellipseType=input$EType, ellipseFill=input$Efill

    )

    return(pca_plot)

})

observe({
    output$pca_out <- renderPlot({
        pca_plotter()
    }, height=input$PHeight, width=input$PWidth)
})

output$DownloadP <- downloadHandler(
    filename=function(){
        paste('PCA',input$DownPFormat,sep='.')
    },
    content=function(file){   
        if(input$DownPFormat=='jpeg'){
            jpeg(file, height=input$PHeight, width=input$PWidth)
            print(pca_plotter())
            dev.off()
        }
        if(input$DownPFormat=='png'){
            png(file, height=input$PHeight, width=input$PWidth)
            print(pca_plotter())
            dev.off()
        }
        if(input$DownPFormat=='tiff'){
            tiff(file, height=input$PHeight, width=input$PWidth, res=1000)
            print(pca_plotter())
            dev.off()
        }
    }
)

# Eigenplots 
############################################################################
observe({
    updateSelectizeInput(session, "EigenMetavars", choices=colnames(InputReactive()$meta_data))
})

eigencorplotter <- reactive({
    
    tryCatch({
        p <- pca(InputReactive()$count_data, metadata=InputReactive()$meta_data, removeVar=0.1)
    },
    error=function(e)
    {
        showNotification(paste(e), type='error', duration=NULL)
    })
    
    validate(need(length(input$EigenMetavars)>=2, message='You must select at least 2 metadata columns'))
    
    eplot <- eigencorplot(p, 
        metavars=input$EigenMetavars,
        cexCorval=input$ELabSize,
        cexLabX=input$EXLabSize,
        cexLabY=input$EYLabSize,
        colCorval = 'white',
        fontCorval = 2,
        posLab = 'bottomleft',
        rotLabX = input$ELabRot,
        posColKey = input$ELegendPos,
        cexLabColKey = input$ELabColSize,
        scale = TRUE,
        main = 'PC1-10 metadata correlations',
        colFrame = 'white',
        plotRsquared = input$ERsquare,
        corFUN = input$EFun,
        corUSE = 'pairwise.complete.obs',
        corMultipleTestCorrection = input$ECorrect,
        signifSymbols = c('***', '**', '*', ''),
        signifCutpoints = c(0, 0.0001, 0.001, 0.01, 1))
    return(eplot)
})

observe({
    output$eigen_out <- renderPlot({
        eigencorplotter()
    }, height=input$PHeight, width=input$PWidth)
})
output$DownloadEigen <- downloadHandler(
    filename=function(){
        paste('eigencorplot',input$DownEigenFormat,sep='.')
    },
    content=function(file){   
        if(input$DownEigenFormat=='jpeg'){
            jpeg(file, height=input$PHeight, width=input$PWidth)
            print(eigencorplotter())
            dev.off()
        }
        if(input$DownEigenFormat=='png'){
            png(file, height=input$PHeight, width=input$PWidth)
            print(eigencorplotter())
            dev.off()
        }
        if(input$DownEigenFormat=='tiff'){
            tiff(file, height=input$PHeight, width=input$PWidth, res=1000)
            print(eigencorplotter())
            dev.off()
        }
    }
)

# Screeplots
############################################################################
scree_plotter <- reactive({
    
    tryCatch({
        p <- pca(InputReactive()$count_data, metadata=InputReactive()$meta_data, removeVar=0.1)
    },
    error=function(e)
    {
        showNotification(paste(e), type='error', duration=NULL)
    })
    
    scree_plot <- screeplot(p,
        xlabAngle=input$SLabRot,
        axisLabSize=input$SLabSize
    )

    return(scree_plot)
})

observe({
    output$scree_out <- renderPlot({
        scree_plotter()
    }, height=input$PHeight, width=input$PWidth)
})
output$DownloadScreePlot <- downloadHandler(
    filename=function(){
        paste('scree_plot',input$DownScreeFormat,sep='.')
    },
    content=function(file){   
        if(input$DownScreeFormat=='jpeg'){
            jpeg(file, height=input$PHeight, width=input$PWidth)
            print(scree_plotter())
            dev.off()
        }
        if(input$DownScreeFormat=='png'){
            png(file, height=input$PHeight, width=input$PWidth)
            print(scree_plotter())
            dev.off()
        }
        if(input$DownScreeFormat=='tiff'){
            tiff(file, height=input$PHeight, width=input$PWidth, res=1000)
            print(scree_plotter())
            dev.off()
        }
    }
)

# PCA 3D plots
############################################################################

pca_plotter3d <- reactive({
    tryCatch({
        p <- prcomp(t(InputReactive()$count_data))
        components <- data.frame(p[['x']])
        components <- cbind(components, InputReactive()$meta_data[[paste(input$PCAColor)]])

    },
    error=function(e)
    {
        showNotification(paste(e), type='error', duration=NULL)
    })
    
    pca_plot3d <- plot_ly(components, 
        x=~PC1,
        y=~PC2,
        z=~PC3,
        color=~InputReactive()$meta_data[[paste(input$PCAColor)]],
        height=input$PHeight,
        width=input$PWidth, 
        marker= list(
            size=input$PCAPointSize
        )
    )

    return(pca_plot3d)

})

observe({
    output$pca_out3d <- renderPlotly({
        pca_plotter3d()
    })
})
