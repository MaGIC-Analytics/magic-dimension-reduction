# Hiding and showing tabs on command!
############################################################################
observe({
    if(is.null(input$counts_file) || is.null(input$metadata_file)){
        hideTab(inputId = "NAVTABS", target = "PCA")
        hideTab(inputId = "NAVTABS", target = "tSNE")
        hideTab(inputId = "NAVTABS", target = "UMAP")
    }
})

observeEvent(input$submit,{
    if( (!is.null(input$counts_file)) & (!is.null(input$metadata_file))){
        showTab(inputId = "NAVTABS", target = "PCA")
        showTab(inputId = "NAVTABS", target = "tSNE")
        showTab(inputId = "NAVTABS", target = "UMAP")
        updateTabsetPanel(session, inputId="NAVTABS", selected="PCA")
    }      
})

observeEvent(input$demo_submit,{
    showTab(inputId = "NAVTABS", target = "PCA")
    showTab(inputId = "NAVTABS", target = "tSNE")
    showTab(inputId = "NAVTABS", target = "UMAP")
    updateTabsetPanel(session, inputId="NAVTABS", selected="PCA")
})
