function(input, output, session) {
    options(shiny.maxRequestSize=30*1024^2)
    source('ui.R',local=TRUE)
    source('input.R',local=TRUE)
    source('tabmanagement.R',local=TRUE)
    source('pca.R',local=TRUE)
    source('tsne.R',local=TRUE)
    source('umap.R',local=TRUE)
}