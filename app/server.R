function(input, output, session) {
    options(shiny.maxRequestSize=30*1024^2)
    source('ui.R',local=TRUE)
}