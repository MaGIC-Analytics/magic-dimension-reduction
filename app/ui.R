library(shiny)
require(shinyjs)
library(shinythemes)
require(shinycssloaders)
library(shinyWidgets)

library(DT)
library(tidyverse)
library(data.table)
library(colourpicker)
library(RColorBrewer)
library(Rtsne)
library(umap)
library(PCAtools)
library(plotly)
library(ggalt)

tagList(
    tags$head(
        includeHTML(("www/GA.html")),
        tags$style(type = 'text/css','.navbar-brand{display:none;}')
    ),
    fluidPage(theme = shinytheme('yeti'),
            windowTitle = "MaGIC Dimensionality Reduction Tool",
            titlePanel(
                fluidRow(
                column(2, tags$a(href='http://www.bioinformagic.io/', tags$img(height =75 , src = "MaGIC_Icon_0f344c.svg")), align = 'center'), 
                column(10, fluidRow(
                  column(10, h1(strong('MaGIC Dimensionality Reduction Tool'), align = 'center',style="color:#0F344C;")),
                  ))
                ),
                windowTitle = "MaGIC Dimensionality Reduction Tool" ),
                tags$style(type='text/css', '.navbar{font-size:20px;}'),
                tags$style(type='text/css', '.nav-tabs{padding-bottom:20px;}'),
                tags$style(type='text/css', '.navbar-value{background-color:#0F344C;}'),
                tags$style(type='text/css', HTML('.navbar { background-color: #0F344C;}
                          .tab-panel{ background-color: #0F344C;}
                          .navbar-value .navbar-nav > .active > a, 
                           .navbar-value .navbar-nav > .active > a:focus, 
                           .navbar-value .navbar-nav > .active > a:hover {
                                color: white;
                                background-color: #008cba;
                            }')
                          ),
                tags$head(tags$style(".modal-dialog{ width:1300px}")),
        navbarPage(title ="", id='NAVTABS',

        ## Intro Page
##########################################################################################################################################################
            tabPanel('Introduction',
                fluidRow(
                    column(2,
                    ),
                    column(8,
                        column(12, align = "center", 
                            style="margin-bottom:25px;",
                            h3(markdown("Welcome to the Dimensionality Reduction Tool by the
                            [Molecular and Genomics Informatics Core (MaGIC)](http://www.bioinformagic.io)."))),
                        hr(),
                    ),
                    column(2,
                    )
                ),
                fluidRow(
                    column(2,
                    ),
                    column(8, align='center', style="margin-bottom:30px;",
                        img(src="dimensions.gif")
                    ),
                    column(2,
                    )
                ),
                fluidRow(
                    column(3,
                    ),
                    column(6,
                        column(12, align = "left", 
                            style="margin-bottom:25px;",
                            h4("About Dimensionality Reduction"),
                            markdown("Dimensionality reduction can be used to reduce datasets with high numbers of features into smaller summarized dimensions. This can be used to visualize similarities and dissimilarities between samples in your dataset. 
                            Ideally, samples that are prescribed as similar should group with like samples- for example your control group and treatment group should cluster respectively.This type of dimensionality reduction can usually be performed using PCA, tSNE, or UMAP. 
                            "),
                            hr(),
                            h4("PCA vs tSNE vs UMAP"),
                            markdown("Each dimensionality reduction method has its own use. In many datasets they will tell a similar story, but it is best to decide which optimally fits your experimental design. In general, PCA will be used for smaller datasets (such as a few samples RNA-seq) and will begin to scale into tSNE and UMAP as the sample N increases, such as for scRNA-seq.  
                            "),
                            column(4,
                                wellPanel(
                                    h6("PCA"),
                                    markdown("Principal component analysis (PCA) is a common linear method of dimensionality reduction. 
                                    In essence, the variant features in your dataset are distilled into eigenvector components that capture the maximum amount of variance.") 
                                )
                            ),
                            column(4,
                                wellPanel(
                                    h6("tSNE"),
                                    markdown("t-Stochastic Neighbor Embedding (tSNE) is a graph based and non-linear dimensionality reduction method.
                                    Essentially, it is calculating the distance for the embedding on the distance to neighbor cells in PCA space.
                                    ") 
                                )
                            ),
                            column(4,
                                wellPanel(
                                    h6("UMAP"),
                                    markdown("Uniform Manifold Approximation and Projection (UMAP) is a non-linear dimensionality reduction method.
                                    UMAP is similar to tSNE, but scales effectively and preserves local/global distances for delineating groups.
                                    ") 
                                )
                            ),
                            hr(),
                            h4("Data Sources"),
                            markdown("The data for dimensionality reduction can come from any type of count data. A few examples are RNA-seq normalized hit counts, Luminex assay counts, plant petal size/color, etc. 
                            If your data sources contain smaller N's, start with the PCA plots. Both tSNE and UMAP are designed for larger datasets, and will require custom hyperparameter tweaks to run, even with the demo data. 
                            "),
                            hr(),
                            h4("Minimum requirements"),
                            markdown("To use this tool, at minimum you must have a tsv/csv table containing with the first row containing your row identifiers (for example Gene IDs), followed by a column with count data per sample (for example VST normalized hit counts).
                            Additionally you need a metadata table. The first column should contain the sample names matching the columns in the count data. Each subsequent column can include any non-measured data, such as grouping variables.
                            
                            ")),
                        hr(),
                    ),
                    column(3,
                    )
                ),

                
               
            ),

        ## Input Data
##########################################################################################################################################################
            tabPanel('Input Data',
                fluidRow(
                    column(3,
                        wellPanel(
                            h2('Input Data', align='center'),
                            hr(),
                            materialSwitch("DemoData", label="Upload your own data", value=FALSE, right=TRUE, status='info'),
                            conditionalPanel("input.DemoData",
                                fileInput('counts_file','Select your counts file',
                                accept=c(
                                    'text/csv',
                                    'text/comma-separated-values, text/plain',
                                    '.csv',
                                    'text/tsv',
                                    'text/tab-separated-values, text/plain',
                                    '.tsv'), 
                                multiple=FALSE
                                ),
                                fileInput('metadata_file','Select your metadata file',
                                accept=c(
                                    'text/csv',
                                    'text/comma-separated-values, text/plain',
                                    '.csv',
                                    'text/tsv',
                                    'text/tab-separated-values, text/plain',
                                    '.tsv'), 
                                multiple=FALSE
                                ),
                                uiOutput('data_selector')
                            ),
                            conditionalPanel("input.DemoData==false",
                                uiOutput('data_demo')
                            )
                        )
                    ),
                    column(9,
                        tabsetPanel(id='InputTables',
                            tabPanel(title='Data table', hr(),
                                withSpinner(type=6, color='#5bc0de',
                                    dataTableOutput('input_table')
                                )
                            ),
                            tabPanel(title='Metadata table', hr(),
                                withSpinner(type=6, color='#5bc0de',
                                    dataTableOutput('meta_table')
                                )
                            )
                        )
                    )
                )
            ),

        ## PCA
##########################################################################################################################################################
            tabPanel('PCA',
                fluidRow(
                    column(3,
                        wellPanel(
                            h2('Plot Options', align='center'),
                            conditionalPanel("input.PCAplots=='Biplots' || input.PCAplots=='3D PCA'",
                                materialSwitch("PointOpts", label="Show point options", value=FALSE, right=TRUE, status='info'),
                                conditionalPanel("input.PointOpts",
                                    selectInput("PCAColor", label='Color By', choices=NULL),
                                    sliderInput("PCAPointSize","Point size: ", min=0, max=30, step=1, value=5),
                                ),
                                conditionalPanel("input.PointOpts && input.PCAplots=='Biplots'",
                                    selectInput("PCAShape", label='Shape By', choices=NULL),
                                )
                            ),
                            conditionalPanel("input.PCAplots=='Biplots'",
                                materialSwitch("ShowLabs", label="Show label options", value=FALSE, right=TRUE, status='info'),
                                conditionalPanel("input.ShowLabs",
                                    sliderInput("PCALabelSize","Label size: ", min=0, max=30, step=1, value=5),
                                    materialSwitch("Encirc", label="Encircle the samples", value=FALSE, right=TRUE, status='info'),
                                    conditionalPanel("input.Encirc",
                                        materialSwitch("FEncirc", label="Fill the encircle", value=TRUE, right=TRUE, status='info'),
                                    ),
                                    materialSwitch("Ellipse", label="Stat ellipses", value=FALSE, right=TRUE, status='info'),
                                    conditionalPanel("input.Ellipse",
                                        radioButtons("EType", label="Ellipse Type", inline=TRUE,
                                            choices=c("T"='t'), selected="t"),
                                        materialSwitch("Efill", label="Ellipse fill", value=TRUE, right=TRUE, status='info'),
                                    ),
                                ),
                                materialSwitch("LegOpts", label="Legend Options", value=FALSE, right=TRUE, status='info'),
                                conditionalPanel("input.LegOpts",
                                    radioButtons("PCALegendPos", label="Legend Position", inline=TRUE,
                                        choices=c("Top"='top',"Bottom"='bottom','Right'='right','left'='left'), selected="bottom"),
                                    sliderInput("PCALegLabSize","Legend Label size: ", min=1, max=30, step=1, value=10),
                                    sliderInput("PCALegIconSize","Legend Icon size: ", min=1, max=30, step=1, value=5)
                                )
                            ),
                            conditionalPanel("input.PCAplots=='Eigenplots'",
                                selectizeInput("EigenMetavars", label='Select the metadata variables', choices=NULL, multiple=TRUE),
                                materialSwitch("EigenLabs", label="Show label options", value=FALSE, right=TRUE, status='info'),
                                conditionalPanel("input.EigenLabs",
                                    sliderInput("ELabSize","Value Label size: ", min=1, max=30, step=1, value=2),
                                    sliderInput("EXLabSize","X-axis Label Size: ", min=1, max=30, step=1, value=2),
                                    sliderInput("EYLabSize","Y-axis Label Size: ", min=1, max=30, step=1, value=2),
                                    sliderInput("ELabRot", "X-axis rotation: ", min=0, max=360, step=45, value=45)
                                ),
                                materialSwitch("EigenFun", label="Show function options", value=FALSE, right=TRUE, status='info'),
                                conditionalPanel("input.EigenFun",
                                    radioButtons("EFun", label='Correlation Function', inline=TRUE,
                                        choices=c("Pearson"='pearson','Spearman'='spearman','Kendall'='kendall'), selected='pearson'),
                                    radioButtons("ERsquare", label="R or R squared", inline=TRUE, 
                                        choices=c("R"='FALSE',"R Squared"='TRUE'), selected="FALSE"),
                                    radioButtons("ECorrect", label="Multiple Testing Correction", inline=TRUE, 
                                        choices=c('False Discovery Rate'='fdr',"Bonferroni"='bonferroni','None'='none'), selected='fdr'),
                                ),
                                radioButtons("ELegendPos", label="Legend Position", inline=TRUE, 
                                    choices=c("Top"='top',"Bottom"='bottom','Right'='right','left'='left'), selected="top"),
                                sliderInput("ELabColSize","Legend Label Size: ", min=1, max=30, step=1, value=2),
                            ),
                            conditionalPanel("input.PCAplots=='Scree Plot'",
                                materialSwitch("ScreeLabs", label='Show label options', value=FALSE, right=TRUE, status='info'),
                                conditionalPanel('input.ScreeLabs',
                                    sliderInput("SLabRot", "X-axis rotation: ", min=0, max=360, step=45, value=45),
                                    sliderInput("SLabSize","Label Size: ", min=1, max=30, step=1, value=16),
                                ),
                            ),
                            materialSwitch("Resize", label="Resize Image", value=FALSE, right=TRUE, status='info'),
                            conditionalPanel("input.Resize",
                                sliderInput('PHeight', label='Plot Heights: ', min=50, max=2000, step=10, value=800),
                                sliderInput('PWidth', label='Plot Widths: ',  min=50, max=2000, step=10, value=800)
                            )
                        )
                    ),
                    column(9,
                        tabsetPanel(id='PCAplots',
                            tabPanel(title='Biplots', hr(),
                                withSpinner(type=6, color='#5bc0de',  
                                    plotOutput("pca_out", height='100%')
                                ),
                                fluidRow(align='center',style="margin-top:25px;",
                                    column(12, selectInput("DownPFormat", label='Choose download format', choices=c('jpeg','png','tiff'))),
                                    column(12, downloadButton('DownloadP', 'Download the PCA Biplot'),style="margin-bottom:50px;")
                                )
                            ),
                            tabPanel(title='Eigenplots', hr(),
                                withSpinner(type=6, color='#5bc0de',  
                                    plotOutput("eigen_out", height='100%')
                                ),
                                fluidRow(align='center',style="margin-top:25px;",
                                    column(12, selectInput("DownEigenFormat", label='Choose download format', choices=c('jpeg','png','tiff'))),
                                    column(12, downloadButton('DownloadEigen', 'Download the Eigenplot'),style="margin-bottom:50px;")
                                )
                            ),
                            tabPanel(title='3D PCA', hr(),
                                withSpinner(type=6, color='#5bc0de',  
                                    plotlyOutput("pca_out3d", width='auto',height='auto')
                                )
                            ),
                            tabPanel(title='Scree Plot', hr(),
                                withSpinner(type=6, color='#5bc0de',  
                                    plotOutput("scree_out", height='100%')
                                ),
                                fluidRow(align='center',style="margin-top:25px;",
                                    column(12, selectInput("DownScreeFormat", label='Choose download format', choices=c('jpeg','png','tiff'))),
                                    column(12, downloadButton('DownloadScreePlot', 'Download the Scree plot'),style="margin-bottom:50px;")
                                )
                            
                            )
                        )
                    )
                )
            ),

        ## tSNE
##########################################################################################################################################################
            tabPanel('tSNE',
                fluidRow(
                    column(3,
                        wellPanel(
                            h2('Plot Options', align='center'),
                            conditionalPanel("input.tSNEplots=='2D tSNE' || input.tSNEplots=='3D tSNE'",
                                materialSwitch("TPointOpts", label="Show point options", value=FALSE, right=TRUE, status='info'),
                                conditionalPanel("input.TPointOpts",
                                    selectInput("tSNEColor", label='Color By', choices=NULL),
                                    sliderInput("tSNEPointSize","Point size: ", min=0, max=30, step=1, value=5),
                                    conditionalPanel("input.tSNEplots=='2D tSNE'",
                                        materialSwitch("TLabels", label="Show point labels", value=FALSE, right=TRUE, status='info'),
                                        conditionalPanel("input.TLabels",
                                            selectInput("TLabChoice", label='Label By', choices=NULL),
                                            sliderInput("TLabSize","Label size: ", min=1, max=30, step=1, value=4),
                                        )
                                    ),
                                ),
                                conditionalPanel("input.tSNEplots=='2D tSNE'",
                                    materialSwitch("TLegend", label="Show legend options", value=FALSE, right=TRUE, status='info'),
                                    conditionalPanel("input.TLegend",
                                    sliderInput("TLegLabSize", "Legend Label Size: ", min=1, max=40, step=1, value=10),
                                    radioButtons("TLegendPos", label="Legend Position", inline=TRUE,
                                        choices=c("Top"='top',"Bottom"='bottom','Right'='right','left'='left'), selected="bottom"),
                                    sliderInput("AxisSize", "Axes Label Size: ", min=1, max=40, step=1, value=10),
                                    ),
                                ),
                                materialSwitch('AdvancedtSNE',label='Advanced tSNE options', value=FALSE, right=TRUE, status='info'),
                                conditionalPanel("input.AdvancedtSNE",
                                    markdown("**Caution**. These are some of the tSNE hyperparameters. We strongly recommend you do not tweak these unless you understand the changes being made. Some of these will drastically increase runtime and memory consumption or trigger crashes. "),
                                    sliderInput("tperplex", label="Perplexity (should be 3*perplexity < nrow(x)-1)", min=1, max=100, step=1,value=30),
                                    sliderInput("ttheta",label="Theta value (speed vs accuracy)", min=0, max=1,step=0.1, value=0.5),
                                    sliderInput("titer",label="Number of iterations", min=1, max=2000,step=50, value=1000),
                                    numericInput("tseed", label="Seed value", 0),
                                    sliderInput("tdims", label="Input PCA dimensions", min=1, max=100, step=1,value=50),
                                    materialSwitch("tcenter",label="Center PCA data", value=TRUE, right=TRUE, status='info'),
                                    materialSwitch("tnorm",label="Internally normalize PCA data", value=FALSE, right=TRUE, status='info'),
                                    materialSwitch("tscale",label="Scale PCA data", value=FALSE, right=TRUE, status='info'),
                                    sliderInput("tmomentum", label="Momentum", min=0.1, max=2, step=0.1,value=0.5),
                                    sliderInput("tmomentumfin", label="Final Momentum", min=0.1, max=1,step=0.1, value=0.8),
                                    sliderInput("tstoplie", label="Stop lying iteration", min=0, max=500, step=1,value=250),
                                    sliderInput("tmomentumswitch", label="Final momentum iteration", min=0, max=500,step=1, value=250),
                                    sliderInput("teta", label="Learning rate", min=0, max=500,step=10, value=200),
                                    sliderInput("texag", label="Exageration factor", min=0, max=50,step=0.5, value=12)
                                )
                            ),
                            materialSwitch("Resize", label="Resize Image", value=FALSE, right=TRUE, status='info'),
                            conditionalPanel("input.Resize",
                                sliderInput('THeight', label='Plot Heights: ', min=50, max=2000, step=10, value=800),
                                sliderInput('TWidth', label='Plot Widths: ',  min=50, max=2000, step=10, value=800)
                            )
                        )
                    ),
                    column(9,
                        tabsetPanel(id='tSNEplots',
                            tabPanel(title='2D tSNE', hr(),
                                withSpinner(type=6, color='#5bc0de',  
                                    plotOutput("tsne2d_out", height='100%')
                                ),
                                fluidRow(align='center',style="margin-top:25px;",
                                    column(12, selectInput("DownTFormat", label='Choose download format', choices=c('jpeg','png','tiff'))),
                                    column(12, downloadButton('DownloadT', 'Download the 2D tSNE'),style="margin-bottom:50px;")
                                )
                            ),
                            tabPanel(title='3D tSNE', hr(),
                                withSpinner(type=6, color='#5bc0de',  
                                    plotlyOutput("tsne3d_out", width='auto',height='auto')
                                )
                            )
                        )
                    )
                )
            ),

        ## UMAP
##########################################################################################################################################################
            tabPanel('UMAP',
                fluidRow(
                    column(3,
                        wellPanel(
                            h2('Plot Options', align='center'),
                            conditionalPanel("input.UMAPplots=='2D UMAP' || input.UMAPplots=='3D UMAP'",
                                materialSwitch("UPointOpts", label="Show point options", value=FALSE, right=TRUE, status='info'),
                                conditionalPanel("input.UPointOpts",
                                    selectInput("UMAPColor", label='Color By', choices=NULL),
                                    sliderInput("UMAPPointSize","Point size: ", min=0, max=30, step=1, value=5),
                                    conditionalPanel("input.UMAPplots=='2D UMAP'",
                                        materialSwitch("ULabels", label="Show point labels", value=FALSE, right=TRUE, status='info'),
                                        conditionalPanel("input.ULabels",
                                            selectInput("ULabChoice", label='Label By', choices=NULL),
                                            sliderInput("ULabSize","Label size: ", min=1, max=30, step=1, value=4),
                                        )
                                    )
                                )
                            ),
                            conditionalPanel("input.UMAPplots=='2D UMAP'",
                                materialSwitch("ULegend", label="Show legend options", value=FALSE, right=TRUE, status='info'),
                                conditionalPanel("input.ULegend",
                                    sliderInput("ULegLabSize", "Legend Label Size: ", min=1, max=40, step=1, value=10),
                                    radioButtons("ULegendPos", label="Legend Position", inline=TRUE,
                                        choices=c("Top"='top',"Bottom"='bottom','Right'='right','left'='left'), selected="bottom"),
                                    sliderInput("UAxisSize", "Axes Label Size: ", min=1, max=40, step=1, value=10),
                                ),
                            ),
                            materialSwitch('AdvancedUMAP',label='Advanced UMAP options', value=FALSE, right=TRUE, status='info'),
                            conditionalPanel("input.AdvancedUMAP",
                                markdown("**Caution**. These are some of the UMAP hyperparameters. We strongly recommend you do not tweak these unless you understand the changes being made. Some of these will drastically increase runtime and memory consumption or trigger crashes. "),
                                sliderInput("uneighbors",label="Number of nearest neighbors", min=1, max=50,step=1, value=15),
                                numericInput("useed", label="Seed value", 0),
                                sliderInput("uepochs",label="Number epochs", min=1, max=2000,step=10, value=200),
                                selectInput("umetric", label="Choose distance metric", choices=c('euclidean','manhattan', 'cosine','pearson','pearson2'), selected='euclidean'),
                                sliderInput("udist", label="Minimum distance", min=0, max=1, step=0.1,value=0.1),
                                sliderInput("uopmix", label="Op mix ratio", min=0, max=1, step=0.01,value=1),
                                sliderInput("ulocal", label="Local connectivity", min=1, max=50, step=1,value=1),
                                sliderInput("uband", label="Bandwidth", min=1, max=50, step=1,value=1),
                                sliderInput("ualpha", label="Alpha", min=1, max=50, step=1,value=1),
                                sliderInput("ugamma", label="Gamma", min=1, max=50, step=1,value=1),
                                sliderInput("uneg", label="Negative sample rate", min=1, max=50, step=1,value=5),
                                sliderInput("uspread", label="Spread", min=1, max=50, step=1,value=1),
                            ),
                            materialSwitch("Resize", label="Resize Image", value=FALSE, right=TRUE, status='info'),
                            conditionalPanel("input.Resize",
                                sliderInput('UHeight', label='Plot Heights: ', min=50, max=2000, step=10, value=800),
                                sliderInput('UWidth', label='Plot Widths: ',  min=50, max=2000, step=10, value=800)
                            )
                        )
                    ),
                    column(9,
                        tabsetPanel(id='UMAPplots',
                            tabPanel(title='2D UMAP', hr(),
                                withSpinner(type=6, color='#5bc0de',  
                                    plotOutput("umap2d_out", height='100%')
                                ),
                                fluidRow(align='center',style="margin-top:25px;",
                                    column(12, selectInput("DownUFormat", label='Choose download format', choices=c('jpeg','png','tiff'))),
                                    column(12, downloadButton('DownloadU', 'Download the UMAP plot'),style="margin-bottom:50px;")
                                )
                            ),
                            tabPanel(title='3D UMAP', hr(),
                                withSpinner(type=6, color='#5bc0de',  
                                    plotlyOutput("umap3d_out", width='auto',height='auto')
                                )
                            )
                        )
                    )
                )
            ),
            
            
        ## Footer
##########################################################################################################################################################
            tags$footer(
                wellPanel(
                    fluidRow(
                        column(4, align='center',
                        tags$a(href="https://github.com/MaGIC-Analytics/magic-dimension-reduction", icon("github", "fa-3x")),
                        tags$h4('GitHub to submit issues/requests')
                        ),
                        column(4, align='center',
                        tags$a(href="http://www.bioinformagic.io/", icon("magic", "fa-3x")),
                        tags$h4('MaGIC Home Page')
                        ),
                        column(4, align='center',
                        tags$a(href="https://github.com/MaGIC-Analytics", icon("address-card", "fa-3x")),
                        tags$h4("Developer's Page")
                        )
                    ),
                    fluidRow(
                        column(12, align='center',
                            HTML('<a href="https://www.youtube.com/watch?v=dQw4w9WgXcQ">
                            <p>&copy; 
                                <script language="javascript" type="text/javascript">
                                var today = new Date()
                                var year = today.getFullYear()
                                document.write(year)
                                </script>
                            </p>
                            </a>
                            ')
                        )
                    ) 
                )
            )
        )#Ends navbarPage,
    )#Ends fluidpage
)#Ends tagList