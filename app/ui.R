library(shiny)
require(shinyjs)
library(shinythemes)
require(shinycssloaders)
library(shinyWidgets)


tagList(
    tags$head(
        #includeHTML(("www/GA.html")),
        tags$style(type = 'text/css','.navbar-brand{display:none;}')
    ),
    fluidPage(theme = shinytheme('yeti'),
            windowTitle = "MODULE TITLE",
            titlePanel(
                fluidRow(
                column(2, tags$a(href='http://www.bioinformagic.io/', tags$img(height =75 , src = "MaGIC_Icon_0f344c.svg")), align = 'center'), 
                column(10, fluidRow(
                  column(10, h1(strong('MODULE TITLE'), align = 'center',style="color:#0F344C;")),
                  ))
                ),
                windowTitle = "MODULE TITLE" ),
                tags$style(type='text/css', '.navbar{font-size:20px;}'),
                tags$style(type='text/css', '.nav-tabs{padding-bottom:20px;}'),
                tags$style(type='text/css', '.navbar-default{background-color:#0F344C;}'),
                tags$style(type='text/css', HTML('.navbar { background-color: #0F344C;}
                          .tab-panel{ background-color: #0F344C;}
                          .navbar-default .navbar-nav > .active > a, 
                           .navbar-default .navbar-nav > .active > a:focus, 
                           .navbar-default .navbar-nav > .active > a:hover {
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
                            h3(markdown("Welcome to the {$TOOL} by the
                            [Molecular and Genomics Informatics Core (MaGIC)](http://www.bioinformagic.io)."))),
                        hr(),
                    ),
                    column(2,
                    )
                ),

                
               
            ),


        ## Page Template
##########################################################################################################################################################
            tabPanel('Page',
                fluidRow(
                    column(3,
                        wellPanel(
                            h2('test', align='center')
                            # 

                        )
                    ),
                    column(9,
                        tabsetPanel(id='Plot',
                            tabPanel(title='output plot', hr()
                            
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
                        tags$a(href="https://github.com/MaGIC-Analytics/{$TOOL}", icon("github", "fa-3x")),
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