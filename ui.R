library(shiny)
library(dplyr)
library(ggplot2)
library(gam)
library(car)
library(mgcv)
library(splines)
library(plotly)
library(rbokeh)
library(effects)
library(reshape2)



# Define UI for application that draws a histogram
navbarPage(
 # theme = shinytheme("lumen"),
  "Modeling Template and Example: Constellation Brand",
  tabPanel(
           "General Sequential Modeling Template",
           mainPanel(
             # h4("We propose a general sequential framework that will apply to most projects analyzing marketing drivers and cannibalization effects.
             #    This sequential framework separate the modeling into three major parts, own driver modeling, cannibalization volume modeling, and competitor drivers modeling.
             #    This sequential framework produces more reasonable coefficients by avoiding multicollinearity.
             #    At each step of the framework, we leverage more complicated statistical model including hierarchical model and Bayesian model as different branch for
             #    different scenario to make the framework generalized to different datasets.
             #    "),
             HTML('<img src="CannibalizationTemplate1202.png"/>'))
           ),
  tabPanel("1. Data Imput and Cleaning",
           mainPanel(tableOutput("table1"))
           ),
  tabPanel("2.1 Exploratory Analysis: Raw Data",
           mainPanel(h4("Select the dimension to see raw data"),
                     fluidRow(
                       column(4,
                              selectInput("CHANNEL",
                                          "Channel:",
                                          c("All",
                                            unique(as.character(final6$CHANNEL))))
                       ),
                       column(4,
                              selectInput("Prod",
                                          "Product:",
                                          c("All",
                                            unique(as.character(final6$Prod))))
                       ),
                       column(4,
                              selectInput("Size",
                                          "Size:",
                                          c("All",
                                            unique(as.character(final6$Size))))
                       )
                     ),
                     # Create a new row for the table.


                     fluidRow(
                       DT::dataTableOutput("table2")
                     ),

                     h4("Select the variable to see trend at selected dimension"),
                     fluidRow(selectInput("Variable21",
                                          "Variable:",
                                          c("Y","PRICE","DISTRIBUTION","TRADE","MEDIA"))),
                     fluidRow(
                       h3("Variable Trend"),
                       dygraphOutput("plot23")
                     )


                     )
           ),
  tabPanel("2.2 Exploratory Analysis: Comparison",
           sidebarPanel(
             selectInput('out', 'Variable', c("Y","TRADE","PRICE","DISTRIBUTION","MEDIA")),
             selectInput('Group', 'Group', c("CHANNEL","Form","Size"))
           ),
           mainPanel(

             fluidRow(

                  h3("Trend over Time"),
                  plotlyOutput("plot10"),
                  h3("YOY Change"),
                  splitLayout(cellWidths = c("95%", "5%"),plotOutput("plot21")),
                  h3("Covariance Matrix and Scatter Plot"),
                  splitLayout(cellWidths = c("40%", "60%"), plotOutput('plot9'), plotlyOutput('plot11')),
                  h3("Distribution across Markets"),
                  plotOutput('plot22')

                  )

           )

           ),
  tabPanel("4.1 Own driver and Competitor Modeling details",
           sidebarPanel(
             selectInput('Group2', 'Group', c("Prod","Form","Product_Code"))
           ),

           mainPanel(

             fluidPage(
               h3("Modeling Formula"),
               withMathJax(),
               uiOutput('ex1'),
               h3("Coefficients Imputation Summary"),
                plotOutput('plot16'),

               h3("Elasticity before and after Imputation"),
               h4("Coefficients calibration applied to each step of the framework, we make sure we get reasonable results before we continue to decompose the volume."),

               splitLayout(cellWidths = c("50%", "50%"), rbokehOutput("plot7", width = 500, height = 540),rbokehOutput("plot6", width = 500, height = 540)),

               h3("Trade Contribution Pattern"),
               plotOutput('plot12'),
               h3("Media Contribution Pattern"),
               plotOutput('plot13')
               )
             )
           ),

#
#   tabPanel("Hierarchical and Bayesian Pontentials",
#            mainPanel(
#              includeHTML("DataScienceWorkShop.html")
#            )
#            ),

  tabPanel("4.2 Own driver and Competitor Modeling results",

  sidebarPanel(
    selectInput('Prod32', 'Product and Size', levels(as.factor(as.character(final6$Product_Code))))
  ),

  mainPanel(

    fluidPage(
      h3("Volume Contribution"),
      splitLayout( plotOutput('plot2')),
      h3("SOVC 2016YTD vs 2015YTD"),
      splitLayout( plotOutput('plot1')),
      h3("Non-Linear Spline"),
      splitLayout(cellWidths = c("40%", "30%","30%"), plotOutput('plot3'), plotOutput('plot4'),plotOutput('plot5'))
    )

  )

 ),


 tabPanel("5. Totoal Cannbalization to each Competitors",
          mainPanel(
          h3("Total Cannibalization on CX attribute to each Competitors"),
          h4("
Here we use CX as an example, the ALL volume bar below is the total cannibalization volume from
all competitor onto CX, which is estimated from previous models. Based on our sequential modeling approach,
we will continue to decompose the total cannibalization into each competitor and then to each driver.
             "),
          plotOutput('plot14'),
          h3("Trend of Total Cannibalization on CX"),
          dygraphOutput("plot8")


          )
          ),
 tabPanel("6. Competitor Drivers",
          mainPanel(
            h3("Competitor Cannibalization volume on CX attribute competitors' drivers"),
            plotOutput('plot15'),

            h3("Competitor Distribution and Price Elasticity on CX"),
            splitLayout(cellWidths = c("50%", "50%"), plotOutput('plot19'), plotOutput('plot20'))
          )

          ),
 tabPanel( "7. Fine Tune Error",
           mainPanel(h4("working with business team to refine the process to get presentable results"))
           ),

 tabPanel( "8. Final results",
   mainPanel(h3("Constellation Brands Modeled Portfolio Growth & Sourcing Impact"),
           h4("by Brand 52 Weeks ending July 24, 2016 vs. YAG"),
           plotOutput("plot18"),
           h3("Brand Inter-Cannibalization"),
           chorddiagOutput('plot17', height = '600px')
   )
 )


)


