# BUYER
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(dplyr)
library(shiny)
listings <- read.csv(file= "~/Seller_PropScore/data/Property Score_v4.csv", header= T, sep= ",")

#names(listings) <- toupper(names(listings))

# Define UI for application
ui <- fluidPage(theme= "bootstrap.css",
                
                fluidRow(
                  column(1,
                         img(src="logoBW.jpg"),
                         align= "right"
                  ),
                  column(11,
                         # Navigation bar
                         navbarPage("PropertyScore",
                                    tabPanel(title= "Buy"),
                                    tabPanel(title= "Sell"),
                                    tabPanel(title= "Rent"),
                                    tabPanel(title= "Mortgage"),
                                    tabPanel(title= "View Our Listings", mainPanel(fluidRow(column(12, h2("Shop through our listings here!"))),
                                                                                   fluidRow(column(12, dataTableOutput("listings"))))),
                                    navbarMenu(title= "More",
                                               tabPanel(title= "About Us"),
                                               tabPanel(title= "Contact Us")),
                                    tabPanel(title= " "),
                                    tabPanel(title= " "),
                                    tabPanel(title= " "),
                                    tabPanel(title= " "),
                                    tabPanel(title= " "),
                                    tabPanel(title= " "),
                                    tabPanel(title= " "),
                                    tabPanel(title= " "),
                                    tabPanel(title= "Welcome Buyer_01"),
                                    tabPanel(title= "Log Out", 
                                             value= actionLink("logout", "Log Out")
                                    )
                         )
                  )
                ),
                
                headerPanel(h2("Tell us your preferences"), windowTitle= "IdealHome"),
                
                sidebarLayout(
                  # Main Panel - body
                  mainPanel(
                    fluidRow(
                      column(3, textInput(inputId= "s_city",label= "", placeholder= "City")),
                      column(3, textInput(inputId= "s_zip",label= "", placeholder= "ZipCode")),
                      column(3, selectInput(inputId= "s_price",label= "Price Range", 
                                            choices = list("Select One",
                                                           "Below $50,000",
                                                           "$50,000 - $100,000",
                                                           "$100,000 - $500,000",
                                                           "$200,000 - $350,000",
                                                           "$350,000 - $500,000",
                                                           "$500,000 - $1,000,000",
                                                           "Above $1,000,000"))),
                      column(2, selectInput(inputId= "s_saletype",label= "Sale Type", 
                                            choices = list("Select One",
                                                           "House",
                                                           "Condo",
                                                           "Apartment",
                                                           "Lot/Land",
                                                           "Auction",
                                                           "For Sale by Owner",
                                                           "TownHouse",
                                                           "Foreclosure",
                                                           "New Construction"))),
                      column(2, numericInput(inputId= "num_bed", label= "No. of bedrooms", value= 0)),
                      column(2, numericInput(inputId= "num_bath", label= "No. of bathrooms", value= 0))
                    ),
                    fluidRow(column(3, actionButton("search","Search"))),
                    br(),
                    fluidRow(column(12,dataTableOutput(outputId = "searchRes")))
                  ),
                  sidebarPanel(
                    fluidRow(column(12, h3("Our Top Listings"))),
                    br(),
                    fluidRow(column(12, tableOutput(outputId = "top_listings"))),
                    fluidRow(column(12, textAreaInput(inputId= "About Us", 
                                                      label= h3("Chat Live (24/7)"),
                                                      placeholder= "Help us help you. \nGet in touch with your personal home expert today."),
                                    actionButton("chat", "Chat with live agent")))
                  )
                )
)


# Define server logic
server <- function(input, output,session) {
  #session$onSessionEnded(stopApp)
  
  output$top_listings <- renderTable({
    filtered <- listings[listings$property_score == "Five" & listings$seller_rating >= 4.5,]
    names(filtered) <- toupper(names(filtered))  
    filtered[1:5,c(1,2,4,5)]
  })
  
  searchRes <- eventReactive(input$search, 
                            {results <- listings %>% filter(input$s_zip == zip,
                                                            input$s_city == city,
                                                            input$num_bed == bedrooms,
                                                            input$num_bath == bathrooms,
                                                            input$s_saletype == sale_type)
                                                            #input$s_price == price_range)
                              
                              names(results) <- toupper(names(results))
                              results[,c(1,2,4:8,17)]
                            }
  )
  output$searchRes <- renderDataTable({
    searchRes()
  })
  
  output$listings <- renderDataTable({
    results <- listings
    names(results) <- toupper(names(results))
    results[,-c(9,10:15,16,19)]
  }, options = list(pageLength = 5))
}

# Run the application 
shinyApp(ui= ui, server= server)

