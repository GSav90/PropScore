# SELLER
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#install.packages("shiny")
library(shiny)
source("seller.R")

listings <- read.csv("~/Seller_PropScore/data/Property Score_v4.csv", header= T, sep= ",")
results_new=data.frame()
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
                                    tabPanel(title= "Home"),
                                    tabPanel(title= "Sell",
                                             mainPanel(
                                               fluidRow(column(12, h2("Please Enter details for your Property:"))),
                                               fluidRow(column(4, textInput("addr","","",placeholder = "Address")),
                                                        column(4, textInput("city","","",placeholder = "City")),
                                                        column(2, textInput("state","","PA",placeholder = "State")),
                                                        column(2, textInput("zip","","",placeholder = "Zip"))),
                                               fluidRow(column(3, textInput("price","Price","",placeholder = "Price")),
                                                        column(3, textInput("sqft","Area","",placeholder = "Area in sqft")),
                                                        column(3, textInput("age","Property Age","",placeholder = "Age in Years")),
                                                        column(3, selectInput(inputId= "saletype",label= "Sale Type", 
                                                                              choices = list("House",
                                                                                             "Condo",
                                                                                             "Apartment",
                                                                                             "Lot/Land",
                                                                                             "Auction",
                                                                                             "For Sale by Owner",
                                                                                             "TownHouse",
                                                                                             "Foreclosure",
                                                                                             "New Construction")))),
                                               fluidRow(column(4, numericInput(inputId= "num_bed", label= "No. of bedrooms", value= 0)),
                                                        column(4, numericInput(inputId= "num_bath", label= "No. of bathrooms", value= 0))),
                                               fluidRow(h4("Other Features Available")),
                                               fluidRow(column(2, radioButtons("parking", "Parking",c("Yes"="1", "No"="0"))),
                                                        column(2, radioButtons("heating", "Heating",c("Yes"="1", "No"="0"))),
                                                        column(2, radioButtons("basement", "Basement",c("Yes"="1", "No"="0"))),
                                                        column(2, radioButtons("attic", "Attic",c("Yes"="1", "No"="0"))),
                                                        column(2, radioButtons("green", "Green Building",c("Yes"="1", "No"="0")))
                                               ),
                                               fluidRow(column(12, actionButton("submit","Submit"))),
                                               br(),
                                               br(),
                                               fluidRow(column(12, dataTableOutput("s01_listings")))
                                             )
                                    ),
                                    tabPanel(title= "View My Listings",
                                             mainPanel(fluidRow(column(12,h2("My Listings"))),
                                                       br(),
                                                       fluidRow(column(12, dataTableOutput("listings"))))),
                                    navbarMenu(title= "More",
                                               tabPanel(title= "About Us"),
                                               tabPanel(title= "Contact Us")),
                                    tabPanel(" "),
                                    tabPanel(" "),
                                    tabPanel(" "),
                                    tabPanel(" "),
                                    tabPanel(" "),
                                    tabPanel(" "),
                                    tabPanel(" "),
                                    tabPanel(" "),
                                    tabPanel(" "),
                                    tabPanel(" "),
                                    tabPanel(" "),
                                    tabPanel(" "),
                                    tabPanel(title= "Welcome Seller_01"),
                                    tabPanel(title= "Log Out", 
                                             value= actionLink("logout", "Log Out")
                                    ))
                  )
                ),
                
                headerPanel(h2(""), windowTitle= "PropertyScore"),
                
                sidebarLayout(
                  # Main Panel - body
                  mainPanel(
                  ),
                  sidebarPanel(fluidRow(column(12, textAreaInput(inputId= "About Us", 
                                                                 label= h3("Chat Live (24/7)"),
                                                                 placeholder= "Help us help you. \nGet in touch with your personal home expert today."),
                                               actionButton("chat", "Chat with live agent")))
                  )
                )
)

# Define server logic required 
server <- function(input, output) {
  
  s01_listings <- eventReactive(input$submit, 
                                {df.new.prop <- data.frame(address = input$addr,
                                                           city = input$city,
                                                           state = input$state,
                                                           zip = input$zip,
                                                           price = as.numeric(input$price),
                                                           sqft = as.numeric(input$sqft),
                                                           bedrooms = as.numeric(input$num_bed), 
                                                           bathrooms = as.numeric(input$num_bath),
                                                           property_age='2006',
                                                           parking = as.numeric(input$parking), 
                                                           heating = as.numeric(input$heating), 
                                                           basement = as.numeric(input$basement), 
                                                           attic = as.numeric(input$attic),
                                                           green_bldg = as.numeric(input$green),
                                                           age_in_years = as.numeric(input$age),
                                                           price_per_sqft= as.numeric(input$price)/as.numeric(input$sqft),
                                                           seller_rating=4.5,
                                                           sale_type = input$saletype,
                                                           price_range='$50k-$100k'
                                                           
                                                           
                                )
                                outp=df.new.prop[,c("bedrooms","bathrooms","parking",
                                                    "heating","basement","attic","age_in_years","price_per_sqft",
                                                    "seller_rating")]
                                
                                results_new <- produce_result(outp,df.new.prop)
                                #write.csv(results_new,"PropertyScore_upd.csv")
                                names(results_new) <- toupper(names(results_new))
                                #print.data.frame(results_new)
                                #results_new
                                results_new[,-c(6,9,15,16,19)]
                                }
  )
  output$s01_listings <- renderDataTable({
    s01_listings()
  }, options = list(pageLength = 5))
  
  output$listings <- renderDataTable({
    results <- listings
    names(results) <- toupper(names(results))
    results[,-c(9,15,16,19)]
  }, options = list(pageLength = 5))
}

# Run the application 
shinyApp(ui = ui, server = server)

