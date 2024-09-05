library(leaflet)
library(plotly)
library(bslib)
library(dplyr)
library(ggplot2)
library(shinyjs)
library(httr)
library(shiny)

# Dummy data for demonstration (Replace with actual data)
setwd("E:/Mt Kenya Carbon Credits")
carbon_data <- read.csv("andersson-2022-nairobi-1-data.csv")

colnames(carbon_data) <- c("Sample_Code", "Week_Day", "Start_Date", "Stop_Date", 
                           "PM2.5", "OC", "BC", "Na", "NH4", "K", 
                           "Mg", "Ca", "F", "Cl", "NO3", 
                           "PO4", "SO4", "EC_14C", "Rainfall", 
                           "D14C", "Ffossil", "Fbiomass", "BCbiomass", "BCfossil")

carbon_data$PM2.5 <- as.numeric(carbon_data$PM2.5)
carbon_data$OC <- as.numeric(carbon_data$OC)
carbon_data$BC <- as.numeric(carbon_data$BC)

total_emissions <- carbon_data %>%
  group_by(Week_Day) %>%
  summarise(Total_PM25 = sum(PM2.5, na.rm = TRUE),
            Total_OC = sum(OC, na.rm = TRUE),
            Total_BC = sum(BC, na.rm = TRUE))

# Function to get GPT response
get_gpt_response <- function(prompt) {
  api_key <- Sys.getenv("OPENAI_API_KEY")  # Set your API key in environment variables
  url <- "https://api.openai.com/v1/completions"
  
  response <- POST(
    url,
    add_headers(Authorization = paste("Bearer", api_key)),
    content_type_json(),
    body = list(
      model = "text-davinci-003",
      prompt = prompt,
      max_tokens = 150
    ),
    encode = "json"
  )
  
  content <- content(response, as = "parsed", type = "application/json")
  return(content$choices[[1]]$text)
}

# UI
ui <- fluidPage(
  useShinyjs(),  # Include shinyjs for JavaScript functionality
  
  theme = bs_theme(version = 5, bootswatch = "lux"),
  
  tags$head(
    tags$style(HTML("
      .navbar-brand {
        padding-top: 5px !important;
      }
      .navbar-brand img {
        height: 40px;
      }
      body {
        font-size: 16px;
        font-family: 'Arial', sans-serif;
      }
      .container {
        max-width: 1200px;
      }
      .section-title {
        margin-top: 40px;
        margin-bottom: 20px;
        font-size: 28px;
        font-weight: bold;
        text-transform: uppercase;
      }
      .section-content {
        margin-bottom: 40px;
        line-height: 1.6;
        color: white; /* White color for About Us text */
        font-weight: bold;
      }
      .alert {
        margin-top: 20px;
      }
      .navbar-nav .nav-link {
        text-transform: uppercase;
      }
      .nav-link.active {
        color: #000 !important;
      }
      #about-us-background {
        background-image: url('https://spadework.org.uk/wp-content/uploads/istockphoto-1387784358-170667a.jpg');
        background-size: cover;
        background-position: center;
        padding: 20px;
        color: white;
        min-height: 100vh;
      }
      #home-background {
        background-image: url('https://t4.ftcdn.net/jpg/04/99/29/67/360_F_499296716_KuUF6yJh33yq9scfnHCV6ukkxJDdYEeL.jpg');
        background-size: cover;
        background-position: center;
        padding: 20px;
        color: white;
        min-height: 100vh;
      }
      .home-content {
        color: white;
        font-weight: bold;
        font-size: 20px;
      }
      .home-title {
        color: white;
        font-weight: bold;
        font-size: 36px;
        text-transform: uppercase;
        margin-bottom: 20px;
      }
      .calculator-container {
        padding: 20px;
        background-color: #f8f9fa;
        border-radius: 8px;
        box-shadow: 0 0 10px rgba(0,0,0,0.1);
      }
      .section-title,
      .section-content,
      .calculator-container,
      .profile-saved-message {
        font-size: 20px;
        font-weight: bold;
        color: black;
      }
    ")),
    tags$script(src = "https://embed.tawk.to/YOUR_TAWK_ID_HERE/default"),
    tags$script(HTML("
      Tawk_API.onLoad = function() {
        Tawk_API.setAttributes({
          'name': 'Visitor',
          'email': 'visitor@example.com'
        });
      };
    "))
  ),
  
  titlePanel(
    div(
      img(src = "https://example.com/logo.png", height = "40px"),
      "CarbonVista Prints Analysis"
    )
  ),
  
  navbarPage(
    "CARBON VISTA",
    
    tabPanel(
      "Home",
      div(id = "home-background",
          fluidRow(
            column(12,
                   h1("Welcome to Carbon Vista", class = "home-title"),
                   p("Explore our platform to analyze carbon emissions, calculate your carbon footprint, and get involved in sustainability projects. Use the menu to navigate to different sections of the app.", class = "home-content"),
                   p("Use the tabs above to access features such as dashboards, calculators, resources, and more.", class = "home-content")
            )
          )
      )
    ),
    
    tabPanel(
      "About Us",
      div(id = "about-us-background",
          fluidRow(
            column(12,
                   h3("About Us", class = "section-title"),
                   p("At Carbon Vista, we are dedicated to providing insights and solutions for reducing carbon footprints and promoting sustainability. Our team of experts is committed to leveraging data and innovative technologies to address climate change and improve environmental health.", class = "section-content"),
                   p("Our mission is to drive positive change by raising awareness, providing actionable data, and supporting projects that contribute to a sustainable future. Through our platform, we aim to empower individuals and organizations to make informed decisions and take meaningful action.", class = "section-content"),
                   h3("Our Contact Information", class = "section-title"),
                   p("Email: contact@carbonvista.com", class = "section-content"),
                   p("Phone: +123-456-7890", class = "section-content"),
                   p("Address: 123 Greenway Lane, Sustainability City, SC 12345", class = "section-content")
            )
          )
      )
    ),
    
    tabPanel(
      "Dashboard",
      sidebarLayout(
        sidebarPanel(
          selectInput("pollutant", "Select Pollutant to Display:", 
                      choices = c("PM2.5", "OC", "BC"), selected = "PM2.5"),
          leafletOutput("mapPlot"),
          actionButton("engage_button", "Get Involved in Projects")
        ),
        mainPanel(
          plotlyOutput("emissionPlot"),
          uiOutput("project_engagement"),
          h4("Explanation of Trends"),
          p("Fine Particulate Matter (PM2.5):"),
          p("Trend: PM2.5 levels often fluctuate based on industrial activities, vehicular emissions, and weather conditions. In some cases, PM2.5 might show spikes during weekdays due to increased traffic and industrial operations."),
          p("Example Analysis: On Mondays, higher PM2.5 levels might be observed due to increased traffic as people return to work and school."),
          p("Organic Carbon (OC):"),
          p("Trend: Organic Carbon levels are influenced by biomass burning, dust, and urban emissions. Trends might show variations based on seasonal agricultural practices or waste management activities."),
          p("Example Analysis: Higher OC levels could be noticed during dry seasons when agricultural burning is more common."),
          p("Black Carbon (BC):"),
          p("Trend: Black Carbon is closely linked to combustion sources such as diesel engines and industrial processes. BC levels might show peaks during periods of high traffic or industrial activity."),
          p("Example Analysis: A noticeable increase in BC levels might be observed during periods of high industrial activity or vehicular traffic.")
        )
      )
    ),
    
    tabPanel(
      "Calculator",
      fluidRow(
        column(6,
               div(class = "calculator-container",
                   h3("Carbon Footprint Calculator", class = "section-title"),
                   sliderInput("electricity", "Electricity Usage (kWh per month):", 
                               min = 0, max = 1000, value = 200, step = 10),
                   sliderInput("gas", "Gas Usage (therms per month):", 
                               min = 0, max = 500, value = 100, step = 10),
                   sliderInput("car_miles", "Miles Driven per Year:", 
                               min = 0, max = 20000, value = 12000, step = 100),
                   selectInput("diet", "Diet Type:", 
                               choices = c("Vegetarian", "Average", "High Meat"), selected = "Average"),
                   actionButton("calculate", "Calculate Footprint"),
                   verbatimTextOutput("carbon_output"),
                   h4("Carbon Credits Needed", class = "section-title"),
                   verbatimTextOutput("carbon_credits")
               )
        ),
        column(6,
               plotOutput("carbon_breakdown")
        )
      )
    ),
    
    tabPanel(
      "Documentation",
      fluidRow(
        column(12,
               h3("Documentation", class = "section-title"),
               p("Our documentation provides comprehensive information about the Carbon Vista platform, including details on how to use the features, technical specifications, and additional resources for understanding carbon emissions and sustainability efforts."),
               p("Click the button below to download the full documentation in PDF format. If you have any questions or need further assistance, please contact us."),
               downloadButton("download_docs", "Download Documentation")
        )
      )
    ),
    
    tabPanel(
      "Projects",
      fluidRow(
        column(4,
               img(src = "https://greenhomesystems.com/wp-content/uploads/2023/09/blog-cover-photo-98.jpg", height = "200px"),
               h3("Solar Energy"),
               p("Our Solar Energy projects focus on harnessing the power of the sun to generate clean, renewable electricity. Solar panels reduce reliance on fossil fuels and lower greenhouse gas emissions."),
               actionButton("join_solar", "Join Solar Project"),
               actionButton("contribute_solar", "Contribute to Solar Project")
        ),
        column(4,
               img(src = "https://www.neallandscapes.co.uk/wp-content/uploads/2021/04/planting-tree-1.jpg", height = "200px"),
               h3("Tree Plantation"),
               p("Our Tree Plantation initiatives aim to increase green cover and improve air quality. Trees absorb CO2 and provide numerous ecological benefits, including habitat for wildlife."),
               actionButton("join_tree", "Join Tree Project"),
               actionButton("contribute_tree", "Contribute to Tree Project")
        ),
        column(4,
               img(src = "https://news.mit.edu/sites/default/files/images/202212/MIT-Windy-Impact-01.jpg", height = "200px"),
               h3("Wind Power"),
               p("Wind Power projects involve using wind turbines to generate electricity. Wind is a clean, renewable energy source that reduces reliance on fossil fuels and helps combat climate change."),
               actionButton("join_wind", "Join Wind Project"),
               actionButton("contribute_wind", "Contribute to Wind Project")
        ),
        column(12,
               h3("Add New Project", class = "section-title"),
               textInput("project_name", "Project Name"),
               textAreaInput("project_description", "Project Description"),
               actionButton("submit_project", "Submit Project"),
               uiOutput("new_project_message")
        )
      )
    ),
    
    tabPanel(
      "Profile",
      fluidRow(
        column(6,
               h3("Register", class = "section-title"),
               textInput("register_name", "Name:"),
               textInput("register_email", "Email:"),
               passwordInput("register_password", "Password:"),
               passwordInput("confirm_password", "Confirm Password:"),
               actionButton("register_button", "Register"),
               uiOutput("registration_message")
        ),
        column(6,
               h3("Login", class = "section-title"),
               textInput("login_email", "Email:"),
               passwordInput("login_password", "Password:"),
               actionButton("login_button", "Login"),
               uiOutput("login_message")
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Map plot
  output$mapPlot <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addMarkers(lng = 36.8219, lat = -1.2921, popup = "Nairobi")  # Replace with actual data
  })
  
  # Emission plot
  output$emissionPlot <- renderPlotly({
    plot_ly(total_emissions, x = ~Week_Day, y = ~Total_PM25, type = 'bar', name = 'PM2.5') %>%
      add_trace(y = ~Total_OC, name = 'OC') %>%
      add_trace(y = ~Total_BC, name = 'BC') %>%
      layout(title = "Total Emissions by Pollutant",
             xaxis = list(title = "Week Day"),
             yaxis = list(title = "Total Emissions"))
  })
  
  # Project engagement
  output$project_engagement <- renderUI({
    req(input$engage_button)
    div(
      h4("Thank you for your interest in our projects!"),
      p("We'll be in touch with more details about how you can get involved.")
    )
  })
  
  observeEvent(input$calculate, {
    electricity_usage <- input$electricity
    gas_usage <- input$gas
    car_miles <- input$car_miles
    diet <- input$diet
    
    # Basic estimation of carbon footprint
    footprint <- (electricity_usage * 0.0005) + (gas_usage * 0.005) + (car_miles * 0.0003)
    
    # Determine the allowable emission based on user type
    allowable_emission <- switch(
      diet,
      "Vegetarian" = 0.4,
      "Average" = 3.7,
      "High Meat" = 6.5
    )
    
    # Calculate remaining emissions
    remaining_emissions <- max(footprint - allowable_emission, 0)
    
    # Calculate carbon credits needed
    credits_needed <- remaining_emissions
    
    # Display result
    output$carbon_output <- renderText({
      paste("Estimated Carbon Footprint: ", round(footprint, 2), " tons of CO2 per year")
    })
    
    output$carbon_credits <- renderText({
      if (remaining_emissions > 0) {
        paste("Carbon Credits Needed: ", round(credits_needed, 2), " tons of CO2 per year")
      } else {
        "No additional carbon credits needed."
      }
    })
    
    # Breakdown plot
    output$carbon_breakdown <- renderPlot({
      breakdown_data <- data.frame(
        Source = c("Electricity", "Gas", "Car Miles"),
        Emissions = c(electricity_usage * 0.0005, gas_usage * 0.005, car_miles * 0.0003)
      )
      ggplot(breakdown_data, aes(x = Source, y = Emissions, fill = Source)) +
        geom_bar(stat = "identity") +
        theme_minimal() +
        labs(title = "Carbon Footprint Breakdown", y = "Emissions (tons CO2)")
    })
  })
  
  # Integrate GPT-3 for additional interaction
  observeEvent(input$engage_button, {
    gpt_prompt <- "Provide engagement information for Carbon Vista's projects."
    gpt_response <- get_gpt_response(gpt_prompt)
    showModal(modalDialog(
      title = "GPT-3 Response",
      gpt_response,
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  # Download documentation
  output$download_docs <- downloadHandler(
    filename = function() { "Documentation.pdf" },
    content = function(file) {
      file.copy("path/to/your/documentation.pdf", file)
    }
  )
  
  observeEvent(input$submit_project, {
    # Code to save the new project (e.g., store in a database)
    output$new_project_message <- renderUI({
      modalDialog(
        title = "Project Submitted",
        "Your new project has been successfully submitted.",
        easyClose = TRUE,
        footer = NULL
      )
    })
  })
  
  observeEvent(input$register_button, {
    if (input$register_password != input$confirm_password) {
      output$registration_message <- renderUI({
        modalDialog(
          title = "Registration Error",
          "Passwords do not match. Please try again.",
          easyClose = TRUE,
          footer = NULL
        )
      })
    } else {
      # Registration logic (e.g., store in a database)
      output$registration_message <- renderUI({
        modalDialog(
          title = "Registration Success",
          "You have been successfully registered.",
          easyClose = TRUE,
          footer = NULL
        )
      })
    }
  })
  
  observeEvent(input$login_button, {
    # Login logic (e.g., validate credentials)
    output$login_message <- renderUI({
      modalDialog(
        title = "Login Success",
        "You have been successfully logged in.",
        easyClose = TRUE,
        footer = NULL
      )
    })
  })
  
  observeEvent(input$join_solar, {
    showModal(modalDialog(
      title = "Join Solar Energy Project",
      "Thank you for your interest in joining the Solar Energy project! We will contact you with further details.",
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  observeEvent(input$contribute_solar, {
    showModal(modalDialog(
      title = "Contribute to Solar Energy Project",
      "Thank you for your willingness to contribute to the Solar Energy project! We will provide you with more information on how you can contribute.",
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  observeEvent(input$join_tree, {
    showModal(modalDialog(
      title = "Join Tree Plantation Project",
      "Thank you for your interest in joining the Tree Plantation project! We will contact you with further details.",
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  observeEvent(input$contribute_tree, {
    showModal(modalDialog(
      title = "Contribute to Tree Plantation Project",
      "Thank you for your willingness to contribute to the Tree Plantation project! We will provide you with more information on how you can contribute.",
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  observeEvent(input$join_wind, {
    showModal(modalDialog(
      title = "Join Wind Power Project",
      "Thank you for your interest in joining the Wind Power project! We will contact you with further details.",
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  observeEvent(input$contribute_wind, {
    showModal(modalDialog(
      title = "Contribute to Wind Power Project",
      "Thank you for your willingness to contribute to the Wind Power project! We will provide you with more information on how you can contribute.",
      easyClose = TRUE,
      footer = NULL
    ))
  })
}

# Run the application 

shinyApp(ui = ui, server = server)