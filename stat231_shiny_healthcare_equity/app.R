# Load necessary packages
library(showtext)
library(shiny)
library(shinythemes)
library(tidyverse)
library(ggrepel)
library(dplyr)
library(bslib)
library(thematic)
library(maps)
library(shinyWidgets)

# Import data
# Data sets for map
df <- read_csv("df.csv")
df2 <- read_csv("df2.csv")
socioeconomic <- read_csv("socioeconomic.csv")
# Data sets for 1st barplot
distress_sex <- read_csv("distress_sex.csv")
distress_race <- read_csv("distress_race.csv")
distress_income <- read_csv("distress_income.csv")
distress_food <- read_csv("distress_food.csv")
distress_edu <- read_csv("distress_edu.csv")
distress_citizen <- read_csv("distress_citizen.csv")
# Data set for 2nd barplot
distress2 <- read_csv("distress2.csv")
#Data set for table
mental <- read_csv("mental.csv")
#Data set for tobacco tab
tobacco_freq <- read_csv("tobacco_freq.csv")

# For TAB 1 INTERACTIVE CHOROPLETH MAP widgets:

## For selectInput
demo_choice_values <- c("median_income", "insecure_prop", "non_citizen_prop", 
                        "uninsured_prop", "unafford_prop")
demo_choice_names <- c("Median Household Income", "Food Insecurity", 
                       "Citizenship Status", 
                       "Health Insurance Coverage Status",
                       "Mental Health Care Affordability")
names(demo_choice_values) <- demo_choice_names

## For selectizeInput choices for US states
region_choices <- unique(socioeconomic$region)


# For TAB 2 BARPLOT widgets:

## For selectInput
demo2_choice_values <- c("sex_orientation", "race", "edu", 
                         "food_security", "income_group", "citizen_years")
demo2_choice_names <- c("Sexual Orientation", "Race", 
                        "Education Level", "Food Security", "Income Group", 
                        "Citizenship")
names(demo2_choice_values) <- demo2_choice_names


#For checkboxGroup Input choices
distress_choice_values <- c("anxiety", "depression")
distress_choice_names <- c("Anxiety", "Depression")
names(distress_choice_values) <- distress_choice_names

#For Sexual Orientation
sex_choices <- unique(distress2$sex_orientation)

#For Race
race_choices <- unique(distress2$race)

#For Education level
edu_choices <- unique(distress2$edu)

#For Food Security
food_choices <- unique(distress2$food_security)

#For Income Group
income_choices <- unique(distress2$income_group)

citizen_choices <- unique(distress2$citizen_years)

# For TAB 3 TABLE widgets: 
uaf_choices <- unique(mental$Affordability)
race_values <- unique(mental$Race)
coverage_choices <- unique(mental$Coverage)


## For TAB4 TOBACCO widgets:
tobacco_choices <- unique(tobacco_freq$Habit)
freq_choices <- unique(tobacco_freq$Frequency)

############
#    ui    #
############
ui <-navbarPage(
  
  title = "Health Care Equity",
  theme = bs_theme(bootswatch = "simplex", 
                   base_font = font_google("Source Sans Pro"),
                   code_font = font_google("Source Sans Pro")),
  
  # Tab 1: Interactive Choropleth Map
  tabPanel(
    title = "Socioeconomic Status Map",
    
    sidebarLayout(
      
      sidebarPanel(
        div("Socioeconomic Status", style = "font-size:17px"),
        selectInput(inputId = "demo_var",
                    label = "Choose a variable of interest",
                    choices = demo_choice_values,
                    selected = "median_income"),
        div("Region", style = "font-size:17px"),
        selectizeInput(inputId = "region",
                       label = "Identify region(s) on the map",
                       choices = region_choices,
                       selected = "northeast",
                       multiple = TRUE)
      ),
      
      mainPanel(plotOutput(outputId = "map"))
    )
  ),
  
  # Tab 2: Barplot
  tabPanel(
    title = "Psychological Distress",
    fluidRow(
      column(2,
             div("Socioeconomic Status", style = "font-size:17px"),
             selectInput(inputId = "demo2_var",
                         label = "Choose a variable of interest:",
                         choices = demo2_choice_values,
                         selected = "sex_orientation"),
             div("Types of Psychological Distress", style = "font-size:17px"),
             prettyCheckboxGroup(inputId = "distress_var",
                                 label = "Choose type(s) of distress:",
                                 choices = distress_choice_values,
                                 selected = c("anxiety", "depression"), 
                                 outline = TRUE,
                                 icon = icon("check"))),
      column(10,
             plotOutput(outputId = "barplot")
      )
      
    ),
    hr(),
    fluidRow(
      column(2,
             div("Sexual Orientation", style = "font-size:16px"),
             selectInput(inputId = "sex_var",
                         label = "Choose a sexual orientation:",
                         choices = sex_choices,
                         selected = "Straight")
      ),
      column(2,
             div("Race", style = "font-size:16px"),
             selectInput(inputId = "race_var",
                         label = "Choose a race:",
                         choices = race_choices,
                         selected = "White")
      ),
      column(2,
             div("Education Level", style = "font-size:16px"),
             selectInput(inputId = "edu_var",
                         label = "Choose an education level:",
                         choices = edu_choices,
                         selected = "Bachelor's degree")
      ),
      column(2,
             div("Food Security", style = "font-size:16px"),
             selectInput(inputId = "food_var",
                         label = "Choose a food security level:",
                         choices = food_choices,
                         selected = "Very low food security")
      ),
      column(2,
             div("Income Group", style = "font-size:16px"),
             selectInput(inputId = "income_var",
                         label = "Choose an income group:",
                         choices = income_choices,
                         selected = "$50,000 to $74,999")
      ),
      column(2,
             div("Citizenship Status", style = "font-size:16px"),
             selectInput(inputId = "citizen_var",
                         label = "Choose an citizenship status:",
                         choices = citizen_choices,
                         selected = "US Citizen")
      )
    ),
    plotOutput(outputId = "barplot2")
  ),
  
  # Tab 3: Interactive Table
  tabPanel(
    title = "Mental Health Care Equity",
    sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "Affordability",
                       label = "Choose Mental Health Care Affordability Status:",
                       choices = uaf_choices,
                       selected = "No"),
        selectInput(inputId = "Coverage",
                    label = "Choose Health Care Coverage Status:",
                    choices = coverage_choices,
                    selected = "Insured"),
        radioButtons(inputId = "Race",
                     label = "Select Race:",
                     choices = race_values),
        
      ),
      
      mainPanel(DT::dataTableOutput(outputId = "table"),
      )
    )
  ),
  
  tabPanel(
    title = "Tobacco",
    
    sidebarLayout(
      sidebarPanel(
        selectizeInput(inputId = "select_frequency",
                       label = "Choose a frequency:",
                       choices = freq_choices,
                       selected = freq_choices[1],
                       multiple = FALSE),
        
        selectizeInput(inputId = "select_habit",
                       label = "Choose type(s) of habit:",
                       choices = tobacco_choices,
                       selected = tobacco_choices,
                       multiple = TRUE)
      ),
      
      
      mainPanel(plotOutput(outputId = "hist"))
    )
  )
  
)



############
# server   #
############

server <- function(input, output){
  
  # TAB 1: INTERACTIVE CHOROPLETH MAP
  output$map <- renderPlot({
    
    if(input$demo_var == "unafford_prop"){
      subtitle0 <- "Proportion of Population Unable to Afford Mental Health Care"
    } else if(input$demo_var == "uninsured_prop"){
      subtitle0 <- "Proportion of Population Not Covered by Health Insurance"
    } else if(input$demo_var == "non_citizen_prop"){
      subtitle0 <- "Proportion of Non-US Citizens Who Lived in the US Over 10 Years  "
    }
    else{
      subtitle0 <- demo_choice_names[demo_choice_values == input$demo_var]
    }
    
    ggplot(df, aes(x = x, y = y)) + 
      geom_polygon(data = socioeconomic, aes_string(x="long", y="lat", group="group", fill = input$demo_var),
                   color="black", size = 0.2) +
      scale_fill_continuous(name = demo_choice_names[demo_choice_values == input$demo_var],
                            low ="#feedde", high = "#a63603") +
      labs(title = "Socioeconomic Status by Region", 
           subtitle = subtitle0) +
      theme(plot.title = element_text(size = 20),
            plot.subtitle = element_text(size = 16),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.line = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            axis.text = element_blank(),
            legend.position = "bottom",
            legend.box = "horizontal",
            legend.title = element_text(size = 13),
            legend.text = element_text(angle = 45, size = 11),
            legend.justification = c(1,0)) +
      guides(fill = guide_colorbar(title.position = "top")) +
      geom_label_repel(data = filter(df2, region %in% input$region),
                      aes(label = region))
    
  },
  width = 550,
  height = 450)
  
  
  # TAB 2: BARPLOT
  data_for_barplot <- reactive({
    if(input$demo2_var == "sex_orientation"){
      data <- filter(distress_sex, type %in% input$distress_var)
    } else if(input$demo2_var == "race"){
      data <- filter(distress_race, type %in% input$distress_var)
    } else if(input$demo2_var == "income_group"){
      data <- filter(distress_income, type %in% input$distress_var)
    } else if(input$demo2_var == "food_security"){
      data <- filter(distress_food, type %in% input$distress_var)
    } else if(input$demo2_var == "edu") {
      data <-filter(distress_edu, type %in% input$distress_var)
    } else{
      data <-filter(distress_citizen, type %in% input$distress_var)
    }
  })
  
  data_for_barplot2 <- reactiveValues(data = NULL)
  observeEvent(input$income_var,{
    data_for_barplot2$data <- filter(distress2, income_group %in% input$income_var)
  })
  observeEvent(input$food_var,{
    data_for_barplot2$data <- filter(distress2, food_security %in% input$food_var)
  })
  observeEvent(input$race_var,{
    data_for_barplot2$data <- filter(distress2, race %in% input$race_var)
  })
  observeEvent(input$edu_var,{
    data_for_barplot2$data <- filter(distress2, edu %in% input$edu_var)
  })
  observeEvent(input$citizen_var,{
    data_for_barplot2$data <- filter(distress2, citizen_years %in% input$citizen_var)
  })
  observeEvent(input$sex_var,{
    data_for_barplot2$data <- filter(distress2, sex_orientation %in% input$sex_var)
  })
  
  
  labels <- reactiveValues(l = NULL)
  observeEvent(input$income_var,{
    labels$l <- input$income_var
  })
  observeEvent(input$food_var,{
    labels$l <- input$food_var
  })
  observeEvent(input$race_var,{
    labels$l <- input$race_var
  })
  observeEvent(input$edu_var,{
    labels$l <- input$edu_var
  })
  observeEvent(input$citizen_var,{
    labels$l <- input$citizen_var
  })
  observeEvent(input$sex_var,{
    labels$l <- input$sex_var
  })
  
  output$barplot <- renderPlot({
    ggplot(data = data_for_barplot(), aes_string(x = input$demo2_var, y = "prop",
                                                 fill = "type")) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(name = "Types of Distress",
                        values = c("anxiety" = "#304C5A", "depression" = "#D4AD67")) +
      labs(title = paste0("Proportion of Adults with Psychological Distress\nby ", 
                          demo2_choice_names[demo2_choice_values == input$demo2_var]),
           x = demo2_choice_names[demo2_choice_values == input$demo2_var],
           y = "Proportion") +
      theme(plot.title = element_text(size = 20),
            axis.text = element_text(size = 14),
            axis.title = element_text(size = 15),
            axis.title.x = element_text(margin = margin(t=0)),
            legend.title = element_text(size=12),
            legend.text = element_text(size=10),
            panel.grid.major = element_line(color = "light grey"),
            panel.grid.minor = element_line(color = "light grey"),
            panel.background = element_rect(fill = "white")) +
      coord_flip() 
  })
  
  output$barplot2 <- renderPlot({
    ggplot(data = data_for_barplot2$data, aes_string(x = "degree", fill = "degree")) +
      geom_bar() +
      facet_wrap(~type) + 
      scale_fill_manual(values = c("Mild" = "#d88963", "Moderate" = "#d35643",
                                   "Severe" = "#a53535", "Not Ascertained" = "#e0c9a4")) +
      labs(title = paste0("Psychological Distress Experienced by ", labels$l),
           x = "Degree",
           y = "Number of Respondents") +
      theme(legend.position = "None",
            plot.title = element_text(size = 20),
            axis.title = element_text(size = 15),
            axis.text = element_text(size = 14),
            strip.text = element_text(size = 16),
            panel.grid.major = element_line(color = "light grey"),
            panel.grid.minor = element_line(color = "light grey"),
            panel.background = element_rect(fill = "white"))
  })
  
  #TAB 3: Table
  data_for_table <- reactive({
    data <- filter(mental, Affordability %in% input$Affordability, 
                   Race %in% input$Race,
                   Coverage %in% input$Coverage)
  })
  
  output$table <- DT::renderDataTable({ 
    data_for_table()
  })
  
  #TAB 4: Tobacco
  data_for_hist <- reactive({
    data <- tobacco_freq %>%
      filter(Frequency == input$select_frequency, Habit %in% input$select_habit) %>%
      group_by(afford_mental_care, Habit) %>%
      summarise(
        count = n()
      )
  })
  
  output$hist <- renderPlot({
    ggplot(data = data_for_hist(), aes(x = Habit, y = count, fill= afford_mental_care)) +
      geom_bar(stat = "identity", position = "dodge") + 
      labs(
        title = "Methods of Tobacco Consumption as a Coping Mechanism for Mental Health")+
      ylab("Number of Respondents \n")+
      xlab ("\nMethods of Tobacco Consumption\n") +
      scale_fill_discrete(name = "Unable to Access Counseling/Therapy in the Past 12 Months Due to Cost") +
      theme(
        plot.title = element_text(size = 18),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 12),
        legend.title = element_text(size=12),
        legend.text = element_text(size=10),
        legend.position = "bottom",
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major.y = element_line(colour = "grey50"),)
    
  })
}

####################
# call to shinyApp #
####################
thematic_shiny(font = "auto")
shinyApp(ui = ui, server = server)