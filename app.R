library("dplyr")
library("shiny")
library("tidyverse")

## Load Data
product <- read.csv("product.csv")
purchase_header <- read.csv("purchase_header.csv")
purchase_lines <- read.csv("purchase_lines.csv")

## Data cleaning
# The product table appears to have repeating product IDs, but any repeats have the same values, so we just need to collect them all
product %>%
  group_by(PRODUCT_ID) %>%
  mutate(count = n()) %>%
  arrange(desc(count)) %>%
  filter(count >= 2) %>%
  summarise(height_SD = sd(HEIGHT_INCHES), width_SD = sd(WIDTH_INCHES), depth_SD = sd(DEPTH_INCHES), weight_SD = sd(WEIGHT_GRAMS)) %>%
  filter (height_SD > 0 | width_SD > 0 | depth_SD > 0 | weight_SD > 0)

unique_products <- product %>%
  group_by(PRODUCT_ID) %>%
  mutate(HEIGHT_INCHES = mean(HEIGHT_INCHES), WIDTH_INCHES = mean(WIDTH_INCHES), DEPTH_INCHES = mean(DEPTH_INCHES), WEIGHT_GRAMS = mean(WEIGHT_GRAMS), row = row_number()) %>%
  filter(row == 1) %>%
  select(-row)

purchase_header %>%
  arrange(PURCHASE_DATE_TIME)

# Now join with purchase lines and purchase header which seem like clean tables, and convert purchase date/time into a date/time variable
full_data <- purchase_lines %>%
  left_join(unique_products) %>%
  left_join(purchase_header) %>%
  mutate(PURCHASE_DATE_TIME = as.POSIXct(PURCHASE_DATE_TIME, format = "%m/%d/%Y"))

# I see height, width, depth, weight -- density formula is screaming at me
full_data <- full_data %>%
  mutate(density = WEIGHT_GRAMS  / (WIDTH_INCHES * HEIGHT_INCHES * DEPTH_INCHES))

ui <- fluidPage(
    titlePanel("Fulfil Data Exploration"),


    sidebarLayout(
        sidebarPanel(
          checkboxGroupInput("department", label = "Select Department:",
                             choices = sort(unique(full_data$DEPARTMENT_NAME)),
                             selected = "Alcohol"),
          radioButtons("meanorsum", label = "Mean Density or Sum Density:",
                       choices = c("Sum", "Mean"),
                       selected = "Sum")
        ),


        mainPanel(
           plotOutput("graph"),
           plotOutput("graph2")
        )
    )
)


server <- function(input, output) {

    output$graph <- renderPlot({
      full_data %>%
        group_by(PURCHASE_DATE_TIME, DEPARTMENT_NAME) %>%
        summarise(TOTAL_QUANTITY = sum(QUANTITY)) %>%
        filter(DEPARTMENT_NAME %in% input$department) %>%
        ggplot(mapping=aes(x=PURCHASE_DATE_TIME, y=TOTAL_QUANTITY, colour = DEPARTMENT_NAME)) +
        geom_point(stat='summary') +
        geom_line(aes(group=DEPARTMENT_NAME)) +
        xlab("Day of Purchase") +
        ylab("Total Quantity (Units Sold)") +
        labs(colour = "Department Name") +
        scale_x_datetime(date_breaks = "days", date_labels = "%B %d") +
        geom_text(aes(label=TOTAL_QUANTITY), nudge_y=100)
    })
  
    observe(
    {
      if (input$meanorsum == "Sum")
      {
      output$graph2 <- renderPlot({
        full_data %>%
          group_by(PURCHASE_DATE_TIME, DEPARTMENT_NAME) %>%
          filter(!is.na(density)) %>%
          summarise(TOTAL_DENSITY = sum(density)) %>%
          filter(DEPARTMENT_NAME %in% input$department) %>%
          ggplot(mapping=aes(x=PURCHASE_DATE_TIME, y=TOTAL_DENSITY, colour = DEPARTMENT_NAME)) +
          geom_point(stat='summary') +
          geom_line(aes(group=DEPARTMENT_NAME)) +
          xlab("Day of Purchase") +
          ylab("Total Density (g/in^3)") +
          labs(colour = "Department Name") +
          scale_x_datetime(date_breaks = "days", date_labels = "%B %d") +
          geom_text(aes(label=format(round(TOTAL_DENSITY, 2), nsmall = 2)), nudge_y=300)
      })
      }
      else
      {
        output$graph2 <- renderPlot({
          full_data %>%
            group_by(PURCHASE_DATE_TIME, DEPARTMENT_NAME) %>%
            filter(!is.na(density)) %>%
            summarise(MEAN_DENSITY = mean(density)) %>%
            filter(DEPARTMENT_NAME %in% input$department) %>%
            ggplot(mapping=aes(x=PURCHASE_DATE_TIME, y=MEAN_DENSITY, colour = DEPARTMENT_NAME)) +
            geom_point(stat='summary') +
            geom_line(aes(group=DEPARTMENT_NAME)) +
            xlab("Day of Purchase") +
            ylab("Mean Density (g/in^3)") +
            labs(colour = "Department Name") +
            scale_x_datetime(date_breaks = "days", date_labels = "%B %d") +
            geom_text(aes(label=format(round(MEAN_DENSITY, 2), nsmall = 2)), nudge_y=1)
        })
      }
    })
}

shinyApp(ui = ui, server = server)
