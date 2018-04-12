#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readr)
library(dplyr)
library(tidyr)
library(maps)
library(mapdata)
library(ggplot2)
library(RCurl)

path <- getURL("https://raw.githubusercontent.com/bryce-murphy/risk_cancer/master/risk_cancer.csv")

risk_cancer <- 
  read_csv(path) %>%
  rename(region = State)

risk_cancer$region <- tolower(risk_cancer$region)

states <- 
  data_frame(region = tolower(state.name), State = toupper(state.abb)) %>%
  bind_rows(data_frame(region = "district of columbia", State = "DC"))

risk_cancer_scaled <-
  risk_cancer %>%
  inner_join(states, by = "region") %>%
  mutate(z_risk_sleep = scale(inadequate_sleep),
         z_risk_drinking = scale(binge_drinking),
         z_risk_depression = scale(depression),
         z_rate_breast = scale(breast),
         z_rate_cervical = scale(cervical),
         z_rate_colon = scale(colon),
         z_rate_hpv = scale(hpv_associated),
         z_rate_lung = scale(lung),
         z_rate_ovarian = scale(ovarian),
         z_rate_prostate = scale(prostate),
         z_rate_skin = scale(skin),
         z_rate_uterine= scale(uterine)) %>%
  mutate_if(is.numeric, funs(round(., 2))) %>%
  select(region, State, starts_with("z_"))

country <- 
  map_data(map = "state") %>%
  inner_join(risk_cancer_scaled, by = "region") %>%
  select(State, region, group, order, long, lat, starts_with("z")) %>%
  gather(key = Cancer_Type, value = Cancer_Rate, -(1:9)) %>%
  gather(key = Risk_Type, value = Risk_Rate, -c(1:6, "Cancer_Type", "Cancer_Rate"))

# ADD STATE ABBREVIATIONS TO THE MAP

labels <- aggregate(cbind(long, lat) ~ region, data = country, 
                    FUN=function(x)mean(range(x)))

labels$State <- c("AL", "AR", "AZ", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "IA", 
                  "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME", "MI", "MN", 
                  "MO", "MS", "MT", "NC", "ND", "NE", "NH", "NJ", "NM", "NV", "NY", 
                  "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VA", 
                  "VT", "WA", "WV", "WI", "WY")


labels$group <- country$group[match(rownames(labels), country$group)]

labels <- 
  labels %>%
  rename(long_center = long, 
         lat_center = lat)

# DEFINE UI -----------------------------------------------------------------

ui <- fluidPage(
  
  # App Title
  titlePanel("Cancer and Risk Factor Prevalence"),
  
  # Sidebar 
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "cancer",
                  label = "Select Type of Cancer:",
                  choices = c("Breast" = "z_rate_breast", 
                              "Cervical" = "z_rate_cervical",
                              "Colon" = "z_rate_colon",
                              "HPV Associated" = "z_rate_hpv",
                              "Lung" = "z_rate_lung",
                              "Ovarian" = "z_rate_ovarian",
                              "Prostate" = "z_rate_prostate",
                              "Skin" = "z_rate_skin",
                              "Uterine" = "z_rate_uterine"),
                  selected = "Lung"),
      selectInput(inputId = "risk",
                  label = "Select Risk Factor:",
                  choices = c("Depression" = "z_risk_depression",
                              "Inadequate Sleep" = "z_risk_sleep",
                              "Binge Drinking" = "z_risk_drinking"))
    ),
    
    mainPanel(
      plotOutput("USA"),
      strong("The USA map is filled by the difference between the z score for the risk factor
             and the z score for the cancer type.
             Large negative or positive values of z.score.diff 
             indicate states where there is a discrepancy between 
             risk factor prevalence and cancer incidence rates 
             (low risk factor prevalence and high cancer incidence, or vice versa).")
      )
      )
      )

# DEFINE SERVER LOGIC ---------------------------------------------------------------

server <- function(input, output) {
  
  output$USA <- renderPlot({
    
    z_diff <- 
      country %>%
      filter(Cancer_Type == input$cancer, Risk_Type == input$risk) %>%
      mutate(z_score_diff = Risk_Rate - Cancer_Rate)
    
    ggplot(z_diff, aes(x = long, y = lat, group = group)) +
      geom_polygon(aes(fill = z_score_diff), color = "black") +
      geom_text(data = labels, aes(long_center, lat_center, label = State, group = group), size = 3) +
      coord_fixed(1.3) + 
      scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, name = "Discrepancy Level") +
      theme_classic() +
      theme(plot.title = element_text(hjust = 0.5))
    
    
    
  })
  
  
  
  
  
}





# Run the application 
shinyApp(ui = ui, server = server)



