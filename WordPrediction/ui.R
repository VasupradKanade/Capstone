##title       : Data Science Capstone - Shiny Application for Word Prediction
##subtitle    : Course 10 - Week 4 - Assignment
##author      : Vasuprad Kanade
##job         : Coursera Data Science Specialization
#
#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)

# Define UI for application that draws a histogram
# Define UI for application that plots random distributions
shinyUI(navbarPage
           (
              title = "Word Prediction",
              theme = shinytheme("cyborg"),
             
             
             # Sidebar with a slider input for number of observations
             tabPanel
               (
                        title = "Wait for 10 seconds for the app to load. Type your words in the input box ...",
                      
                      fluidRow
                       (
							column(3),
							column(6,
								   tags$head
								   (
									 tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
								   ),
								   tags$div
								   (
										 #HTML('<textarea width="60%" id="text" rows="3" cols="30" class="form-control"></textarea>'),
										 h3("Enter your text here"),
										 tags$textarea(id = 'text', placeholder = 'Type here', rows = 3, class='form-control',""),

										 HTML('<script type="text/javascript"> 
											  document.getElementById("text").focus();
											  </script>'),
										 
										 HTML("<div id='buttons'>"),
										 uiOutput("prediction1",inline = T),
										 uiOutput("prediction2",inline = T),
									 uiOutput("prediction3",inline = T)
									 ),
								   HTML("</div>"),align="center"),
							column(3)
                        ), ##fluidRow
						fluidRow
						(
						hr(),
						tags$span(
						  style="color:grey",  
								  tags$footer
									  ("@2017-",  
										tags$a( 
										href="http://vasupradkanade.github.com", 
										target="_blank", 
										"Vasuprad Kanade"),  
										align = "Center"
									  )
								  )
						) ##fluidRow
               ) ##tabPanel
            ) ##navbarPage
        ) ##shinyUI
