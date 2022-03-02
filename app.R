#Code to create ScotPHO's Shiny profile platform
# This script includes the user-interface definition of the app.
library(visNetwork)
library(png)
library(viridisLite)
library(viridis)
library(dplyr) 
library(tidyr)
library(readr)
library(scales) #rescaling variables
library(haven) #for SPPS file reading 
library(zoo) # dealing with dates
library(lubridate) #for automated list of dates in welcome modal
library(janitor) #cleaning names
library(gsheet) #for reading google sheets
library(rgdal) #for reading shapefiles
library(rgeos) #for reducing size of shapefiles
library(rmapshaper) #for reducing size of shapefiles
library(maptools) #for dissolving dzs shp into localities
library(shiny)
library(cartography)
library(rnaturalearth)
library(ggplot2) #data visualization
library (DT) # for data tables
library(leaflet) #javascript maps
library(plotly) #interactive graphs
library(shinyWidgets) # for extra widgets
library(tibble) # rownames to column in techdoc
library(shinycustomloader)#for valuebox on techdoc tab
library(sp)
library(lubridate) #for automated list of dates in welcome modal
library(shinycssloaders) #for loading icons, see line below
library(rmarkdown)
library(flextable) #for tech document table
library(webshot) #to download plotly charts
library(rintrojs)
library(questionr)
library(foreign)
library(dplyr) 
library(highcharter)
library(tibble)
library(dplyr)
library(highcharter)
library(billboarder)
library(DT)
library(lubridate)
library(tidyr)
library(shinyWidgets)
library(tidytext)
library(openxlsx)
library(officer)
library(flextable)
library(tm)  # ce package propose un ensemble de fonctions facilitant le traitement de donnees textuelles
library(readxl)
library(tidyr)
library(readr)
library(scales) #rescaling variables
library(haven) #for SPPS file reading
library(data.table) #reading data
library(zoo) # dealing with dates
library(shinydashboardPlus)
library(shinydashboard)

###############################################.
## Header ---- 
###############################################.

source("carouselPanel.R")


# Panel div for visualization
# override the currently broken definition in shinyLP version 1.1.0


appCSS <- "
#loading-content {
  position: absolute;
  background: #1ab7f0;
  opacity: 0.9;
  z-index: 100;
  left: 0;
  right: 0;
  height: 100%;
  text-align: center;
  color: #FFFFFF;
}
"

ui <-tagList( #needed for shinyjs
    useShinyjs(),
    inlineCSS(appCSS),
    div(
      id = "loading-content",
      h2("Loading...")
    ),# Include shinyjs
    introjsUI(),   # Required to enable introjs scripts
    navbarPage(id = "intabset",
               #needed for landing page
               title = div(tags$a(img(src="logo.png", height=40)),
                           style = "position: relative; top: -5px;"), # Navigation bar
               windowTitle = "BM", #title for browser tab
               theme =shinytheme("cerulean"), #Theme of the app (blue navbar)
              #tab panels collapse into menu in small screens
               header =         
                   tags$head( #CSS styles
                       tags$link(rel="shortcut icon",href="maternite.ico"), #Icon for browser tab
                       #Including Google analytics and Cookie control
                       includeScript("google-analytics.js"),
                       includeScript("bootstrap.min.js"),
                       includeScript("respond.min.js"),
                       includeScript("jquery.easing.1.3.js"),
                       # includeScript("cookie-control.js"),
                       includeCSS("www/style.css"),
                       includeCSS("www/paper.css"),
                       HTML("<base target='_blank'>") # to make external links open a new tab
                   ),
               
               tabPanel(
                   title = "Page d'accueil ", icon = icon("home"),
                   shinyjs::useShinyjs(),
                   tags$head(tags$script(HTML('
                                                        var fakeClick = function(tabName) {
                                                       var dropdownList = document.getElementsByTagName("a");
                                                       for (var i = 0; i < dropdownList.length; i++) {
                                                       var link = dropdownList[i];
                                                       if(link.getAttribute("data-value") == tabName) {
                                                       link.click();
                                                       };
                                                       }
                                                       };
                                                       '))),
                   
                   htmlOutput("page")),
              tabPanel("representation graphique",
                       leafletOutput("graphe1",  height=800),
                       absolutePanel(
                         id = "controls", class = "panel panel-default",
                         top = 75, left = 55, width = 250, fixed=TRUE,
                         draggable = TRUE, height = "auto"
                         
                       ))
               ),
               ###############################################.
               ## accueil page ----
               ###############################################.
               
    #Copyright warning
    tags$footer(column(6, "© Banque mondiale  2022"), 
                column(2, tags$a( tags$b("Contactez nous ©BM  !"), 
                                  class="externallink", style = "color: white; text-decoration: none")), 
                column(3, tags$a(tags$b("Privacy & cookies"), 
                                 class="externallink", style = "color: white; text-decoration: none")), 
                column(1, actionLink("twitter_share", label = "Share", icon = icon("twitter"),
                                     style= "color:white;")), 
                style = "
   position:fixed;
   text-align:center;
   left: 0;
   bottom:0;
   width:100%;
   z-index:1000;  
   height:30px; /* Height of the footer */
   color: white;
   padding: 10px;
   font-weight: bold;
   background-color:#1995dc;"
    ))   
################################################.
#code HTML pour la page d'accueil



ui_home <- function(){
  fluidPage(fluidRow(
                  HTML("
                                     
                                     <section id='fh5co-hero' class='no-js-fullheight' style='background-image: url(buness2.jpg);' data-next='yes'>
			<div class='fh5co-overlay'></div>
			<div class='container'>
				<div class='fh5co-intro no-js-fullheight'>
					<div class='fh5co-intro-text'>
						<!-- 
							INFO:
							Change the class to 'fh5co-right-position' or 'fh5co-center-position' to change the layout position
							Example:
							<div class='fh5co-right-position'>
						-->
						<div class='fh5co-center-position'>
							<h2 class='animate-box'>Bienvenu sur notre page</h2>
							<h3 class='animate-box'>Notre page vous presente un exemple d'application shiny portant sur des projets.</h3>
						</div>
					</div>
				</div>
			</div>
			<div class='fh5co-learn-more animate-box'>
				<a href='#' class='scroll-btn'>
                                <span class='text'>Explore more about us</span>
                                    <span class='arrow'><i class='icon-chevron-down'></i></span>
                                    </a>
                                    </div>
                                    </section>
                                     
                                    ")
                  
                ),
               
                HTML("	
                           
                           
                           <div class='row'>
					<div >
						<div class='col-md-4 col-sm-6 col-xs-12' >
						<ul >
						<br >
						
						</br >
						</ul>
							<h4 >Les domaines d'intervention de la banque</h4>
								<ul >
								<li class='animate-box'><a href='#'><i class='icon-twitter'></i></a></li>
                                <li class='animate-box'><a href='#'><i class='icon-dribbble'></i></a></li>
                                    <li class='animate-box'><a href='#'><i class='icon-globe'></i></a></li>
                                    </ul>
                                    </div>
                                    </div>
                                     "),
                
                mainPanel(width = 11, style="margin-left:4%; margin-right:4%",
                          #2nd row of boxes
                          fluidRow(
                            br(), #spacing
                            column(12,
                                   HTML("	
                           
                           <div class='row'>
					<div class='col-md-4 col-sm-6 col-xs-12'>
						<div class='fh5co-person'>	
							<figure class='animate-box'>
								<img src='echange.jpg' alt='Free HTML5 Bootstrap Template by FREEHTML5.co' class='img-responsive'>
							</figure>
							<h3 class='fh5co-name animate-box'>Amelioration finance</h3>
							<h4 class='fh5co-designation animate-box'>Projet economie banque  </h4>
							<p class='fh5co-bio animate-box'> projet qui regroupe les banques des pays du monde.En plus de cela on a le chiffre d'affaire de chacun des banques  .</p>
							<ul class='fh5co-social'>
								<li class='animate-box'><a href='#'><i class='icon-twitter'></i></a></li>
                                <li class='animate-box'><a href='#'><i class='icon-dribbble'></i></a></li>
                                    <li class='animate-box'><a href='#'><i class='icon-globe'></i></a></li>
                                    </ul>
                                    </div>
                                    </div>
                                     "),
                                   HTML("	
                           
                           <div class='row'>
					<div class='col-md-4 col-sm-6 col-xs-12'>
						<div class='fh5co-person'>	
							<figure class='animate-box'>
								<img src='agriculture.jpg' alt='Free HTML5 Bootstrap Template by FREEHTML5.co' class='img-responsive'>
							</figure>
							<h3 class='fh5co-name animate-box'>Agriculte</h3>
							<h4 class='fh5co-designation animate-box'>Projet economie agriculture</h4>
							<p class='fh5co-bio animate-box'>Le projet economie de l'agriculture regroupe tous les projet de l'agriculture financé par la banque mondiale.</p>
							<ul class='fh5co-social'>
								<li class='animate-box'><a href='#'><i class='icon-twitter'></i></a></li>
                                <li class='animate-box'><a href='#'><i class='icon-dribbble'></i></a></li>
                                    <li class='animate-box'><a href='#'><i class='icon-globe'></i></a></li>
                                    </ul>
                                    </div>
                                    </div>
                                     "),
                                   HTML("	
                           
                           <div class='row'>
					<div class='col-md-4 col-sm-6 col-xs-12'>
						<div class='fh5co-person'>	
							<figure class='animate-box'>
								<img src='elevage.jpg' alt='Free HTML5 Bootstrap Template by FREEHTML5.co' class='img-responsive'>
							</figure>
							<h3 class='fh5co-name animate-box'>Elevage</h3>
							<h4 class='fh5co-designation animate-box'>Projet economie elevage</h4>
							<p class='fh5co-bio animate-box'> projet economie de l'evalage regroupe tous les projet de l'elevage financé par la banque mondiale. </p>
							<ul class='fh5co-social'>
								<li class='animate-box'><a href='#'><i class='icon-twitter'></i></a></li>
                                <li class='animate-box'><a href='#'><i class='icon-dribbble'></i></a></li>
                                    <li class='animate-box'><a href='#'><i class='icon-globe'></i></a></li>
                                    </ul>
                                    </div>
                                    </div>
                                     ")),
                            fluidRow(HTML("	
                           
                           <div class='row'>
					<div class='col-md-4 col-sm-6 col-xs-12'>
						<div class='fh5co-person'>	
							<figure class='animate-box'>
								<img src='health.jpg' alt='Free HTML5 Bootstrap Template by FREEHTML5.co' class='img-responsive'>
							</figure>
							<h3 class='fh5co-name animate-box'>Santé</h3>
							<h4 class='fh5co-designation animate-box'>Projet economie sante </h4>
							<p class='fh5co-bio animate-box'>le projet economie sante regroupe les projet de la santé fiancé par la banque mondiale.</p>
							<ul class='fh5co-social'>
								<li class='animate-box'><a href='#'><i class='icon-twitter'></i></a></li>
                                <li class='animate-box'><a href='#'><i class='icon-dribbble'></i></a></li>
                                    <li class='animate-box'><a href='#'><i class='icon-globe'></i></a></li>
                                    </ul>
                                    </div>
                                    </div>
                                     "),
                                     HTML("	
                           
                           <div class='row'>
					<div class='col-md-4 col-sm-6 col-xs-12'>
						<div class='fh5co-person'>	
							<figure class='animate-box'>
								<img src='education.jpg' alt='Free HTML5 Bootstrap Template by FREEHTML5.co' class='img-responsive'>
							</figure>
							<h3 class='fh5co-name animate-box'>Education </h3>
							<h4 class='fh5co-designation animate-box'>Projet economie education </h4>
							<p class='fh5co-bio animate-box'>le projet economie de l'education regroupe les projet de l'éducation financé par la banque mondiale.</p>
							<ul class='fh5co-social'>
								<li class='animate-box'><a href='#'><i class='icon-twitter'></i></a></li>
                                <li class='animate-box'><a href='#'><i class='icon-dribbble'></i></a></li>
                                    <li class='animate-box'><a href='#'><i class='icon-globe'></i></a></li>
                                    </ul>
                                    </div>
                                    </div>
                                     ")),
                            
                                                        
                            fluidRow(
                              HTML("<div id='fh5co-info'>
                                     <div class='container'>
                                     <div class='row'>
                                     <div class='col-md-6'>
                                     <figure class='animate-box'>
                                     <img src='buness1.jpg' alt='Free HTML5 Bootstrap Template by FREEHTML5.co' class='img-responsive'>
                                     </figure>
                                     </div>
                                     <div class='col-md-6'>
                                     <div class='fh5co-label animate-box'>Plus d'information sur economie </div>
                                     <h2 class='fh5co-lead animate-box'> Projet de la banque mondiale</h2>
                                     <p class='fh5co-sub-lead animate-box'>   la banque mondiale intervient sur plusieurs projet économique tel que les banque, les financement des projet de l'agriculture, santé , elevage et la sante. dans ce cadre notre application representatera les projet sur une carte du monde . </p>
                                     </div>
                                     </div>
                                     </div>
                                     </div>
                                 "))),#introBox 3 close
                          #Inequalities box
                          
                          
                          
                          
                          fluidRow(
                            HTML("
                                 <section id='fh5co-subscribe'>
                                     <div class='container'>
                                     <p> Terrains d'intervention </p>
                                     <figure class='animate-box'>
                                     <img src='world.jpg' alt='Free HTML5 Bootstrap Template by FREEHTML5.co' class='img-responsive'>
                                     </figure> 
                                     </div>
                                     </section>
                                         ")
                          ) # fluid row close
                ),
                
                br(),
                br()
  )
  ###############################################.
  ## accueil page ----
  ###############################################.
  
  #Copyright warning
  
}




server <- function(input, output) {
  world_data <- read_delim("world.csv", 
                           ";", escape_double = FALSE, trim_ws = TRUE)
  
  Sys.sleep(1)
  
  # Hide the loading message when the rest of the server function has executed
  hide(id = "loading-content", anim = TRUE, animType = "fade")    
  show("app-content")
  observe({
    
    output$page <- renderUI({
      div(class="outer",do.call(fluidPage,c("",ui_home())))
    })
    
    
    
  })
  observe({
    
    output$page1 <- renderUI({
      div(class="outer",do.call(dashboardPage,c("",ui_home1())))
    })
    
    
    
  })
  
  #utilisation de donnees fictifs sur une carte 
  output$graphe1<-renderLeaflet({
      pal <- colorNumeric(c("#fa3932", "#fffc40", "#3ccf25"), domain = 0:70
      )
      # Pass the palette function a data vector to get the corresponding colors
      pal(c(1,20,40))
      
      leaflet(world_data) %>%
        addTiles() %>%
        addProviderTiles("Esri.WorldStreetMap")%>%
        clearControls()%>%
        clearShapes()%>%# Add default OpenStreetMap map tiles
        addCircleMarkers(lng=world_data$long, lat=world_data$lat,clusterOptions = markerClusterOptions(),weight = 1,  stroke = TRUE,fillColor = ~pal(10),color = "#ffffff", fillOpacity = 0.8, radius =~7 )
        
      
      
    })
  
}


# Run the application 
shinyApp(ui = ui, server = server)
