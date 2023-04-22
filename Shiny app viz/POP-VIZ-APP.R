library(shinydashboard)
library(shiny)
library(tidyverse)
library(DT)
library(readxl)
library(cowplot)
library(ggimage)
library(glue)
library(ggiraph)
library(ggpubr)
library(here)
library(scales)
library(highcharter)
library(fmsb)
f = function(x){
  res = x
  res[x < 1] = ""
  return(res)
}

all_data = read_rds("all_data.rds") %>% tibble()


countrylist = unique(all_data$country)%>% 
  data.frame(country = .)

classe_age = unique(all_data$Age)
  


header = dashboardHeader(title =  
                           tags$div(HTML('<i class="fa-solid fa-people-roof"></i> POP-VIZ'))
                         #span( icon("users"), "User Profile")
                         
  
)

sidebar = dashboardSidebar(
  
  
  sidebarMenu(
    menuItem(text =  "Présentation de l'application",
             tabName = "about", icon = icon("person-chalkboard")
             
    ),
    menuItem(
      text = "Reproduction et Amélioration",
      tabName = "reprod",icon = icon("people-arrows")
    ),
    menuItem(text = "Scrapping", tabName = "scrap", icon = icon("internet-explorer")), 
    menuItem(text = "Actualisation", tabName = "actu", icon = icon("arrows-rotate")),
    menuItem(
      text = "Comparaion de structure",
      tabName = "compar", icon = icon("not-equal")
    ),
    menuItem(
      text = "Visualisation globale",
      tabName =  "globalvis", icon = icon("map")
    ),
    menuItem(text = "Source code", icon = icon("laptop-code"))
    
  )
  
)

body = dashboardBody(
  tabItems(
    tabItem(tabName =  "about",
          column(width =  6,
            fluidRow(
              box(width = 12,
              title = "Objectifs", solidHeader = TRUE, status = "primary" ,
              p("A travers cette application on explore la première pyramide des âges historique de Francis Walker (1972). 
                On  traitera ce graphique sous trois angles différents"),
              tags$ul(
                tags$li("Un premier qui consiste à reporoduire simplement la pyramide historique"),
                tags$li("Un deuxième qui consite à une amélioration et une actualisation du graphique"),
                tags$li("Et pour finir proposer des alternatives de représentation de la dynamique de la population")
              )
            )),
            fluidRow(box(width = 12,
              title = "Première pyramide (1972)", solidHeader = TRUE, status = "primary",
              imageOutput("pyramid")
            ))),
          
            box(title = "Données", solidHeader = TRUE, status = "primary",
                p("Donnée recueilli directement sur le graphique"),
                  DTOutput("data")
            )
                   ),
    tabItem(tabName = "reprod", 
            column(6,
                  
              tabBox( width = 12,
                      title = "Reproduction",
                tabPanel("Reproductionn", plotOutput("pyramidreprod")),
                tabPanel("Code",verbatimTextOutput("code")))
              ,
               box(width =  12, title = "Affichage",
                  radioButtons("affich", "", 
                              choices = c("Native White", "colored", "Les deux"), 
                              selected = "Native White")))
            ,
            column(6,fluidRow(
              tabBox(width =  12,
                  title = "Première amélioration",
                  tabPanel("Statique", plotOutput("pyramidamelior")),
                  tabPanel("Interactive", ggiraphOutput("pyraminter")),
                  tabPanel("Code", verbatimTextOutput("code1")),
                  tabPanel("Original", imageOutput("pyram")), selected = "Original"
                  
              )),
              fluidRow( tabBox(width =  12, title = "Affichage",
                               tabPanel("Graphique", radioButtons("affich1", "", 
                                                                  choices = c("Native White", "colored", "Les deux"), 
                                                                  selected = "Native White")),
                               tabPanel("Code", radioButtons("affichcode", "", 
                                                             choices = c("Statique", "Interactive"), 
                                                             selected = "Statique"))
                           ))
            )
      
    ), 
    tabItem(
      tabName = "scrap",
      fluidRow(
      column(6,       verbatimTextOutput("scrappinng")), 
      column(6, DTOutput("alldata"))
    )),
    tabItem(
      tabName = "actu",
      fluidRow(
        column(3, selectInput("pays", "Pays", 
                    choices = countrylist$country, 
                    selected = countrylist$country[1])),
        column(3, sliderInput("year", "Year", min = 1960, max = 2015,value = 1960, animate = TRUE)),
        column(4, 
               checkboxGroupInput("age1", "Classe d'âge",
                                     choiceNames = (classe_age),
                                     choiceValues = 1:length(classe_age),
                                     inline = TRUE) 
               ),
        column(2, uiOutput("indicateurr")
        ),
      fluidRow(
        column(6,tabBox(id = "actu1", width = 12, 
                        tabPanel("Interactive",ggiraphOutput("graphactu") ),
                        tabPanel(title = "Statique", plotOutput("graphstatique") ),
                        tabPanel("Radar", plotOutput("radaar"))) ), 
        column(6,
               valueBoxOutput("Garçon"),
               valueBoxOutput("Fille"),
               valueBoxOutput("Total")
               ,
               plotOutput("evolution"))
      )
    )),
    tabItem(
      tabName = "compar",
      column(3,
             fluidRow(
               helpText("Choisir Si vous souhaitez comparer deux pays différents (même année) ou 
                        le deux années différentes(même pays)"),
               radioButtons("compartype", "Type de comparaison", 
                                choiceNames  = c("Deux pays différents", "Deux années différentes"),
                            choiceValues = c(1, 2),
                                selected = 2),
               uiOutput("morecontrol")
                   )
             ),
      column(9, 
            tabBox(width = 12,
              tabPanel("Superposé", plotOutput("super")),
              tabPanel("Non superposé", ggiraphOutput("comparpyram")),
              tabPanel("Radar", plotOutput("Radar"))
            )
            
             )
    ),
    tabItem(
      "globalvis",
      fluidRow(column(3, sliderInput("year3", "Year", min = 1960, max = 2015,value = 1960, animate = TRUE)), 
               column(3, radioButtons("sexxe", "Genre", choiceNames = c("Masculin", "Feminin", "Total"),
                                      choiceValues = c("Masculin", "Feminin", "Total"), 
                                      selected = "Total")),
               column(3, checkboxGroupInput("age2", "Classe d'âge",
                                  choiceNames = (classe_age),
                                  choiceValues = 1:length(classe_age),
                                  inline = TRUE)),
               column(3, uiOutput("indic2"))),
      fluidRow(highchartOutput("mapp"))
    )
  )
  
)

ui = dashboardPage(header, sidebar, body)

server = function(input, output){
  
  ##### Préambule ####
  Native_white <<- read_excel("Histoire/HISTOIRE.xlsx", sheet = "Native white")
  colored <- read_excel("Histoire/HISTOIRE.xlsx", sheet = "Colored") %>% as_tibble()
  Native_white = as_tibble(Native_white)
  Native_white <<- Native_white %>%
    mutate(Age = factor(paste(seq(0, 80, 10), seq(10, 90, 10), sep = "-" ), ordered = T),
           Skin = rep("Native White", 9))
  colored <<- colored %>%
    mutate(Age = factor(paste(seq(0, 80, 10), seq(10, 90, 10), sep = "-" ), ordered = T),
           Skin = rep("colored", 9))
  merge <- rbind(Native_white, colored)
  output$data = renderDT(
    merge %>% mutate(
      origine = rep(c("Native White", "Colored"), each = 9)
    ) %>% 
      pivot_longer(cols = c(M, `F`) ,names_to = "Sexe", values_to = "Effectif")
  )

  
  ##### Code graphique de reproduction #####
  output$code = renderText(
    glue(' NW <-  Native_white %>%
    ggplot(aes(x = 1:9)) +
    # Masculin
    geom_area(aes( y = -M), fill = "#503A22ff", colour = "black", size = 1) +
    geom_bar( aes(y = -M), stat = "identity", colour = "black", size = 1, width = 0 )+
    
    ## Féminin
    geom_area(aes( y = `F`), fill = "#E8B35Eff" , colour = "black", size = 1) +
    geom_bar( aes(y = `F`), stat = "identity",colour = "black", size = 1, width = 0 ) + 
    
    # Le thème 
    theme(
      plot.background = element_rect(fill = "#E8B35Eff"),
      panel.background = element_rect(fill = "#E8B35Eff"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      text = element_text(face = "bold", size = 15)) + 
    xlab("") + ylab("") +
    # Ajout des effectif
    ## Le ifelse pour afficher les 0 a gauche des nombre car 
    ## R les enlèves par défaut
    annotate(geom = "text", x = 1:9 + 0.1, y = -Native_white$M -10,
             label =ifelse(str_length(Native_white$M)==2, 
                           paste(0,Native_white$M,sep = ""),
                           ifelse(str_length(Native_white$M)==1, 
                                  paste("00",Native_white$M,sep = ""),Native_white$M)),
             fontface = "bold.italic") +
    
    annotate(geom = "text", x = 1:9 + 0.1, y = (Native_white$`F` + c(12,12,12,12,10,8,8,8,8)),
             label = ifelse(str_length(Native_white$`F`)==2, 
                            paste(0,Native_white$`F`,sep = ""),
                            ifelse(str_length(Native_white$`F`)==1, 
                                   paste("00",Native_white$`F`,sep = ""),Native_white$`F`)),
             fontface = "bold.italic") + 
    ylab("Native White") +
    geom_segment(aes(x = 1, xend = 9, y = 0, yend = 0), lwd = 1) +
    coord_flip()
  ')
  )

  
  ##### Reproduction ####  
  output$pyramid <- renderImage({
    
    list(src = "population_pyramid.jpg",
         width = "100%", height = 400)
    
  }, deleteFile = F)
  output$pyram <- renderImage({
    
    list(src = "population_pyramid.jpg",
         width = "100%", height = 400)
    
  }, deleteFile = F)
  
  
  
  NW <-  Native_white %>%
    ggplot(aes(x = 1:9)) +
    # Masculin
    geom_area(aes( y = -M), fill = "#503A22ff", colour = "black", size = 1) +
    geom_bar( aes(y = -M), stat = "identity", colour = "black", size = 1, width = 0 )+
    
    ## Féminin
    geom_area(aes( y = `F`), fill = "#E8B35Eff" , colour = "black", size = 1) +
    geom_bar( aes(y = `F`), stat = "identity",colour = "black", size = 1, width = 0 ) + 
    
    # Le thème 
    theme(
      plot.background = element_rect(fill = "#E8B35Eff"),
      panel.background = element_rect(fill = "#E8B35Eff"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      text = element_text(face = "bold", size = 15)) + 
    xlab("") + ylab("") +
    # Ajout des effectif
    ## Le ifelse pour afficher les 0 a gauche des nombre car 
    ## R les enlèves par défaut
    annotate(geom = "text", x = 1:9 + 0.1, y = -Native_white$M -10,
             label =ifelse(str_length(Native_white$M)==2, 
                           paste(0,Native_white$M,sep = ""),
                           ifelse(str_length(Native_white$M)==1, 
                                  paste("00",Native_white$M,sep = ""),Native_white$M)),
             fontface = "bold.italic") +
    
    annotate(geom = "text", x = 1:9 + 0.1, y = (Native_white$`F` + c(12,12,12,12,10,8,8,8,8)),
             label = ifelse(str_length(Native_white$`F`)==2, 
                            paste(0,Native_white$`F`,sep = ""),
                            ifelse(str_length(Native_white$`F`)==1, 
                                   paste("00",Native_white$`F`,sep = ""),Native_white$`F`)),
             fontface = "bold.italic") + 
    ylab("Native White") +
    geom_segment(aes(x = 1, xend = 9, y = 0, yend = 0), lwd = 1) +
    coord_flip()
  
  
  
  C <- colored %>%
    ggplot(aes(x = 1:9)) +
    
    geom_area(aes( y = -M), fill = "#E8B35Eff", colour = "black", size = 1) +
    geom_bar( aes(y = -M), stat = "identity", colour = "black", size = 1, width = 0 )+
    
    geom_area(aes( y = `F`), fill = "#503A22ff", colour = "black", size = 1) +
    geom_bar( aes(y = `F`), stat = "identity",colour = "black", size = 1, width = 0 ) + 
    
    theme(
      plot.background = element_rect(fill = "#E8B35Eff"),
      panel.background = element_rect(fill = "#E8B35Eff"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      text = element_text(face = "bold", size = 15)) + 
    xlab("") + ylab("") +
    
    annotate(geom = "text", x = 1:9 + 0.1, y = -colored$M -10, 
             label = ifelse(str_length(colored$M)==2, paste(0,colored$M,sep = ""),
                            ifelse(str_length(colored$M)==1, 
                                   paste("00",colored$M,sep = ""),colored$M)),
             fontface = "bold.italic") +
    ## on ajoute des valeur sur les coordonnées afin d'ajustées la position
    annotate(geom = "text", x = 1:9 + 0.1, y = (colored$`F` + c(12,12,12,12,10,8,8,8,8)), 
             label = ifelse(str_length(colored$`F`)==2, paste(0,colored$`F`,sep = ""),
                            ifelse(str_length(colored$`F`)==1, 
                                   paste("00",colored$`F`,sep = ""),colored$`F`)),
             fontface = ("bold.italic")) + 
    ylab("Colored") +
    
    geom_segment(aes(x = 1, xend = 9, y = 0, yend = 0), lwd = 1) +
    coord_flip()
  output$pyramidreprod <- renderPlot(
    if(input$affich == "Native White"){
      print(NW)
    }else if(input$affich == "colored"){
      print(C)
    }else{
      plot_grid(NW, C, nrow = 2, ncol = 1 )
    }
    
  )
  
  ##### Code amélioration statiques et interactives #####
  
  output$code1 <- renderText(
    if(input$affichcode == "Statique"){
    glue('
         
      ## Statique 
    position = data.frame(M = -Native_white$M - 5, `F` = Native_white$`F` +5)
 NWa <- Native_white%>%
    ggplot(aes(x = c(1:9))) +
    
    geom_bar( aes(y = -M), stat = "identity", width = 0.95, size = 3, fill ="#4682B4") +
    geom_bar( aes(y = `F`), stat = "identity", width = 0.95, size = 3, fill = "#C6116B") + 
    
    theme_light() +
    xlab("") + ylab("") +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = 1, 
                                   size = 0.65, linetype = "solid"),
          plot.title = element_text(face = "bold", hjust = 0.5, size = 13),
          plot.caption = element_text(face = "bold", hjust = 1, size = 12),
          plot.subtitle = element_text(face = "italic", hjust = 0, size = 13),
          panel.border = element_rect(colour = 1, 
                                      size = 0.65, linetype = "solid")) +
    
    scale_x_continuous(n.breaks = 9, labels = c("","0-10", "10-20", "20-30", "30-40",
                                                "40-50", "50-60", "60-70","70-80",
                                                "80-90","")) +
    scale_y_continuous(n.breaks = 8, labels = c(rev(seq(0,150,50)), seq(50,150,50))) + 
    coord_flip() +
    labs(title = "Nebraska population structure: Native White") +#Made by Françis Walker (1874)
    geom_image(aes(x = 8, y = -100), image = "MALEE.png", size = 0.1) +
    geom_image(aes(x = 8, y = 100), image = "FEMALEE.png", size = 0.1) +
    geom_label(aes(y = position$M , label = M), fill = "#4682B4", col = "white", fontface = "bold") +
    geom_label(aes(y = position$`F`, label = `F`), fill = "#C6116B", col = "white", fontface = "bold")

    ')
    }else{
      glue('
          ## Interactive
        ############# Interactivité ############
  age = c("0-10", "10-20", "20-30", "30-40",
          "40-50", "50-60", "60-70","70,80",
          "80-90")
  
  ## Un dataframe qui contient ce que doit afficher linteractivité
  
  tool = data.frame(
    label_M = paste("Masculin", "\n" ,"Age : ", age, "\n", "Effectif :", Native_white$M, sep = " "),
    label_F = paste("Féminin", "\n","Age : ", age, "\n", "Effectif :", Native_white$`F`, sep = " "))
  
  a = Native_white %>%
    ggplot(aes(x = c(1:9))) +
    ## Les fonction qui permettent de crée des barres interactives
    ## Largument tooltip prend laffichage de la bulle lorsque le curseur survole la barre
    geom_bar_interactive( aes(y = -M, data_id = M, tooltip = tool$label_M), stat = "identity", width = 0.95, size = 3, fill ="#4682B4") +
    geom_bar_interactive( aes(y = `F`, data_id = `F`, tooltip = tool$label_F), stat = "identity", width = 0.95, size = 3, fill = "#C6116B") + 
    
    theme_light() +
    xlab("") + ylab("") +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(colour = 1, 
                               size = 0.65, linetype = "solid"),
      plot.title = element_text(face = "bold", hjust = 0.5, size = 13),
      plot.caption = element_text(face = "bold", hjust = 1, size = 12),
      panel.border = element_rect(colour = 1, 
                                  size = 0.65, linetype = "solid")) +
    
    scale_x_continuous(n.breaks = 9, labels = c("","0-10", "10-20", "20-30", "30-40",
                                                "40-50", "50-60", "60-70","70,80",
                                                "80-90","")) +
    scale_y_continuous(n.breaks = 8, labels = c(rev(seq(0,150,50)), seq(50,150,50))) + 
    coord_flip() +
    
    labs(title = "Nebraska population structure: Native White") +
    
    geom_image(aes(x = 8, y = -100), image = "MALEE.png", size = 0.1) +
    geom_image(aes(x = 8, y = 100), image = "FEMALEE.png", size = 0.1)
  
 
  ggiraph(code = print(a),hover_css = "cursor:pointer;fill:skyblue;stroke:skyblue;stroke-width:2pt;" ,
              tooltip_extra_css = "background-color:skyblue;color:white;font-style:italic;padding:10px;border-radius:10px 20px 10px 20px;",
              tooltip_opacity = 1 )

      ')
    }
 
  )
  
  
  ##### Amélioration #####
position = data.frame(M = -Native_white$M - 5, `F` = Native_white$`F` +5)
 NWa <- Native_white%>%
    ggplot(aes(x = c(1:9))) +
    
    geom_bar( aes(y = -M), stat = "identity", width = 0.95, size = 3, fill ="#4682B4") +
    geom_bar( aes(y = `F`), stat = "identity", width = 0.95, size = 3, fill = "#C6116B") + 
    
    theme_light() +
    xlab("") + ylab("") +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = 1, 
                                   size = 0.65, linetype = "solid"),
          plot.title = element_text(face = "bold", hjust = 0.5, size = 13),
          plot.caption = element_text(face = "bold", hjust = 1, size = 12),
          plot.subtitle = element_text(face = "italic", hjust = 0, size = 13),
          panel.border = element_rect(colour = 1, 
                                      size = 0.65, linetype = "solid")) +
    
    scale_x_continuous(n.breaks = 9, labels = c("","0-10", "10-20", "20-30", "30-40",
                                                "40-50", "50-60", "60-70","70-80",
                                                "80-90","")) +
    scale_y_continuous(n.breaks = 8, labels = c(rev(seq(0,150,50)), seq(50,150,50))) + 
    coord_flip() +
    labs(title = "Nebraska population structure: Native White") +#Made by Françis Walker (1874)
    geom_image(aes(x = 8, y = -100), image = "MALEE.png", size = 0.1) +
    geom_image(aes(x = 8, y = 100), image = "FEMALEE.png", size = 0.1) +
    geom_label(aes(y = position$M , label = M), fill = "#4682B4", col = "white", fontface = "bold") +
    geom_label(aes(y = position$`F`, label = `F`), fill = "#C6116B", col = "white", fontface = "bold")
  
  positionc = data.frame(M = -colored$M - 5, `F` = colored$`F` +5)
 Ca <-  colored%>%
    ggplot(aes(x = c(1:9))) +
    
    geom_bar( aes(y = -M), stat = "identity", width = 0.95, size = 3, fill ="#4682B4") +
    geom_bar( aes(y = `F`), stat = "identity", width = 0.95, size = 3, fill = "#C6116B") + 
    
    theme_light() +
    xlab("") + ylab("") +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = 1, 
                                   size = 0.65, linetype = "solid"),
          plot.title = element_text(face = "bold", hjust = 0.5, size = 13),
          plot.caption = element_text(face = "bold", hjust = 1, size = 12),
          plot.subtitle = element_text(face = "italic", hjust = 0, size = 13),
          panel.border = element_rect(colour = 1, 
                                      size = 0.65, linetype = "solid")) +
    
    scale_x_continuous(n.breaks = 9, labels = c("","0-10", "10-20", "20-30", "30-40",
                                                "40-50", "50-60", "60-70","70-80",
                                                "80-90","")) +
    scale_y_continuous(n.breaks = 8, labels = c(rev(seq(0,150,50)), seq(50,150,50))) + 
    coord_flip() +
    labs(title = "Nebraska population structure: Colored") +#Made by Françis Walker (1874)
    geom_image(aes(x = 8, y = -100), image = "MALEE.png", size = 0.1) +
    geom_image(aes(x = 8, y = 100), image = "FEMALEE.png", size = 0.1) +
    geom_label(aes(y = positionc$M , label = M), fill = "#4682B4", col = "white", fontface = "bold") +
    geom_label(aes(y = positionc$`F`, label = `F`), fill = "#C6116B", col = "white", fontface = "bold")
  
  output$pyramidamelior <- renderPlot(
  if(input$affich1 == "Native White"){
    print(NWa)
  }else if(input$affich1 == "colored"){
    print(Ca)
  }else{
    plot_grid(NWa, Ca, nrow = 2, ncol = 1 )
  }
  )

  
  ############# Interactivité ############
  age = c("0-10", "10-20", "20-30", "30-40",
          "40-50", "50-60", "60-70","70,80",
          "80-90")
  
  ## Un dataframe qui contient ce que doit afficher l'interactivité
  
    tool = data.frame(
      label_M = paste("Masculin", "\n" ,"Age : ", age, "\n", "Effectif :", Native_white$M, sep = " "),
      label_F = paste("Féminin", "\n","Age : ", age, "\n", "Effectif :", Native_white$`F`, sep = " "))
    
    a = Native_white %>%
      ggplot(aes(x = c(1:9))) +
      ## Les fonction qui permettent de crée des barres interactives
      ## L'argument tooltip prend l'affichage de la bulle lorsque le curseur survole la barre
      geom_bar_interactive( aes(y = -M, data_id = M, tooltip = tool$label_M), stat = "identity", width = 0.95, size = 3, fill ="#4682B4") +
      geom_bar_interactive( aes(y = `F`, data_id = `F`, tooltip = tool$label_F), stat = "identity", width = 0.95, size = 3, fill = "#C6116B") + 
      
      theme_light() +
      xlab("") + ylab("") +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = 1, 
                                 size = 0.65, linetype = "solid"),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 13),
        plot.caption = element_text(face = "bold", hjust = 1, size = 12),
        panel.border = element_rect(colour = 1, 
                                    size = 0.65, linetype = "solid")) +
      
      scale_x_continuous(n.breaks = 9, labels = c("","0-10", "10-20", "20-30", "30-40",
                                                  "40-50", "50-60", "60-70","70,80",
                                                  "80-90","")) +
      scale_y_continuous(n.breaks = 8, labels = c(rev(seq(0,150,50)), seq(50,150,50))) + 
      coord_flip() +
      
      labs(title = "Nebraska population structure: Native White") +
      
      geom_image(aes(x = 8, y = -100), image = "MALEE.png", size = 0.1) +
      geom_image(aes(x = 8, y = 100), image = "FEMALEE.png", size = 0.1)
    
    
    toolb = data.frame(
      label_M = paste("Masculin", "\n" ,"Age : ", age, "\n", "Effectif :", colored$M, sep = " "),
      label_F = paste("Féminin", "\n","Age : ", age, "\n", "Effectif :", colored$`F`, sep = " "))
    
    b = colored %>%
      ggplot(aes(x = c(1:9))) +
      ## Les fonction qui permettent de crée des barres interactives
      ## L'argument tooltip prend l'affichage de la bulle lorsque le curseur survole la barre
      geom_bar_interactive( aes(y = -M, data_id = M, tooltip = toolb$label_M), stat = "identity", width = 0.95, size = 3, fill ="#4682B4") +
      geom_bar_interactive( aes(y = `F`, data_id = `F`, tooltip = toolb$label_F), stat = "identity", width = 0.95, size = 3, fill = "#C6116B") + 
      
      theme_light() +
      xlab("") + ylab("") +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = 1, 
                                 size = 0.65, linetype = "solid"),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 13),
        plot.caption = element_text(face = "bold", hjust = 1, size = 12),
        panel.border = element_rect(colour = 1, 
                                    size = 0.65, linetype = "solid")) +
      
      scale_x_continuous(n.breaks = 9, labels = c("","0-10", "10-20", "20-30", "30-40",
                                                  "40-50", "50-60", "60-70","70,80",
                                                  "80-90","")) +
      scale_y_continuous(n.breaks = 8, labels = c(rev(seq(0,150,50)), seq(50,150,50))) + 
      coord_flip() +
      
      labs(title = "Nebraska population structure: Colored", caption = "Made by OUOROU Rachidou ") +
      
      geom_image(aes(x = 8, y = -100), image = "MALEE.png", size = 0.1) +
      geom_image(aes(x = 8, y = 100), image = "FEMALEE.png", size = 0.1)
    
  
  
  output$pyraminter <- renderggiraph(
    
    if(input$affich1 == "Native White"){
      ggiraph(code = print(a),hover_css = "cursor:pointer;fill:skyblue;stroke:skyblue;stroke-width:2pt;" ,
              tooltip_extra_css = "background-color:skyblue;color:white;font-style:italic;padding:10px;border-radius:10px 20px 10px 20px;",
              tooltip_opacity = 1 )
    }else{
      ggiraph(code = print(b),hover_css = "cursor:pointer;fill:skyblue;stroke:skyblue;stroke-width:2pt;" ,
              tooltip_extra_css = "background-color:skyblue;color:white;font-style:italic;padding:10px;border-radius:10px 20px 10px 20px;",
              tooltip_opacity = 1 )
    }
    
    
  )
  

  ##### Scrapping ####
  
  output$alldata = renderDT({
    all_data %>% 
      mutate(propM = round(propM, 5), 
             propF = round(propF, 5))
  })
  output$scrappinng = renderText({
    
      '
library(RCurl)
library(XML)
library(rvest)
library(tidyverse)

url = "https://www.populationpyramid.net/afghanistan/2019/"
read_html(url) %>% 
  html_nodes("#countryDropdown") %>% 
  html_text() %>% 
  str_split(., "\n") %>% unlist()-> a
a[str_length(a) >0 ] -> countrylist

read_html(url) %>% 
  html_nodes("#pp-csv-link") %>%       # find all links
html_attrs() %>%     # get the url
  str_subset("\\csv")
html_node("#pp-csv-link")

country = 1:100 

link = paste0("https://www.populationpyramid.net/api/pp/", country, "/2017/?csv=true")

## Exploratoire

reslist = list()
pb <- winProgressBar("Téléchargement", "Nombre de fichiers telecharges",
                     0, 1000, 0)
for(i in 1:100){
  reslist[[i]] <- try(read_html(link[i]), silent = TRUE) 
  xopen::xopen(link[i])
  info <- sprintf("Telechargement effectuer: %d ", i)
  setWinProgressBar(pb, i, sprintf("test (%s)", info), info)
}
close(pb)

truelink <- link[which(sapply(reslist, function(x) !inherits(x, "try-error")))]

## Application sur toutes les années

str_replace_all(truelink, "https://www.populationpyramid.net/api/pp/", "") -> truelink

truecountry = parse_number(truelink)
annee = 1960:2016

finallink  = paste0("https://www.populationpyramid.net/api/pp/", truecountry, "/", 2016, "/?csv=true")

allink = sapply(annee, function(x) paste0("https://www.populationpyramid.net/api/pp/", truecountry, "/", x, "/?csv=true") )
allink = data.frame(allink)

collectt = function(x){
  pb <- winProgressBar("Téléchargement", "Nombre de fichiers telecharges",0, 235, 0)
  i = 1
  for(link in x){
    xopen::xopen(link)
    info <- sprintf("Telechargement effectuer: %d /235", i)
    setWinProgressBar(pb, i, sprintf("test (%s)", info), info)
    i = i+1
  }
  close(pb)
}

start = Sys.time()
apply(allink, MARGIN = 2, collectt)
end = Sys.time()
end - start
sapply(allink[, 57], collectt)

## Traitement

library(maps)
library(here)

files_names <- list.files(here("Data"))
files_names <- files_names[!str_detect(files_names,"[(]" )]
countrylist = gsub("-.*$","",files_names) %>% unique() %>% 
  data.frame(country = .)
genreta_data = function(country = countrylist$country[1]){
  files_names <- list.files(here("Data")) 
  files_names <- files_names[!str_detect(files_names,"[(]" )]
  files_country <- files_names[str_detect(files_names, country)]
  nb_files <- length(files_country)

  data_names <- vector("list",length=nb_files)
  
  dataset_list <- vector("list",length=nb_files)
  for (i in 1:nb_files) {
    x <- read.csv(paste(here("Data", files_country[i])), sep = ",") %>% 
      mutate(Year = 1960 + i -1, country = country)  
    dataset_list[[i]] <- x %>% mutate(propM = M/ sum(x$M+x$`F`), propF = `F`/ sum(x$M + x$`F`))
      #pivot_longer(cols = c(M, `F`), names_to = "Sexe", values_to = "value")
  }
  complete_data <- data.table::rbindlist(dataset_list)
  return(complete_data)
  
}

pb <- winProgressBar("Téléchargement", "Nombre de fichiers telecharges",0,length(countrylist$country), 0)
all_data = vector("list", length = length(countrylist$country))
for (i in 1:length(countrylist$country)) {
  all_data[[i]] <- genreta_data(countrylist$country[i])
  info <- sprintf("Telechargement effectuer: %d ", i)
  setWinProgressBar(pb, i, sprintf("test (%s)", info), info)
}
close(pb)
all_data = data.table::rbindlist(all_data)
write_rds(all_data, "all_data.rds")  
read_rds("all_data.rds")
'
  })
  
  ##### Actualisation ####
  
  
 plott <- reactive({
    year = input$year
    pays = input$pays
    r = all_data %>% 
      filter(country == pays, Year == year)
    age = r$Age
    
    
    r$Age = factor(r$Age, levels = unique(r$Age), ordered = T)
    ## Un dataframe qui contient ce que doit afficher l'interactivité
    tool = data.frame(
      label_M = paste("Effectif :", r$M,"\n", "Prop:" , paste(round(r$propM,4)*100,"%"), sep = " "),
      label_F = paste("Effectif :", r$`F`,"\n", "Prop:" , paste(round(r$propM,4)*100,"%"),sep = " "))
    plott <- r %>%
      ggplot(aes(x = c(1:length(age)))) +
      ## Les fonction qui permettent de crée des barres interactives
      ## L'argument tooltip prend l'affichage de la bulle lorsque le curseur survole la barre
      geom_bar_interactive( aes(y = -M, data_id = M, tooltip = tool$label_M), stat = "identity", width = 0.95, size = 3, fill ="#4682B4") +
      geom_bar_interactive( aes(y = `F`, data_id = `F`, tooltip = tool$label_F), stat = "identity", width = 0.95, size = 3, fill = "#C6116B") + 
      
      theme_light() +
      xlab("") + ylab("") +
      # coord_flip() +
      
      theme(panel.grid.major = element_blank(),
            axis.text.x = element_text(face = "bold"),
            axis.text.y = element_text(face = "bold"),
            panel.grid.minor = element_blank(),
            axis.line = element_line(colour = 1,
                                     size = 0.65, linetype = "solid"),
            plot.title = element_text(face = "bold", hjust = 0.5, size = 13),
            plot.caption = element_text(face = "bold", hjust = 1, size = 12),
            plot.subtitle = element_text(face = "italic", hjust = 0, size = 13),
            panel.border = element_rect(colour = 1,
                                        size = 0.65, linetype = "solid")) +
      
      scale_x_continuous(n.breaks = length(age), labels = c("",as.character(age),"")) +
      scale_y_continuous(labels = function(x) abs(x)) + 
      coord_flip() +
      
      labs(title = paste("Structure de la population :",pays), caption = paste("Année", year)) +
      geom_image(aes(x = 19, y = -max(M)/1.5), image = "MALEE.png", size = 0.1) +
      geom_image(aes(x = 19, y = max(`F`)/1.5), image = "FEMALEE.png", size = 0.1)
    
  })
  
  output$graphactu = renderggiraph({
    
    ggiraph(code = print(plott()),hover_css = "cursor:pointer;fill:skyblue;stroke:skyblue;stroke-width:2pt;" ,
            tooltip_extra_css = "background-color:skyblue;color:white;font-style:italic;padding:10px;border-radius:10px 20px 10px 20px;",
            tooltip_opacity = 1 )
    
    
  })
  
  output$graphstatique = renderPlot({
    year = input$year
    pays = input$pays
    r = all_data %>% 
      filter(country == pays, Year == year)
    age = r$Age
    
    
    r$Age = factor(r$Age, levels = unique(r$Age), ordered = T)
    
    r %>% 
      ggplot(aes(x = Age)) +
      geom_bar( aes(y = -propM), stat = "identity", width = 0.95, size = 1, fill ="#4682B4") +
      geom_bar( aes(y = propF), stat = "identity", width = 0.95, size = 3, fill = "#C6116B") +
      theme_light() +
      xlab("") + ylab("") +
      # coord_flip() +
      theme(panel.grid.major = element_blank(),
            axis.text.x = element_text(face = "bold"),
            axis.text.y = element_text(face = "bold"),
            panel.grid.minor = element_blank(),
            axis.line = element_line(colour = 1,
                                     size = 0.65, linetype = "solid"),
            plot.title = element_text(face = "bold", hjust = 0.5, size = 13),
            plot.caption = element_text(face = "bold", hjust = 1, size = 12),
            plot.subtitle = element_text(face = "italic", hjust = 0, size = 13),
            panel.border = element_rect(colour = 1,
                                        size = 0.65, linetype = "solid")) +
      scale_y_continuous(labels = function(x){paste(abs(x)*100, "%")}) +
      coord_flip() +
      labs(title = paste("Structure de la population",input$pays), caption = paste("Année", input$year)) +
      geom_image(aes(x = 19, y = -max(abs(propM))/1.2), image = "Malee.png", size = 0.1) +
      geom_image(aes(x = 19, y = max(propF)/1.2), image = "FEMALEE.png", size = 0.1)  +
      geom_text(aes(y = -propM/2 , label = f(round(propM,4)*100)), fontface = "bold", col  = "white") +
      geom_text(aes(y = propF/2, label =f(round(propF,4)*100)), fontface = "bold", col  = "white")
    
    
  })
  f1 = function(x, class) sum(x[class])
  
  output$indicateurr = renderUI({
    if(length(input$age1) >= 1){
      
    }else{
      selectInput("indicateur", "Indicateurs démographiques", 
                  choices= c("", "Rapport de masculinité", 
                             "Rapport de dépendances(v)",
                             "Rapport de dépendances(j)", 
                             "Rapport de dépendances(v-j)"), selected = "")
    }
  })
  
  
  
  output$evolution = renderPlot({
    if(length(input$age1) >= 1){
    all_data %>% 
      filter(country == input$pays) %>% 
      group_by(Year) %>% 
      summarise(
        M = f1(M, as.numeric(input$age1) ),
        `F` = f1(`F`, as.numeric(input$age1))
      ) %>% 
      mutate(total = M+`F`) %>% 
      pivot_longer(cols = c(M, `F`, total), values_to = "Effectif", names_to = "Sexe") %>% 
      ggplot(aes(x = Year, y = Effectif, col = Sexe)) +
      geom_line() +
      geom_point(aes(fill= Sexe),shape=21, color="black", size=2) +
      
      theme_light() +
      guides(guide_legend(title = ""))+
      ggtitle(paste("Evolution de la poulation de", input$pays)) 
    }else if(input$indicateur == "Rapport de masculinité"){
      all_data %>% 
        filter(country == input$pays) %>% 
        group_by(Year) %>% 
        summarise(
          M =sum(M) ,
          `F` = sum(`F`),
          rm = (M/`F`)*100
        ) %>% 
        ggplot(aes(x = Year, y = rm)) +
        geom_line() +
        geom_point(fill = 12,shape=21, color="black", size=2) +
        
        theme_light() +
        guides(guide_legend(title = ""))+
        ggtitle(paste("Rapport de masculinité", input$pays)) +
        labs(y = "")
    }else if (input$indicateur == "Rapport de dépendances(v)"){
      all_data %>% 
        filter(country == input$pays) %>% 
        group_by(Year) %>%
        mutate(t = M + `F`) %>% 
        summarise(
          rv =  (f1(t, 14:21)/f1(t, 5:13))*100
        )  %>% 
        ggplot(aes(x = Year, y = rv)) +
        geom_line() +
        geom_point(fill = 12,shape=21, color="black", size=2) +
        
        theme_light() +
        guides(guide_legend(title = ""))+
        ggtitle(paste("Rapport de dépendance Vieux", input$pays)) +
        labs(y = "")
    }else if (input$indicateur == "Rapport de dépendances(j)"){
      all_data %>% 
        filter(country == input$pays) %>% 
        group_by(Year) %>%
        mutate(t = M + `F`) %>% 
        summarise(
          rj = (f1(t, 1:4)/f1(t, 5:13))*100 ,
        )  %>% 
        ggplot(aes(x = Year, y = rj)) +
        geom_line() +
        geom_point(fill = 12,shape=21, color="black", size=2) +
        
        theme_light() +
        guides(guide_legend(title = ""))+
        ggtitle(paste("Rapport de dépendance Jeune", input$pays)) +
        labs(y = "")
    }else if (input$indicateur == "Rapport de dépendances(v-j)"){
      all_data %>% 
        filter(country == input$pays) %>% 
        group_by(Year) %>%
        mutate(t = M + `F`) %>% 
        summarise(
          rv = (f1(t, c(1:4, 14:21)) /f1(t, 5:13))*100 ,
        )  %>% 
        ggplot(aes(x = Year, y = rv)) +
        geom_line() +
        geom_point(fill = 12,shape=21, color="black", size=2) +
        
        theme_light() +
        guides(guide_legend(title = ""))+
        ggtitle(paste("Rapport de dépendance Vieux-jeune", input$pays)) +
        labs(y = "")
    }else{
      all_data %>% 
        filter(country == input$pays) %>% 
        group_by(Year) %>% 
        summarise(
          M = f1(M, 1:(n()) ),
          `F` = f1(`F`, 1:(n()))
        ) %>% 
        mutate(total = M+`F`) %>% 
        pivot_longer(cols = c(M, `F`, total), values_to = "Effectif", names_to = "Sexe") %>% 
        ggplot(aes(x = Year, y = Effectif, col = Sexe)) +
        geom_line() +
        geom_point(aes(fill= Sexe),shape=21, color="black", size=2) +
        
        theme_light() +
        guides(guide_legend(title = ""))+
        ggtitle(paste("Evolution de la poulation de", input$pays))
    }
    
  })
  struc = reactive({
    if(length(input$age1) >= 1){
      all_data %>% 
        filter(country == input$pays , Year == input$year) %>% 
        summarise(
          M = f1(M, as.numeric(input$age1) ),
          `F` = f1(`F`, as.numeric(input$age1))
        ) %>% 
        mutate(total = M+`F`)
    } else{
    all_data %>% 
      filter(country == input$pays , Year == input$year) %>% 
      summarise(
        M = sum(M),
        `F` = sum(`F`)
      ) %>% 
      mutate(total = M+`F`)
      }
  })
  output$Garçon = renderValueBox({
    
    valueBox( 
    
      tags$p(struc()[, 1] %>% pull() %>% label_comma()(.), style = "font-size: 75%;"),
      "Garçon",
      color = "green",
      icon = icon("person")
    )
   
  })
  output$Fille = renderValueBox({
    valueBox(
      tags$p(struc()[, 2] %>% pull()%>% label_comma()(.), style = "font-size: 75%;"),
      "Fille",
      color = "red",
      icon = icon("person-dress")
    )
  })
  output$Total = renderValueBox({
    valueBox(
      tags$p(struc()[, 3] %>% pull()%>% label_comma()(.), style = "font-size: 75%;"),
      "Total",
      color = "blue",
      icon = icon("users")
    )
  })
  
  output$radaar = renderPlot({
    
    all_data %>% 
      filter(country == input$pays , Year == input$year) %>% 
      select(-M, -`F`) %>% 
      pivot_longer(names_to = "Sexe", values_to = "Prop", cols = c(propM, propF)) %>% 
      pivot_wider(names_from = Age, values_from = c(Prop)) -> radardata
    max = ceiling(max(radardata[, -(1:4)]*100))  
    par(mar= margin(1, 2, 2, 1))
    radardata%>% 
      select(-(1:4)) -> daataforrada
    daataforrada = daataforrada*100
    rbind(max, 0, daataforrada) %>%  
      radarchart(
        axistype = 1,
        caxislabels = paste(0:max, "%"), 
        seg = max, 
        pcol = (c("#4682B4","#C6116B")), pfcol = alpha((c("#4682B4","#C6116B")), c(0.4, 0.3)), plwd = 2, plty = 1,
        cglcol = "grey", cglty = 1, cglwd = 0.8, axislabcol = "black", 
        title = paste("Population", input$pays, ":",input$year)
      )
    legend(
      x = "right", legend = c("Masculin", "Féminin"),
      bty = "n", pch = 20 , col = (c("#4682B4","#C6116B")),
      text.col = "black", cex = 1, pt.cex = 2
    )
  })
  
  ##### Comparaison ####
  output$morecontrol = renderUI({
    if(input$compartype == 1){
      tagList(
        selectInput("pays1", "Pays", 
                    choices = countrylist$country,
                    selected = "Germany"),
        selectInput("pays2", "Pays",
                    choices = countrylist$country,
                    selected = "France"),
        sliderInput("yearr", "Year", min = 1960, max = 2018,value = 1960, animate = TRUE)
      )
    }else{
      tagList(
        sliderInput("year1", "Year", min = 1960, max = 2018,value = 1960, animate = TRUE),
        sliderInput("year2", "Year", min = 1960, max = 2018,value = 2015, animate = TRUE),
        selectInput("payys", "Pays",
                    choices = countrylist$country,
                    selected = "Japan")
      )
      
    }
    
  })
  don = reactive({
    if(input$compartype == 2){
      all_data %>% 
        filter(Year %in% c(input$year1,input$year2), country == input$payys)  %>% 
        pivot_longer(c(M, `F`), names_to = "Sexe", values_to = "Eff") 
    }else{
      all_data %>% 
        filter(country %in% c(input$pays1,input$pays2), Year == input$yearr)  %>% 
        pivot_longer(c(M, `F`), names_to = "Sexe", values_to = "Eff")
    }
    
    }) 
  
  plottt = reactive({
   
    if(input$compartype == 2){
      r4 = don()
    r4$Age = factor(r4$Age, levels = unique(r4$Age), ordered = T)
    
    
    Male = c(paste(input$year1 ," : Male", unique(r4$Age), "\n", abs(r4$Eff[r4$Year == input$year1 & r4$Sexe == "M"]) %>% label_comma()(.), sep = 
                     " ") ,
             paste(input$year2 ," : Male",unique(r4$Age),"\n",  abs(r4$Eff[r4$Year == input$year2 & r4$Sexe == "M"]) %>% label_comma()(.), sep = 
                     " "))
    
    Female = c(paste(input$year1, ": Female", unique(r4$Age), "\n", abs(r4$Eff[r4$Year == input$year1 & r4$Sexe == "F"])%>% label_comma()(.), sep = 
                       " ") ,
               paste(input$year2," : Female",unique(r4$Age),"\n",  abs(r4$Eff[r4$Year == input$year2 & r4$Sexe == "F"])%>% label_comma()(.), sep = 
                       " "))
    r4$Year = factor(r4$Year)
    ## Geom_bar avec position "dodge" pour afficher deux barres pour chaque années
     ggplot() +
      geom_bar_interactive(
        data = r4 %>%filter(Sexe == "M"),
        aes(x = Age, y = - 1*Eff, fill = Year, tooltip = Male),
        stat = "identity",
        width = 0.90, position = "dodge"
      ) +
      geom_bar_interactive(
        data =r4 %>%filter(Sexe == "F"),
        aes(x = Age, y = Eff, fill  = Year, tooltip = Female),
        stat = "identity",
        width = 0.90, position = "dodge"
      ) +
      
      geom_hline(yintercept = 0,  size =0.9, col = "white") +
      coord_flip() +
      theme_light() +
      xlab("") + ylab("") +
      
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_line(colour = 1, 
                                     size = 0.65, linetype = "solid"),
            plot.title = element_text(face = "bold", hjust = 0.5, size = 13),
            plot.subtitle = element_text(face = "bold", hjust = 0.5, size = 13),
            plot.caption = element_text(face = "bold", hjust = 1, size = 12),
            panel.border = element_rect(colour = 1, 
                                        size = 0.65, linetype = "solid")) + 
      scale_y_continuous(labels = function(x){abs(x)}) +
      
      labs(title = paste("Comparaison de la population :", input$payys), subtitle = ) +
      geom_image(aes(x = 19, y = -max(abs(r4$Eff)/1.2)), image = "MALEE.png", size = 0.1) +
      geom_image(aes(x = 19, y = max(r4$Eff)/1.2), image = "FEMALEE.png", size = 0.1) +
      
      scale_fill_manual(values = c("#4682B4","#C6116B" ), aesthetics = "fill") 
    }else{
     
      
      r4 = don()
      
      r4$Age = factor(r4$Age, levels = unique(r4$Age), ordered = T)
      
      
      Male = c(paste(input$pays1 ," : Male", unique(r4$Age), "\n", abs(r4$Eff[r4$country == input$pays1 & r4$Sexe == "M"]) %>% label_comma()(.), sep = 
                       " ") ,
               paste(input$pays2 ," : Male",unique(r4$Age),"\n",  abs(r4$Eff[r4$country == input$pays2 & r4$Sexe == "M"]) %>% label_comma()(.), sep = 
                       " "))
      
      Female = c(paste(input$pays1, ": Female", unique(r4$Age), "\n", abs(r4$Eff[r4$country == input$pays1 & r4$Sexe == "F"])%>% label_comma()(.), sep = 
                         " ") ,
                 paste(input$pays2," : Female",unique(r4$Age),"\n",  abs(r4$Eff[r4$country == input$pays2 & r4$Sexe == "F"])%>% label_comma()(.), sep = 
                         " "))
      r4$Year = factor(r4$Year)
      ## Geom_bar avec position "dodge" pour afficher deux barres pour chaque années
      ggplot() +
        geom_bar_interactive(
          data = r4 %>%filter(Sexe == "M"),
          aes(x = Age, y = - 1*Eff, fill = country, tooltip = Male),
          stat = "identity",
          width = 0.90, position = "dodge"
        ) +
        geom_bar_interactive(
          data =r4 %>%filter(Sexe == "F"),
          aes(x = Age, y = Eff, fill  = country, tooltip = Female),
          stat = "identity",
          width = 0.90, position = "dodge"
        ) +
        
        geom_hline(yintercept = 0,  size =0.9, col = "white") +
        coord_flip() +
        theme_light() +
        xlab("") + ylab("") +
        
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.line = element_line(colour = 1, 
                                       size = 0.65, linetype = "solid"),
              plot.title = element_text(face = "bold", hjust = 0.5, size = 13),
              plot.subtitle = element_text(face = "bold", hjust = 0.5, size = 13),
              plot.caption = element_text(face = "bold", hjust = 1, size = 12),
              panel.border = element_rect(colour = 1, 
                                          size = 0.65, linetype = "solid")) + 
        scale_y_continuous(labels = function(x){abs(x)}) +
        
        labs(title = paste("Comparaison de population :", input$yearr), subtitle = ) +
        geom_image(aes(x = 19, y = -max(abs(r4$Eff)/1.2)), image = "MALEE.png", size = 0.1) +
        geom_image(aes(x = 19, y = max(r4$Eff)/1.2), image = "FEMALEE.png", size = 0.1) +
        
        scale_fill_manual(values = c("#4682B4","#C6116B" ), aesthetics = "fill") 
      
    }
  })
  output$comparpyram = renderggiraph({

    
    ggiraph(code = print(plottt()),
            hover_css = "cursor:pointer;fill:gray;stroke:gray;stroke-width:2pt;" ,
            tooltip_extra_css = "background-color:skyblue;color:white;font-style:italic;padding:10px;border-radius:10px 20px 10px 20px;",
            tooltip_opacity = 1 ) 
  })
  
  
  output$super = renderPlot({
    
    if(input$compartype == 2){
    r4 = don()
    
    r4$Age = factor(r4$Age, levels = unique(r4$Age), ordered = T)
    
    r4 %>%
      ggplot() +
      geom_bar(data = r4 %>% filter(Year == input$year1 & Sexe == "M"),
               aes(x= Age, y =- Eff), stat = "identity", fill = "#4682B4", width = 0.5) +
      
      geom_bar(data = r4 %>% filter(Year == input$year2 & Sexe == "M"),
               aes(x= Age, y = -Eff), stat = "identity", fill = "#4682B4", alpha = 0.4, width = 1) +
      
      geom_bar(data = r4 %>% filter(Year == input$year1 & Sexe == "F"),
               aes(x= Age, y = Eff), stat = "identity", fill = "#C6116B" , width = 0.5) +
      
      geom_bar(data = r4 %>% filter(Year == input$year2 & Sexe == "F"),
               aes(x= Age, y = Eff), stat = "identity", fill = "#C6116B", alpha = 0.4, width = 1) +
      coord_flip() +
      theme_light() +
      xlab("") + ylab("") +
      # coord_flip() +
      
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_line(colour = 1, 
                                     size = 0.65, linetype = "solid"),
            plot.title = element_text(face = "bold", hjust = 0.5, size = 13),
            plot.subtitle = element_text(face = "bold", hjust = 0.5, size = 13),
            plot.caption = element_text(face = "bold", hjust = 1, size = 12),
            panel.border = element_rect(colour = 1, 
                                        size = 0.65, linetype = "solid"), 
            legend.position="bottom") + 
      scale_y_continuous(n.breaks = 10, labels = function(x){abs(x)}) +
      
      labs(title = paste("Comparaison de la population :", input$payys)) +
      geom_image(aes(x = 19, y = -max(abs(Eff))/1.2), image = "MALEE.png", size = 0.1) +
      geom_image(aes(x = 19, y = max(Eff)/1.2), image = "FEMALEE.png", size = 0.1) +
      
      ## Les geom_label font office de legendes
      geom_label(aes(x = 18,  y = -max(abs(Eff)/1.7),label =  input$year2),
                 fill = "#B5CDE1ff", col = "white", fontface = "bold")+
      geom_label(aes(x = 19,  y = -max(abs(Eff)/1.7),label =input$year1), 
                 fill = "#4682B4", col = "white", fontface = "bold")+
      geom_label(aes(x = 18,  y = max(abs(Eff)/1.7),label = input$year2),
                 fill = "#E8A0C4ff",  col = "white", fontface = "bold")+
      geom_label(aes(x = 19,  y = max(abs(Eff)/1.7),label = input$year1),
                 fill = "#C6116B", col = "white", fontface = "bold")
    }else {
      r4 = don()
      r4$Age = factor(r4$Age, levels = unique(r4$Age), ordered = T)
      
      r4 %>%
        ggplot() +
        geom_bar(data = r4 %>% filter(country == input$pays1 & Sexe == "M"),
                 aes(x= Age, y =- Eff), stat = "identity", fill = "#4682B4", width = 0.5) +
        
        geom_bar(data = r4 %>% filter(country == input$pays2 & Sexe == "M"),
                 aes(x= Age, y = -Eff), stat = "identity", fill = "#4682B4", alpha = 0.4, width = 1) +
        
        geom_bar(data = r4 %>% filter(country == input$pays1 & Sexe == "F"),
                 aes(x= Age, y = Eff), stat = "identity", fill = "#C6116B" , width = 0.5) +
        
        geom_bar(data = r4 %>% filter(country == input$pays2 & Sexe == "F"),
                 aes(x= Age, y = Eff), stat = "identity", fill = "#C6116B", alpha = 0.4, width = 1) +
        coord_flip() +
        theme_light() +
        xlab("") + ylab("") +
        # coord_flip() +
        
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.line = element_line(colour = 1, 
                                       size = 0.65, linetype = "solid"),
              plot.title = element_text(face = "bold", hjust = 0.5, size = 13),
              plot.subtitle = element_text(face = "bold", hjust = 0.5, size = 13),
              plot.caption = element_text(face = "bold", hjust = 1, size = 12),
              panel.border = element_rect(colour = 1, 
                                          size = 0.65, linetype = "solid"), 
              legend.position="bottom") + 
        scale_y_continuous(n.breaks = 10, labels = function(x){abs(x)}) +
        
        labs(title = paste("Comparaison de population :", input$yearr)) +
        geom_image(aes(x = 19, y = -max(abs(Eff))/1.2), image = "MALEE.png", size = 0.1) +
        geom_image(aes(x = 19, y = max(Eff)/1.2), image = "FEMALEE.png", size = 0.1) +
        
        ## Les geom_label font office de legendes
        geom_label(aes(x = 18,  y = -max(abs(Eff)/1.7),label =  input$pays2),
                   fill = "#B5CDE1ff", col = "white", fontface = "bold")+
        geom_label(aes(x = 19,  y = -max(abs(Eff)/1.7),label =input$pays1), 
                   fill = "#4682B4", col = "white", fontface = "bold")+
        geom_label(aes(x = 18,  y = max(abs(Eff)/1.7),label = input$pays2),
                   fill = "#E8A0C4ff",  col = "white", fontface = "bold")+
        geom_label(aes(x = 19,  y = max(abs(Eff)/1.7),label = input$pays1),
                   fill = "#C6116B", col = "white", fontface = "bold")

    }
  })
  
  output$Radar = renderPlot({
    if(input$compartype == 2){

    all_data %>% 
      filter(Year %in% c(input$year1,input$year2), country == input$payys) %>% 
      mutate(Total = (propM +propF)*100) %>% 
      select(-M, -`F`, -propM, - propF) %>% 
      pivot_wider(names_from = Age, values_from = Total) -> radardata
    
    
    
    max = ceiling(max(radardata[, -(1:3)]))  
    par(mar= margin(1, 2, 2, 1))
    rbind(max, 0, radardata) %>% 
      select(-(1:3)) %>% 
      radarchart(
        axistype = 1,
        caxislabels = paste(0:max, "%"), 
        seg = max, 
        pcol = rev(c("#4682B4","#C6116B")), pfcol = alpha(rev(c("#4682B4","#C6116B")), c(0.4, 0.3)), plwd = 2, plty = 1,
        cglcol = "grey", cglty = 1, cglwd = 0.8, axislabcol = "black", 
        title = "Population Totale"
      )
    
    legend(
      x = "right", legend = c(input$year2, input$year1),
      bty = "n", pch = 20 , col = (c("#4682B4","#C6116B")),
      text.col = "black", cex = 1, pt.cex = 2
    )
    }else{
      all_data %>% 
        filter(country %in% c(input$pays1,input$pays2),Year  == input$yearr) %>% 
        mutate(Total = (propM +propF)*100) %>% 
        select(-M, -`F`, -propM, - propF) %>% 
        pivot_wider(names_from = Age, values_from = Total) -> radardata
      
      
      
      max = ceiling(max(radardata[, -(1:3)]))  
      par(mar= margin(1, 2, 2, 1))
      rbind(max, 0, radardata) %>% 
        select(-(1:3)) %>% 
        radarchart(
          axistype = 1,
          caxislabels = paste(0:max, "%"), 
          seg = max, 
          pcol = rev(c("#4682B4","#C6116B")), pfcol = alpha(rev(c("#4682B4","#C6116B")), c(0.4, 0.3)), plwd = 2, plty = 1,
          cglcol = "grey", cglty = 1, cglwd = 0.8, axislabcol = "black", 
          title = "Population Totale"
        )
      legend(
        x = "right", legend = c(input$pays2, input$pays1),
        bty = "n", pch = 20 , col = (c("#4682B4","#C6116B")),
        text.col = "black", cex = 1, pt.cex = 2
      )
    }
    
  })
  
  ##### Visualisatio globale ####
      
  
  data(worldgeojson, package = "highcharter")
  output$mapp = renderHighchart({
    if(length(input$age2) >= 1){
      mapdon = all_data %>%
        filter(Year==input$year3) %>%
        group_by(Country) %>%
        summarize( Masculin= f1(M, as.numeric(input$age2))/sum(M),
                   Feminin =  f1(`F`, as.numeric(input$age2))/sum(`F`),
                   Total = sum(f1(M, as.numeric(input$age2)) + f1(`F`, as.numeric(input$age2)))/ sum(M + `F`)) %>% 
        .[ , c("Country", input$sexxe)]
      hc <- highchart() %>%
        hc_add_series_map(
          worldgeojson, mapdon, value = input$sexxe, joinBy = c('name','Country'),
          name = input$sexxe, dataLabels = list(enabled = TRUE, format = "{point.name}"),
          borderColor = "#FAFAFA",
          borderWidth = 0.1
        )  %>% 
        hc_colorAxis(max = max(mapdon$Total), stops = color_stops(10, viridisLite::inferno(10, direction = -1))) %>% 
        #c("#CFE0C3","#9EC1A3", "#70A9A1",  "#40798C", "#1F363D") ))  %>% 
        hc_title(text = "Population Mondiale") %>% 
        hc_subtitle(text = paste(input$sexxe, ": ", input$year3)) 
      hc %>% 
        hc_mapNavigation(enabled = TRUE)
    }else{
      mapdon = all_data %>%
        filter(Year==input$year3) %>%
        group_by(Country) %>%
        summarize( Masculin= sum(M),
                   Feminin =  sum(`F`),
                   Total = sum(M + `F`)) %>% 
        .[ , c("Country", input$sexxe)]
      hc <- highchart() %>%
        hc_add_series_map(
          worldgeojson, mapdon, value = input$sexxe, joinBy = c('name','Country'),
          name = input$sexxe, dataLabels = list(enabled = TRUE, format = "{point.name}"),
          borderColor = "#FAFAFA",
          borderWidth = 0.1
        )  %>% 
        hc_colorAxis(max = 1.5e9, stops = color_stops(10, viridisLite::inferno(10, direction = -1))) %>% 
        #c("#CFE0C3","#9EC1A3", "#70A9A1",  "#40798C", "#1F363D") ))  %>% 
        hc_title(text = "Population Mondiale") %>% 
        hc_subtitle(text = paste(input$sexxe, ": ", input$year3)) 
      hc %>% 
        hc_mapNavigation(enabled = TRUE)
    }
    
  })
}

shiny::shinyApp(ui, server)