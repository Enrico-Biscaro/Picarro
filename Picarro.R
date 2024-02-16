#INSTALLAZIONE DEI PACCHATTI############################################################################################################################################
packages_to_check <- c("shiny", "DT", "grid", "shinydashboard", "dplyr", "ggplot2", 
                       "tidyr", "zoo", "ggpmisc", "insight", "patchwork", "gridExtra", 
                       "rstudioapi")
for (package in packages_to_check) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}
library(shiny)
library(DT)
library(grid)
library(gridExtra)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(tidyr)
library(zoo)
library(ggpmisc)
library(insight)
library(patchwork)
library(rstudioapi)
folder_path <- rstudioapi::getSourceEditorContext()
folder <- dirname(folder_path$path)   
#CREAZIONE TABELLE SHINY###############################################################################################################################################
values_oxygen <- c(0.78, -8.88, -15.37, -31.12, -34.04, -38.78, -54.56, -51.95, -5.04)
values_hydrogen <- c(5.22, -58.36, -112.75, -239.52, -273.27, -306.39, -424.23, -407.46, -32.67)
column_names_oxygen <- c("MSW_O", "VE2022_O", "NS_O", "NVL_O", "ITASE_O", "TD_O", "AP1_O", "DCS_O", "C_O")
row_names_oxygen <- c("Oxygen")
column_names_hydrogen <- c("MSW_H", "VE2022_H", "NS_H", "NVL_H", "ITASE_H", "TD_H", "AP1_H", "DCS_H", "C_H")
row_names_hydrogen <- c("Hydrogen")
data_oxygen <- data.frame(matrix(values_oxygen, nrow = 1, byrow = TRUE))
colnames(data_oxygen) <- column_names_oxygen
rownames(data_oxygen) <- row_names_oxygen
data_hydrogen <- data.frame(matrix(values_hydrogen, nrow = 1, byrow = TRUE))
colnames(data_hydrogen) <- column_names_hydrogen
rownames(data_hydrogen) <- row_names_hydrogen
#INTERFACCIA APP SHINY#################################################################################################################################################
ui <- fluidPage(
  tags$style(HTML("
    body {overflow: hidden; margin: 0px;}
    html, body, .container {height: 100%;}
    #plot_G_S {
      margin-left: -300px; /* Imposta il margine sinistro in base alle tue esigenze */
    }
  ")),
  tags$head(
    tags$script('
      $(document).ready(function() {
        var elem = document.documentElement;
        var requestFullScreen = elem.requestFullscreen || elem.mozRequestFullScreen || elem.webkitRequestFullscreen || elem.msRequestFullscreen;

        if (requestFullScreen) {
          requestFullScreen.call(elem);
        }
      });
    ')
  ),
  titlePanel(
    div(
      "Calibration for Picarro Analyzers L2130-i/L2140-i",
      style = "color: white; background-color: green; padding: 10px; font-size: 50px;"
    ),
    windowTitle = "PICARRO L2130-i/L2140-i"
  ),
  
  br(),
  fluidRow(
    column(
      width = 4,
      fileInput("file_mydf", "Upload raw data (mydf.csv)"),
      fileInput("file_samples", "Upload samples (Samples.csv)"),
      textInput("input_testo", "Sampling date", value = ""),
      selectInput("std_r_o_1", "Standard Oxygen (1)", choices = c("", "0.78", "-8.88", "-15.37", "-31.12", "-34.04", "-38.78", "-54.56", "-51.95", "-5.04")),
      selectInput("std_r_o_2", "Standard Oxygen (2)", choices = c("", "0.78", "-8.88", "-15.37", "-31.12", "-34.04", "-38.78", "-54.56", "-51.95", "-5.04")),
      selectInput("std_r_h_1", "Standard Hydrogen (1)", choices = c("", "5.22", "-58.36", "-112.75", "-239.52", "-273.27", "-306.39", "-424.23", "-407.46", "-32.67")),
      selectInput("std_r_h_2", "Standard Hydrogen (2)", choices = c("", "5.22", "-58.36", "-112.75", "-239.52", "-273.27", "-306.39", "-424.23", "-407.46", "-32.67")),
      selectInput("c_r_o", "Control sample Oxygen", choices = c("", "0.78", "-8.88", "-15.37", "-31.12", "-34.04", "-38.78", "-54.56", "-51.95", "-5.04")),
      selectInput("c_r_h", "Control sample Hydrogen", choices = c("", "5.22", "-58.36", "-112.75", "-239.52", "-273.27", "-306.39", "-424.23", "-407.46", "-32.67")),
      actionButton("run_button", "Esegui Script", class = "btn btn-primary", style = "background-color: green;")
    ),
    column(
      width = 8,
      plotOutput("plot_G_S", height = "800px", width = "1500"),
      uiOutput("control_result_oxygen"),
      uiOutput("control_result_hydrogen")
    )
  ),
  
  fluidRow(
    column(
      width = 6,
      h4("Standards values for Oxygen"),
      DTOutput("table_oxygen")
    ),
    column(
      width = 6,
      h4("Standards values for Hydrogen"),
      DTOutput("table_hydrogen")
    )
  )
)

server <- function(input, output, session) {
  data_reactive <- reactiveValues(
    data_oxygen = data_oxygen,
    data_hydrogen = data_hydrogen,
    G_S = NULL,
    control_result_oxygen = NULL,
    control_result_hydrogen = NULL
  )
  observeEvent(input$run_button, {
    STD_r_O_1 <- input$std_r_o_1
    STD_r_O_2 <- input$std_r_o_2
    STD_r_H_1 <- input$std_r_h_1
    STD_r_H_2 <- input$std_r_h_2
    C_r_O <- input$c_r_o
    C_r_H <- input$c_r_h
    STD_r_O_1 <- as.numeric(input$std_r_o_1)
    STD_r_O_2 <- as.numeric(input$std_r_o_2)
    STD_r_H_1 <- as.numeric(input$std_r_h_1)
    STD_r_H_2 <- as.numeric(input$std_r_h_2)
    C_r_O <- as.numeric(input$c_r_o)
    C_r_H <- as.numeric(input$c_r_h)
    req(input$file_mydf$datapath, input$file_samples$datapath)
    mydf <- read.csv(input$file_mydf$datapath)
    Samples <- read.csv(input$file_samples$datapath)
    write.csv(mydf, file.path(folder, "mydf.csv"), row.names = FALSE)
    write.csv(Samples, file.path(folder, "Samples.csv"), row.names = FALSE)
    
    #Caricamento file di input (raw data e lista campioni)#################################################################################################################
    folder_path <- rstudioapi::getSourceEditorContext()
    folder <- dirname(folder_path$path)   
    mydf <- read.csv(paste(folder,"/mydf.csv",sep=""))
    Samples <- read.csv(paste(folder,"/Samples.csv",sep=""))
    par <- mydf %>% select(Temperature = `DAS.Temp`, ppm_water = `H2O_Mean`)
    mydf <- mydf %>% select(Oxygen = `d.18_16.Mean`, Hydrogen = `d.D_H.Mean`)
    
    #TRATTAMENTO DEI DATI##################################################################################################################################################
    
    # 1. Load the PICARRO output (mydf.csv) and the samples list (Samples.csv) and run 
    # N.B. Change the "each" value depending on the number of injections
    # N.B. Before running the regression check the data.frame "final_df"
    Samples_list <- data.frame(Samples = Samples[rep(1:nrow(Samples), each = 8), ])
    GroupLabels <- rep(c(1:8),times=length(Samples))
    original_df <- data.frame(GroupLabels,Samples_list,mydf) 
    new_df <- subset(original_df, !(GroupLabels %in% c(1,2,3,4,5)))
    new_df <- subset(original_df, !(GroupLabels %in% c(1,2,3,4,5)))
    final_df <- new_df %>% 
      mutate(Count = rep(row_number(), each=3, length.out = n()))
    aggregate(final_df$Oxygen, list(final_df$Count), FUN=mean)
    Mean_oxygen <- setNames(aggregate(final_df$Oxygen, list(final_df$Count), FUN=mean),c("Samples","Mean_oxygen"))
    SD_oxygen <-  setNames(aggregate(final_df$Oxygen, list(final_df$Count), FUN=sd),c("Samples","Standard_deviation_oxygen"))
    Mean_oxygen <- data.frame(Mean_oxygen,SD_oxygen$Standard_deviation_oxygen)
    names(Mean_oxygen) <- c("Samples","Mean_oxygen","Standard_deviation_oxygen")
    Mean_hyrdogen <-  setNames(aggregate(final_df$Hydrogen, list(final_df$Count), FUN=mean),c("Samples","Mean_hydrogen"))
    SD_hydrogen <-  setNames(aggregate(final_df$Hydrogen, list(final_df$Count), FUN=sd),c("Samples","Standard_deviation_hydrogen"))
    Mean_hydrogen <- data.frame(Mean_hyrdogen,SD_hydrogen$Standard_deviation_hydrogen)
    names(Mean_hydrogen) <- c("Samples","Mean_oxygen","Standard_deviation_hydrogen")
    
    
    #Here you can select the standards that you want to use for the calibration 
    O <- data.frame(Samples=Samples,Mean_oxygen=Mean_oxygen$Mean_oxygen,Standard_deviation_oxygen=Mean_oxygen$Standard_deviation_oxygen)
    H <- data.frame(Samples=Samples,Mean_hydrogen=Mean_hyrdogen$Mean_hydrogen, Standard_deviation_hydrogen=Mean_hydrogen$Standard_deviation_hydrogen)
    S_O <- subset(O, Samples == "STD_1.2" | Samples == "STD_2.2")
    S_H <- subset(H, Samples == "STD_1.2" | Samples == "STD_2.2")
    Real_standard_O <- c(STD_r_O_1,STD_r_O_2)
    Real_standard_H <- c(STD_r_H_1,STD_r_H_2)
    STD_a_O <- setNames(aggregate(S_O$Mean_oxygen,list(S_O$Samples),FUN=mean),c("STD_O","Analyzed_standard_O"))
    STD_a_H <- setNames(aggregate(S_H$Mean_hydrogen,list(S_H$Samples),FUN=mean),c("STD_H","Analyzed_standard_H"))
    R_O <- data.frame(STD_a_O,Real_standard_O)
    R_H <- data.frame(STD_a_H,Real_standard_H)
    O$Samples<-factor(O$Samples, levels = unique(O$Samples))
    H$Samples<-factor(H$Samples, levels = unique(H$Samples))
    G_SD_O <-ggplot(O, aes(x = Samples, y = Standard_deviation_oxygen)) +
      geom_bar(stat = "identity", position = "dodge", fill = "red", width = 0.7) +
      labs(title = "Standard Deviations Oxygen", x = "Samples", y = "Standard deviation oxygen") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
    G_SD_H <- ggplot(H, aes(x = Samples, y = Standard_deviation_hydrogen)) +
      geom_bar(stat = "identity", position = "dodge", fill = "blue", width = 0.7) +
      labs(title = "Standard Deviations Hydrogen", x = "Samples", y = "Standard deviation hydrogen") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
    D_O <- ggplot(R_O, aes(x = STD_O, y = Real_standard_O-Analyzed_standard_O)) +
      geom_bar(stat = "identity", fill = "red") +
      labs(title="Real standatd vs anaylzed standard (difference) for O",x="Standard",y="Difference")+
      theme_bw()
    D_H <-  ggplot(R_H, aes(x = STD_H, y = Real_standard_H-Analyzed_standard_H)) +
      geom_bar(stat = "identity", fill = "blue") +
      labs(title="Real standatd vs anaylzed standard (difference) for H",x="Standard",y="Difference")+
      theme_bw()
    G_SD_O + G_SD_H + D_O + D_H
    
    #3. Run the calibration curve for oxygen and hydrogen
    fit_O <- lm(STD_a_O$Analyzed_standard_O~Real_standard_O, data=R_O)
    fit_H <- lm(STD_a_H$Analyzed_standard_H~Real_standard_H, data=R_H)
    G_O <- ggplot(R_O, aes(x = STD_a_O$Analyzed_standard_O, y = Real_standard_O)) +
      labs(title="Oxygen calibration curve",x="Standard (PICARRO)",y="Standard (Real values)")+
      geom_point(colour = 'red', size = 3) +
      geom_line(colour="red")+
      stat_poly_eq(formula = y ~ x, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),size=5.5,parse=TRUE)+
      geom_smooth(method = "lm", se = FALSE, color = "red", fullrange = TRUE)+
      theme_bw()
    #geom_text(aes(label = STD_O,hjust = - 0.05, vjust = 2))
    G_H <- ggplot(R_H, aes(x = STD_a_H$Analyzed_standard_H, y = Real_standard_H)) +
      labs(title="Hydrogen calibration curve",x="Standard (PICARRO)",y="Standard (Real values)")+
      geom_point(colour = 'blue', size = 3) +
      geom_line(colour="blue")+
      stat_poly_eq(formula = y ~ x, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),size=5.5,parse=TRUE)+
      geom_smooth(method = "lm", se = FALSE, color = "blue", fullrange = TRUE)+
      theme_bw()
    G_O + G_H + G_SD_O + G_SD_H    
    linear_model <- lm(STD_a_O$Analyzed_standard_O~Real_standard_O, data=R_O)
    linear_model <- lm(STD_a_H$Analyzed_standard_H~Real_standard_H, data=R_H)
    
    #4. Control samples
    Control_O <- subset(O,Samples=="C")
    Control_H <- subset(H,Samples=="C")
    data_G_O <- ggplot_build(G_O)$data[[1]]
    lm_model_O_manual <- lm(data_G_O$y ~ data_G_O$x)
    pendenza_O  <- coef(lm_model_O_manual)[2]
    intercetta_O <- coef(lm_model_O_manual)[1]
    E_O <- (Control_O$Mean_oxygen * pendenza_O + intercetta_O) - C_r_O
    data_G_H <- ggplot_build(G_H)$data[[1]]
    lm_model_H_manual <- lm(data_G_H$y ~ data_G_H$x)
    pendenza_H  <- coef(lm_model_H_manual)[2]
    intercetta_H <- coef(lm_model_H_manual)[1]
    E_H <- (Control_H$Mean_hydrogen * pendenza_H + intercetta_H) - C_r_H
    for (i in seq_along(E_O)) {
      if (abs(E_O[i]) < 0.1) {
        print_color(paste("GOOD CONTROL SAMPLES FOR OXYGEN! E_O =", E_O[i]), "green")
      } else {
        print_color(paste("BAD CONTROL SAMPLES FOR OXYGEN! E_O =", E_O[i]), "red")
      }
    }
    for (j in seq_along(E_H)) {
      if (abs(E_H[j]) < 0.5) {
        print_color(paste("GOOD CONTROL SAMPLES FOR HYDROGEN! E_H =", E_H[j]), "green")
      } else {
        print_color(paste("BAD CONTROL SAMPLES FOR HYDROGEN! E_H =", E_H[j]), "red")
      }
    }
    
    #6. Data calibration 
    Cal_O <- subset(O, Samples != "STD_1.1" & Samples != "STD_1.2" & Samples != "STD_2.1" & Samples != "STD_2.2" & Samples != "C")
    Cal_H <- subset(H, Samples != "STD_1.1" & Samples != "STD_1.2" & Samples != "STD_2.1" & Samples != "STD_2.2" & Samples != "C")
    lm_model_O <- lm(Real_standard_O ~ Analyzed_standard_O, data = STD_a_O)
    intercept_O <- coef(lm_model_O)[1]
    slope_O <- coef(lm_model_O)[2]
    O_results<- data.frame(Samples=as.factor(Cal_O$Samples),Calibrated_O = slope_O * Cal_O$Mean_oxygen + intercept_O)
    O_results$Samples<-factor(O_results$Samples, levels = unique(O_results$Samples))
    Cal_G_O <- ggplot(O_results, aes(x = Samples, y = Calibrated_O)) +
      geom_bar(stat = "identity", fill = "red") +
      labs(title = "Calibrated data for oxygen", x = "Sample", y = "Oxygen")+
      coord_cartesian(ylim = c(min(O_results$Calibrated_O), max(O_results$Calibrated_O)))+
      theme_bw()
    lm_model_H <- lm(Real_standard_H ~ Analyzed_standard_H, data = STD_a_H)
    intercept_H <- coef(lm_model_H)[1]
    slope_H <- coef(lm_model_H)[2]
    H_results<- data.frame(Samples=Cal_H$Samples,Calibrated_H = slope_H * Cal_H$Mean_hydrogen + intercept_H)
    H_results$Samples<-factor(H_results$Samples, levels = unique(H_results$Samples))
    Cal_G_H <- ggplot(H_results, aes(x = Samples, y = Calibrated_H)) +
      geom_bar(stat = "identity", fill = "blue") +
      labs(title = "Calibrated data for hydrogen", x = "Sample", y = "Hydrogen")+
      coord_cartesian(ylim = c(min(H_results$Calibrated_H), max(H_results$Calibrated_H)))+
      theme_bw()
    par$group <- rep(1:ceiling(nrow(par)/8), each = 8)[1:nrow(par)]
    mean_data <- par %>% 
      group_by(group) %>% 
      summarise(mean_Temperature = mean(Temperature),
                mean_ppm_water = mean(ppm_water))
    T <- ggplot(mean_data, aes(x = group, y = mean_Temperature)) +
      geom_line(size = 1.5, color = "green") +
      geom_smooth(method = "loess", se = FALSE, color = "lightgreen", size = 0.5) +
      labs(title = "Temperature (mean each 8 values)",
           x = "Group (8 values)",
           y = "Temperature (°C)")+
      theme_bw()
    W <- ggplot(mean_data, aes(x = group, y = mean_ppm_water)) +
      geom_line(size = 1.5, color = "orange") +
      geom_smooth(method = "loess", se = FALSE, color = "orange", size = 0.5) +
      labs(title = "ppm water (mean each 8 values)",
           x = "Group (8 values)",
           y = "Water (ppm)")+
      theme_bw()
    
    #7. Summary 
    G_S <- grid.arrange(G_SD_O, G_O, Cal_G_O, D_O, T, G_SD_H, G_H, Cal_G_H, D_H, W, ncol = 5)
    for (i in seq_along(E_O)) {
      if (abs(E_O[i]) < 0.1) {
        print_color(paste("GOOD CONTROL SAMPLES FOR OXYGEN! E_O =", E_O[i]), "green")
      } else {
        print_color(paste("BAD CONTROL SAMPLES FOR OXYGEN! E_O =", E_O[i]), "red")
      }
    }
    for (j in seq_along(E_H)) {
      if (abs(E_H[j]) < 0.5) {
        print_color(paste("GOOD CONTROL SAMPLES FOR HYDROGEN! E_H =", E_H[j]), "green")
      } else {
        print_color(paste("BAD CONTROL SAMPLES FOR HYDROGEN! E_H =", E_H[j]), "red")
      }
    }
    date_sampling <- input$input_testo
    l <- 21.75
    h <- 11.22
    folder_path <- rstudioapi::getSourceEditorContext()
    
    # Modifica la variabile folder per includere la sottocartella con il nome date_sampling
    folder <- file.path(dirname(folder_path$path), date_sampling)
    dir.create(folder, showWarnings = FALSE)
    
    date <- format(Sys.Date(), "%Y_%m_%d")
    file_name <- paste(date, "PICARRO", date_sampling, "vExecuted.png", sep = "_")
    ggsave(file.path(folder, file_name), plot = G_S, width = l , height =h)
    Samples_subset <- subset(Samples, !(Samples %in% c("STD_1.1", "STD_1.2", "STD_2.1", "STD_2.2","C")))
    Results_samples <- data.frame(Samples=Samples_subset,Calibrated_H=H_results$Calibrated_H,Calibrated_O=O_results$Calibrated_O)
    Results_standard <-data.frame(Standards=R_H$STD_H,Analyzed_standard_H=R_H$Analyzed_standard_H,Real_standard_H=R_H$Real_standard_H,
                                  Analyzed_standard_O=R_O$Analyzed_standard_O,Real_standard_O=R_O$Real_standard_O)
    Controlli <- data.frame(Samples=Control_H$Samples,Control_Hydrogen=Control_H$Mean_hydrogen,
                            Control_Oxygen=Control_O$Mean_oxygen)
    Samples_path <- file.path(folder, paste(date, "Samples_results" ,date_sampling,"vExecuted.ods", sep = "_"))
    write.csv(Results_samples, file = Samples_path, row.names = FALSE)
    Standard_path <- file.path(folder, paste(date, "Standard_results",date_sampling,"vExecuted.ods", sep = "_"))
    write.csv(Results_standard, file = Standard_path, row.names = FALSE)
    Control_path <- file.path(folder, paste(date, "Control_results",date_sampling,"vExecuted.ods", sep = "_"))
    write.csv(Controlli, file = Control_path, row.names = FALSE)
    data_reactive$data_oxygen <- data_oxygen
    data_reactive$data_hydrogen <- data_hydrogen
    data_reactive$G_S <- G_S
    control_result_oxygen <- ifelse(all(abs(E_O) < 0.1),
                                    paste('<span style="color: green;">Good control samples for oxygen!</span>',
                                          ' Diff. O =', round(E_O, 2)),
                                    paste('<span style="color: red;">Bad control samples for oxygen!</span>',
                                          ' Diff. O =', round(E_O, 2)))
    control_result_hydrogen <- ifelse(all(abs(E_H) < 0.5),
                                      paste('<span style="color: green;">Good control samples for hydrogen!</span>',
                                            ' Diff. H =', round(E_H, 2)),
                                      paste('<span style="color: red;">Bad control samples for hydrogen!</span>',
                                            ' Diff. H =', round(E_H, 2)))
    data_reactive$control_result_oxygen <- control_result_oxygen
    data_reactive$control_result_hydrogen <- control_result_hydrogen
  })
  
  #Ouput app Shiny########################################################################################################################################################
  output$control_result_oxygen <- renderUI({
    HTML(data_reactive$control_result_oxygen)
  })
  output$control_result_hydrogen <- renderUI({
    HTML(data_reactive$control_result_hydrogen)
  })
  output$table_oxygen <- renderDT({
    datatable(data_reactive$data_oxygen, options = list(pageLength = 5, dom = 't'), rownames = FALSE) %>%
      formatStyle(
        names(data_reactive$data_oxygen),
        fontWeight = styleEqual(names(data_reactive$data_oxygen), "MSW_O", "bold")
      )
  })
  output$table_hydrogen <- renderDT({
    datatable(data_reactive$data_hydrogen, options = list(pageLength = 5, dom = 't'), rownames = FALSE) %>%
      formatStyle(
        names(data_reactive$data_hydrogen),
        fontWeight = styleEqual(names(data_reactive$data_hydrogen), "MSW_H", "bold")
      )
  })
  output$plot_G_S <- renderPlot({
    if (!is.null(data_reactive$G_S)) {
      grid.draw(data_reactive$G_S)
    }
  })
  
}
shinyApp(ui = ui, server = server, options = list(port = 8082))
