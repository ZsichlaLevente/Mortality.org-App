# Libraries----
library(shiny)
library(tidyverse)
library(scales)
library(reshape2)
library(shinydlplot)

# Data----
filenames <- c("Births.csv", "Deaths_1x1.csv", "E0per.csv", "fltper_1x1.csv", "mltper_1x1.csv", "Population.csv", "Countries.csv")
objectnames <- c("births", "deaths", "E0", "flt", "mlt", "pop", "countries")

for (i in 1:length(filenames)) {
  assign(objectnames[i], read.csv(filenames[i], sep = " ", dec = "."))
}

# Functions----
deg2rad <- function(deg) {
  return((pi * deg) / 180)
}
rad2deg <- function(rad) {
  return((180 * rad) / pi)
}
dem.matrix <- function(v1, v2) {
  m <- matrix(rep(NA, length(v1) * length(v2)), nrow = length(v1), ncol = length(v2))
  return(m)
}
permillion <- function(dem){
  dem[, c("Total","Male","Female")] <- dem[, c("Total","Male","Female")] * 1e6 / sum(dem$Total)
  return(dem)
}

# App UI----
ui <- navbarPage(
  "Mortality.org Data Exploration App",
  tabPanel("Description",
           sidebarLayout(
             sidebarPanel(
               p("Contact: Soon...")
             ),
             mainPanel(
               p("Features: Soon...")
             )
           )
  ),
  tabPanel(
    "One country visualization",
    sidebarLayout(
      sidebarPanel(
        selectInput("plotType",
          "Plot Type:",
          choices = c("Age pyramid", "Life expectancy at birth", "Births", "Death pyramid", "Base mortality", "Life expectancy", "Frailty matrix", "Internal mortality"),
          selected = "Age pyramid"
        ),
        selectInput("CCode",
          "Country code:",
          choices = countries$code,
          selected = "HUN"
        ),
        uiOutput("slider"),
        uiOutput("checkbox"),
        fluidRow(
          column(7, actionButton("stop", "Stop")),
          column(3, actionButton("play", "Play"))
        ),
        downloadButton("downloadData", "Download selected data"),
        downloadButton('downloadPlot','Download Graph'),
      ),
      mainPanel(
        plotOutput("popPlot", height = "500px"),
        plotOutput("popPlot2", height = "500px")
      )
    )
  ),
  tabPanel("Comparison of 2 countries",
           sidebarLayout(
             sidebarPanel(
               selectInput("plotType21",
                           "Plot Type:",
                           choices = c("Age pyramid", "Life expectancy at birth", "Births", "Death pyramid", "Base mortality", "Life expectancy"),
                           selected = "Age pyramid"
               ),
               selectInput("CCode21",
                           "Country code1:",
                           choices = countries$code,
                           selected = "HUN"
               ),
               uiOutput("slider21"),
               selectInput("CCode22",
                           "Country code2:",
                           choices = countries$code,
                           selected = "AUT"
               ),
               uiOutput("slider22"),
               uiOutput("checkbox21"),
               downloadButton('downloadPlot2','Download Graph'),
             ),
             mainPanel(
               plotOutput("popPlot21", height = "500px")
             )
           )
        ),
  tabPanel("World map",
           sidebarLayout(
             sidebarPanel(),
             mainPanel()
           )
        )
)

# Server logic----
server <- function(input, output, session) {
  #tabPanel1
  output$slider <- renderUI({
    if (input$plotType %in% c("Age pyramid")) {
      tagList(
        sliderInput(
          inputId = "year", label = "Year:",
          min = min(filter(pop, c_code == input$CCode)$Year), max = max(filter(pop, c_code == input$CCode)$Year),
          value = max(filter(pop, c_code == input$CCode)$Year), step = 1
        )
      )
    } else if (input$plotType %in% c("Death pyramid")) {
      tagList(
        sliderInput(
          inputId = "year", label = "Year:",
          min = min(filter(deaths, c_code == input$CCode)$Year), max = max(filter(deaths, c_code == input$CCode)$Year),
          value = max(filter(deaths, c_code == input$CCode)$Year), step = 1
        )
      )
    } else if (input$plotType %in% c("Base mortality", "Life expectancy", "Frailty matrix")) {
      tagList(
        sliderInput(
          inputId = "year", label = "Year:",
          min = min(filter(mlt, c_code == input$CCode)$Year), max = max(filter(mlt, c_code == input$CCode)$Year),
          value = max(filter(mlt, c_code == input$CCode)$Year), step = 1
        )
      )
    } else if (input$plotType %in% c("Internal mortality")) {
      tagList(
        sliderInput(
          inputId = "year", label = "Year:",
          min = min(filter(mlt, c_code == input$CCode)$Year), max = max(filter(mlt, c_code == input$CCode)$Year),
          value = max(filter(mlt, c_code == input$CCode)$Year), step = 1
        ),
        numericInput("Mx", "Male x-coordinate:", 60, min = 50, max = 70, step = 0.01),
        numericInput("My", "Male y-coordinate:", -5, min = -10, max = -1, step = 0.01),
        numericInput("Fx", "Female x-coordinate:", 60, min = 50, max = 70, step = 0.01),
        numericInput("Fy", "Female y-coordinate:", -5, min = -10, max = -1, step = 0.01),
        sliderInput(inputId = "interval", label = "Regression interval:", min = 20, max = 70, value = c(40, 60), step = 1)
      )
    }
  })
  output$checkbox <- renderUI({
    if (input$plotType %in% c("Base mortality")) {
      tagList(checkboxInput(inputId = "log", label = "Logarithmic scale:"))
    }
  })
  plotInput11<-function(){
    
    if (input$plotType == "Age pyramid") {
      ggplot(pop[pop$c_code == input$CCode & pop$Year == input$year, ]) +
        geom_col(aes(Age, Male), fill = "dodgerblue", col = "black") +
        geom_col(aes(Age, -Female), fill = "firebrick2", col = "black") +
        coord_flip() +
        theme_bw() +
        labs(y = "Number of people", x = "Age", title = "Age pyramid") +
        theme(
          axis.text = element_text(size = 15),
          axis.title = element_text(size = 18, face = "bold"),
          title = element_text(size = 20),
          legend.position = c(0.25, 0.85),
          legend.background = element_rect(colour = "black"),
          legend.text = element_text(size = 14)
        )
    } else if (input$plotType == "Life expectancy at birth") {
      ggplot(E0[E0$c_code == input$CCode, ]) +
        geom_line(aes(Year, Male, col = "dodgerblue"), size = 1.5) +
        geom_line(aes(Year, Female, col = "firebrick2"), size = 1.5) +
        geom_line(aes(Year, Total, col = "black"), size = 1.5) +
        theme_bw() +
        labs(y = "E0", x = "Year", title = "Life expectancy at birth") +
        scale_color_manual(name = "Gender", labels = c("Total", "Male", "Female"), values = c("black", "dodgerblue", "firebrick2")) +
        theme(
          axis.text = element_text(size = 15),
          axis.title = element_text(size = 18, face = "bold"),
          title = element_text(size = 20),
          legend.position = "top",
          legend.background = element_rect(colour = "black"),
          legend.text = element_text(size = 14)
        )
    } else if (input$plotType == "Births") {
      ggplot(births[births$c_code == input$CCode, ]) +
        geom_line(aes(Year, Male, col = "dodgerblue"), size = 1.5) +
        geom_line(aes(Year, Female, col = "firebrick2"), size = 1.5) +
        geom_line(aes(Year, Total, col = "black"), size = 1.5) +
        theme_bw() +
        labs(y = "Number of births", x = "Year", title = "Births") +
        scale_color_manual(name = "Gender", labels = c("Total", "Male", "Female"), values = c("black", "dodgerblue", "firebrick2")) +
        theme(
          axis.text = element_text(size = 15),
          axis.title = element_text(size = 18, face = "bold"),
          title = element_text(size = 20),
          legend.position = "top",
          legend.background = element_rect(colour = "black"),
          legend.text = element_text(size = 14)
        )
    } else if (input$plotType == "Death pyramid") {
      ggplot(deaths[deaths$c_code == input$CCode & deaths$Year == input$year, ]) +
        geom_col(aes(Age, Male), fill = "dodgerblue", col = "black") +
        geom_col(aes(Age, -Female), fill = "firebrick2", col = "black") +
        coord_flip() +
        theme_bw() +
        labs(y = "Number of people", x = "Age", title = "Age pyramid") +
        theme(
          axis.text = element_text(size = 15),
          axis.title = element_text(size = 18, face = "bold"),
          title = element_text(size = 20),
          legend.position = c(0.25, 0.85),
          legend.background = element_rect(colour = "black"),
          legend.text = element_text(size = 14)
        )
    } else if (input$plotType == "Base mortality") {
      myPlot <- ggplot() +
        geom_line(data = mlt[mlt$c_code == input$CCode & mlt$Year == input$year, ], aes(x = Age, y = qx, col = "dodgerblue", lty = "solid"), size = 1.5) +
        geom_line(data = flt[flt$c_code == input$CCode & flt$Year == input$year, ], aes(x = Age, y = qx, col = "firebrick2", lty = "solid"), size = 1.5) +
        geom_line(data = mlt[mlt$c_code == input$CCode & mlt$Year == input$year, ], aes(x = Age, y = mx, col = "dodgerblue", lty = "dotted"), size = 1.5) +
        geom_line(data = flt[flt$c_code == input$CCode & flt$Year == input$year, ], aes(x = Age, y = mx, col = "firebrick2", lty = "dotted"), size = 1.5) +
        theme_bw() +
        labs(y = "Base mortality", x = "Age", title = "Base mortality") +
        scale_color_manual(name = "Gender", labels = c("Male", "Female"), values = c("dodgerblue", "firebrick2")) +
        scale_linetype_manual(name = "Method", labels = c("mx", "qx"), values = c("dotted", "solid")) +
        theme(
          axis.text = element_text(size = 15),
          axis.title = element_text(size = 18, face = "bold"),
          title = element_text(size = 20),
          legend.position = "top",
          legend.background = element_rect(colour = "black"),
          legend.text = element_text(size = 14)
        )
      if (input$log) {
        myPlot <- myPlot + scale_y_log10(
          breaks = trans_breaks("log10", function(x) 10^x),
          labels = trans_format("log10", math_format(10^.x))
        )
      }
      myPlot
    } else if (input$plotType == "Life expectancy") {
      ggplot() +
        geom_line(data = mlt[mlt$c_code == input$CCode & mlt$Year == input$year, ], aes(x = Age, y = ex, col = "dodgerblue"), size = 1.5) +
        geom_line(data = flt[flt$c_code == input$CCode & flt$Year == input$year, ], aes(x = Age, y = ex, col = "firebrick2"), size = 1.5) +
        theme_bw() +
        labs(y = "Base mortality", x = "Age", title = "Base mortality") +
        scale_color_manual(name = "Gender", labels = c("Male", "Female"), values = c("dodgerblue", "firebrick2")) +
        theme(
          axis.text = element_text(size = 15),
          axis.title = element_text(size = 18, face = "bold"),
          title = element_text(size = 20),
          legend.position = "top",
          legend.background = element_rect(colour = "black"),
          legend.text = element_text(size = 14)
        )
    } else if (input$plotType == "Frailty matrix") {
      
      # Main code
      CCode <- input$CCode
      year <- input$year
      
      # Filtered tibble
      dem <- pop %>%
        filter(c_code == CCode & Year == year) %>%
        inner_join(filter(mlt, c_code == CCode & Year == year), by = c("Age" = "Age")) %>%
        inner_join(filter(flt, c_code == CCode & Year == year), by = c("Age" = "Age")) %>%
        select(Age, Males = Male, Females = Female, Total, mort.Males = qx.x, mort.Females = qx.y)
      
      age <- min(dem$Age):max(dem$Age)
      YPL <- age
      
      frailty_matrix_male <- dem.matrix(YPL, age)
      frailty_matrix_female <- dem.matrix(YPL, age)
      
      frailty_matrix_male[1, ] <- dem$Males * dem$mort.Males
      frailty_matrix_female[1, ] <- dem$Females * dem$mort.Females
      
      for (R in 2:length(YPL)) {
        for (C in 1:length(age)) {
          if ((C + R - 1) > length(dem$Age)) {
            frailty_matrix_male[R, C] <- (dem$Males[C] - sum(frailty_matrix_male[, C][1:(R - 1)])) * dem$mort.Males[length(dem$Age)]
            frailty_matrix_female[R, C] <- (dem$Females[C] - sum(frailty_matrix_female[, C][1:(R - 1)])) * dem$mort.Females[length(dem$Age)]
          } else {
            frailty_matrix_male[R, C] <- (dem$Males[C] - sum(frailty_matrix_male[, C][1:(R - 1)])) * dem$mort.Males[C + R - 1]
            frailty_matrix_female[R, C] <- (dem$Females[C] - sum(frailty_matrix_female[, C][1:(R - 1)])) * dem$mort.Females[C + R - 1]
          }
        }
      }
      
      frailty_matrix_tot <- melt(t(frailty_matrix_male) + t(frailty_matrix_female))
      colnames(frailty_matrix_tot) <- c("Age", "YPL", "value")
      
      ggplot(data = frailty_matrix_tot, aes(x = (Age + age[1] - 1), y = (YPL - 1))) +
        geom_raster(aes(fill = value)) +
        theme_bw() +
        labs(x = "Age", y = "YPL", title = "Age-YPL matrix", fill = "") +
        theme(
          axis.text = element_text(size = 15),
          axis.title = element_text(size = 18, face = "bold"),
          title = element_text(size = 20),
          legend.text = element_text(size = 12),
          legend.position = "right"
        ) +
        scale_fill_gradient2(low = "black", high = "red", mid = "purple", midpoint = max(frailty_matrix_tot$value) / 2) +
        lims(x = c(age[1] - 1, age[length(age)] + 1))
    } else if (input$plotType == "Internal mortality") {
      # Main code
      CCode <- input$CCode
      year <- input$year
      point_male <- c(input$Mx, input$My)
      point_female <- c(input$Fx, input$Fy)
      reg_interval <- input$interval[1]:input$interval[2]
      
      # Filtered tibble
      dem <- pop %>%
        filter(c_code == CCode & Year == year) %>%
        inner_join(filter(mlt, c_code == CCode & Year == year), by = c("Age" = "Age")) %>%
        inner_join(filter(flt, c_code == CCode & Year == year), by = c("Age" = "Age")) %>%
        select(Age, Total, Males = Male, Females = Female, mort.Males = qx.x, mort.Females = qx.y)
      
      reg_male <- lm(log(dem$mort.Males[reg_interval + 1]) ~ dem$Age[reg_interval + 1])
      reg_female <- lm(log(dem$mort.Females[reg_interval + 1]) ~ dem$Age[reg_interval + 1])
      
      steepness_male <- tan(deg2rad(90 - (90 - rad2deg(atan(reg_male$coefficients[2])) - 1)))
      steepness_female <- tan(deg2rad(90 - (90 - rad2deg(atan(reg_female$coefficients[2])) - 0.7)))
      
      intercept_male <- -1 * (steepness_male * point_male[1]) + point_male[2]
      intercept_female <- -1 * (steepness_female * point_female[1]) + point_female[2]
      
      internal_male_20_60 <- steepness_male * 20:60 + intercept_male
      internal_female_20_60 <- steepness_female * 20:60 + intercept_female
      
      par(mar = c(4.1, 5.1, 3.1, 1.1))
      plot(dem$Age, log(dem$mort.Males), pch = 16, cex = 1.3, xlab = "Age", ylab = "log(Mortality)", main = "Males", cex.lab = 2, cex.axis = 2, cex.main = 2)
      abline(a = intercept_male, b = steepness_male, lwd = 2, col = "blue")
      points(20:60, internal_male_20_60, pch = 16, col = "blue", cex = 1.3)
      abline(a = reg_male$coefficients[1], b = reg_male$coefficients[2], lwd = 2)
      points(point_male[1], point_male[2], cex = 3, pch = 18, col = "purple")
    }
    
  }
  output$popPlot <- renderPlot(plotInput11(),
    height = 500,
    width = "auto"
  )
  plotInput12<-function(){
    if (input$plotType == "Internal mortality") {
      # Main code
      CCode <- input$CCode
      year <- input$year
      point_male <- c(input$Mx, input$My)
      point_female <- c(input$Fx, input$Fy)
      reg_interval <- input$interval[1]:input$interval[2]
      
      # Filtered tibble
      dem <- pop %>%
        filter(c_code == CCode & Year == year) %>%
        inner_join(filter(mlt, c_code == CCode & Year == year), by = c("Age" = "Age")) %>%
        inner_join(filter(flt, c_code == CCode & Year == year), by = c("Age" = "Age")) %>%
        select(Age, Total, Males = Male, Females = Female, mort.Males = qx.x, mort.Females = qx.y)
      
      reg_male <- lm(log(dem$mort.Males[reg_interval + 1]) ~ dem$Age[reg_interval + 1])
      reg_female <- lm(log(dem$mort.Females[reg_interval + 1]) ~ dem$Age[reg_interval + 1])
      
      steepness_male <- tan(deg2rad(90 - (90 - rad2deg(atan(reg_male$coefficients[2])) - 1)))
      steepness_female <- tan(deg2rad(90 - (90 - rad2deg(atan(reg_female$coefficients[2])) - 0.7)))
      
      intercept_male <- -1 * (steepness_male * point_male[1]) + point_male[2]
      intercept_female <- -1 * (steepness_female * point_female[1]) + point_female[2]
      
      internal_male_20_60 <- steepness_male * 20:60 + intercept_male
      internal_female_20_60 <- steepness_female * 20:60 + intercept_female
      
      par(mar = c(4.1, 5.1, 3.1, 1.1))
      plot(dem$Age, log(dem$mort.Females), pch = 16, cex = 1.3, xlab = "Age", ylab = "log(Mortality)", main = "Females", cex.lab = 2, cex.axis = 2, cex.main = 2)
      abline(a = intercept_female, b = steepness_female, lwd = 2, col = "red")
      points(20:60, internal_female_20_60, pch = 16, col = "red", cex = 1.3)
      abline(a = reg_female$coefficients[1], b = reg_female$coefficients[2], lwd = 2)
      points(point_female[1], point_female[2], cex = 3, pch = 18, col = "purple")
    }
    
  }
  output$popPlot2 <- renderPlot(plotInput12(),
    height = 500,
    width = "auto"
  )
  output$downloadData <- downloadHandler(
    filename = function() {
      if (input$plotType == "Age pyramid") {
        paste("pop_", input$CCode, "_", str_sub(input$year, -2), ".csv", sep = "")
      } else if (input$plotType == "Life expectancy at birth") {
        paste("e0_", input$CCode, ".csv", sep = "")
      } else if (input$plotType == "Births") {
        paste("births_", input$CCode, ".csv", sep = "")
      } else if (input$plotType == "Death pyramid") {
        paste("deaths_", input$CCode, "_", str_sub(input$year, -2), ".csv", sep = "")
      } else if (input$plotType == "Base mortality") {
        paste("mort_", input$CCode, "_", str_sub(input$year, -2), ".csv", sep = "")
      } else if (input$plotType == "Life expectancy") {
        paste("ex_", input$CCode, "_", str_sub(input$year, -2), ".csv", sep = "")
      } else if (input$plotType == "Frailty matrix") {
        paste("matrix_", input$CCode, "_", str_sub(input$year, -2), ".csv", sep = "")
      } else if (input$plotType == "Internal mortality") {
        paste("mort_", input$CCode, "_", str_sub(input$year, -2), "_mod.csv", sep = "")
      }
    },
    content = function(file) {
      if (input$plotType == "Age pyramid") {
        write.table(pop[pop$c_code == input$CCode & pop$Year == input$year, ], file, sep = " ", dec = ".")
      } else if (input$plotType == "Life expectancy at birth") {
        write.table(E0[E0$c_code == input$CCode, ], file, sep = " ", dec = ".")
      } else if (input$plotType == "Births") {
        write.table(births[births$c_code == input$CCode, ], file, sep = " ", dec = ".")
      } else if (input$plotType == "Death pyramid") {
        write.table(deaths[deaths$c_code == input$CCode & deaths$Year == input$year, ], file, sep = " ", dec = ".")
      } else if (input$plotType == "Base mortality") {
        CCode <- input$CCode
        year <- input$year
        dem <- pop %>%
          filter(c_code == CCode & Year == year) %>%
          inner_join(filter(mlt, c_code == CCode & Year == year), by = c("Age" = "Age")) %>%
          inner_join(filter(flt, c_code == CCode & Year == year), by = c("Age" = "Age")) %>%
          select(Age, Males = Male, Females = Female, Total, mort.Males = qx.x, mort.Females = qx.y)
        write.table(dem, file, sep = " ", dec = ".")
      } else if (input$plotType == "Life expectancy") {
        CCode <- input$CCode
        year <- input$year
        dem <- pop %>%
          filter(c_code == CCode & Year == year) %>%
          inner_join(filter(mlt, c_code == CCode & Year == year), by = c("Age" = "Age")) %>%
          inner_join(filter(flt, c_code == CCode & Year == year), by = c("Age" = "Age")) %>%
          select(Age, Males = Male, Females = Female, Total, ex.Males = ex.x, ex.Females = ex.y)
        write.table(dem, file, sep = " ", dec = ".")
      } else if (input$plotType == "Frailty matrix") {
        # Main code
        CCode <- input$CCode
        year <- input$year

        # Filtered tibble
        dem <- pop %>%
          filter(c_code == CCode & Year == year) %>%
          inner_join(filter(mlt, c_code == CCode & Year == year), by = c("Age" = "Age")) %>%
          inner_join(filter(flt, c_code == CCode & Year == year), by = c("Age" = "Age")) %>%
          select(Age, Males = Male, Females = Female, Total, mort.Males = qx.x, mort.Females = qx.y)

        age <- min(dem$Age):max(dem$Age)
        YPL <- age

        frailty_matrix_male <- dem.matrix(YPL, age)
        frailty_matrix_female <- dem.matrix(YPL, age)

        frailty_matrix_male[1, ] <- dem$Males * dem$mort.Males
        frailty_matrix_female[1, ] <- dem$Females * dem$mort.Females

        for (R in 2:length(YPL)) {
          for (C in 1:length(age)) {
            if ((C + R - 1) > length(dem$Age)) {
              frailty_matrix_male[R, C] <- (dem$Males[C] - sum(frailty_matrix_male[, C][1:(R - 1)])) * dem$mort.Males[length(dem$Age)]
              frailty_matrix_female[R, C] <- (dem$Females[C] - sum(frailty_matrix_female[, C][1:(R - 1)])) * dem$mort.Females[length(dem$Age)]
            } else {
              frailty_matrix_male[R, C] <- (dem$Males[C] - sum(frailty_matrix_male[, C][1:(R - 1)])) * dem$mort.Males[C + R - 1]
              frailty_matrix_female[R, C] <- (dem$Females[C] - sum(frailty_matrix_female[, C][1:(R - 1)])) * dem$mort.Females[C + R - 1]
            }
          }
        }

        frailty_matrix_tot <- melt(t(frailty_matrix_male) + t(frailty_matrix_female))
        colnames(frailty_matrix_tot) <- c("Age", "YPL", "value")

        write.table(frailty_matrix_tot, file, sep = " ", dec = ".")
      } else if (input$plotType == "Internal mortality") {
        # Main code
        CCode <- input$CCode
        year <- input$year
        point_male <- c(input$Mx, input$My)
        point_female <- c(input$Fx, input$Fy)
        reg_interval <- input$interval[1]:input$interval[2]

        # Filtered tibble
        dem <- pop %>%
          filter(c_code == CCode & Year == year) %>%
          inner_join(filter(mlt, c_code == CCode & Year == year), by = c("Age" = "Age")) %>%
          inner_join(filter(flt, c_code == CCode & Year == year), by = c("Age" = "Age")) %>%
          select(Age, Total, Males = Male, Females = Female, mort.Males = qx.x, mort.Females = qx.y)

        reg_male <- lm(log(dem$mort.Males[reg_interval + 1]) ~ dem$Age[reg_interval + 1])
        reg_female <- lm(log(dem$mort.Females[reg_interval + 1]) ~ dem$Age[reg_interval + 1])

        steepness_male <- tan(deg2rad(90 - (90 - rad2deg(atan(reg_male$coefficients[2])) - 1)))
        steepness_female <- tan(deg2rad(90 - (90 - rad2deg(atan(reg_female$coefficients[2])) - 0.7)))

        intercept_male <- -1 * (steepness_male * point_male[1]) + point_male[2]
        intercept_female <- -1 * (steepness_female * point_female[1]) + point_female[2]

        internal_male_20_60 <- steepness_male * 20:60 + intercept_male
        internal_female_20_60 <- steepness_female * 20:60 + intercept_female

        internal_mort_male <- c(exp(internal_male_20_60), dem$mort.Males[62:100])
        internal_mort_female <- c(exp(internal_female_20_60), dem$mort.Females[62:100])

        dem_mod <<- data.frame(dem$Age[21:100], dem$Total[21:100], dem$Male[21:100], dem$Female[21:100], internal_mort_male, internal_mort_female)
        colnames(dem_mod) <<- c("Age", "Total", "Males", "Females", "mort.Males", "mort.Females")
        write.table(dem_mod, file, sep = " ", dec = ".")
      }
    }
  )
  output$downloadPlot <- downloadHandler(
    filename <- function() {
      if (input$plotType == "Age pyramid") {
        paste("pop_", input$CCode, "_", str_sub(input$year, -2), ".png", sep = "")
      } else if (input$plotType == "Life expectancy at birth") {
        paste("e0_", input$CCode, ".png", sep = "")
      } else if (input$plotType == "Births") {
        paste("births_", input$CCode, ".png", sep = "")
      } else if (input$plotType == "Death pyramid") {
        paste("deaths_", input$CCode, "_", str_sub(input$year, -2), ".png", sep = "")
      } else if (input$plotType == "Base mortality") {
        paste("mort_", input$CCode, "_", str_sub(input$year, -2), ".png", sep = "")
      } else if (input$plotType == "Life expectancy") {
        paste("ex_", input$CCode, "_", str_sub(input$year, -2), ".png", sep = "")
      } else if (input$plotType == "Frailty matrix") {
        paste("matrix_", input$CCode, "_", str_sub(input$year, -2), ".png", sep = "")
      } else if (input$plotType == "Internal mortality") {
        paste("mort_", input$CCode, "_", str_sub(input$year, -2), "_mod.png", sep = "")
      }
    },
    content <- function(file) {
      png(file, width = 1000, height = 750,units="px", pointsize = 12,
          bg = "white", res = NA)
      plot1<-plotInput11()
      print(plot1)
      dev.off()},
    contentType = "image/png",
  )
  session2 <- reactiveValues()
  session2$timer <- reactiveTimer(Inf)
  getNextYear <- function(obj) {
    if (input$year == max(filter(obj, c_code == input$CCode)$Year)) {
      return(min(filter(obj, c_code == input$CCode)$Year))
    } else {
      return(input$year + 1)
    }
  }
  observeEvent(input$play, {
    session2$timer <- reactiveTimer(100)
    observeEvent(session2$timer(), {
      if (input$plotType == "Age pyramid") {
        updateSliderInput(session,
          inputId = "year",
          label = "Year:",
          min = min(filter(pop, c_code == input$CCode)$Year),
          max = max(filter(pop, c_code == input$CCode)$Year),
          value = getNextYear(pop),
          step = 1
        )
      } else if (input$plotType == "Death pyramid") {
        updateSliderInput(session,
          inputId = "year",
          label = "Year:",
          min = min(filter(deaths, c_code == input$CCode)$Year),
          max = max(filter(deaths, c_code == input$CCode)$Year),
          value = getNextYear(deaths),
          step = 1
        )
      } else if (input$plotType %in% c("Base mortality", "Life expectancy", "Frailty matrix")) {
        updateSliderInput(session,
          inputId = "year",
          label = "Year:",
          min = min(filter(mlt, c_code == input$CCode)$Year),
          max = max(filter(mlt, c_code == input$CCode)$Year),
          value = getNextYear(mlt),
          step = 1
        )
      }
    })
  })
  observeEvent(input$stop, {
    session2$timer <- reactiveTimer(Inf)
  })
  
  #tabPanel2
  output$slider21 <- renderUI({
    if (input$plotType21 %in% c("Age pyramid")) {
      tagList(
        sliderInput(
          inputId = "year21", label = "Year1:",
          min = min(filter(pop, c_code == input$CCode21)$Year), max = max(filter(pop, c_code == input$CCode21)$Year),
          value = max(filter(pop, c_code == input$CCode21)$Year), step = 1
        )
      )
    } else if (input$plotType21 %in% c("Death pyramid")) {
      tagList(
        sliderInput(
          inputId = "year21", label = "Year:",
          min = min(filter(deaths, c_code == input$CCode21)$Year), max = max(filter(deaths, c_code == input$CCode21)$Year),
          value = max(filter(deaths, c_code == input$CCode21)$Year), step = 1
        )
      )
    } else if (input$plotType21 %in% c("Base mortality")) {
      tagList(
        sliderInput(
          inputId = "year21", label = "Year:",
          min = min(filter(mlt, c_code == input$CCode21)$Year), max = max(filter(mlt, c_code == input$CCode21)$Year),
          value = max(filter(mlt, c_code == input$CCode21)$Year), step = 1
        )
      )
    }
    else if (input$plotType21 %in% c("Life expectancy")) {
      tagList(
        sliderInput(
          inputId = "year21", label = "Year:",
          min = min(filter(mlt, c_code == input$CCode21)$Year), max = max(filter(mlt, c_code == input$CCode21)$Year),
          value = max(filter(mlt, c_code == input$CCode21)$Year), step = 1
        )
      )
    }
  })
  output$slider22 <- renderUI({
    if (input$plotType21 %in% c("Age pyramid")) {
      tagList(
        sliderInput(
          inputId = "year22", label = "Year2:",
          min = min(filter(pop, c_code == input$CCode22)$Year), max = max(filter(pop, c_code == input$CCode22)$Year),
          value = max(filter(pop, c_code == input$CCode22)$Year), step = 1
        )
      )
    } else if (input$plotType21 %in% c("Death pyramid")) {
      tagList(
        sliderInput(
          inputId = "year22", label = "Year:",
          min = min(filter(deaths, c_code == input$CCode22)$Year), max = max(filter(deaths, c_code == input$CCode22)$Year),
          value = max(filter(deaths, c_code == input$CCode22)$Year), step = 1
        )
      )
    } else if (input$plotType21 %in% c("Life expectancy")) {
      tagList(
        sliderInput(
          inputId = "year22", label = "Year:",
          min = min(filter(mlt, c_code == input$CCode22)$Year), max = max(filter(mlt, c_code == input$CCode22)$Year),
          value = max(filter(mlt, c_code == input$CCode22)$Year), step = 1
        )
      )
    }else if (input$plotType21 %in% c("Base mortality")) {
      tagList(
        sliderInput(
          inputId = "year22", label = "Year:",
          min = min(filter(mlt, c_code == input$CCode22)$Year), max = max(filter(mlt, c_code == input$CCode22)$Year),
          value = max(filter(mlt, c_code == input$CCode22)$Year), step = 1
        ),
        radioButtons(inputId="method", label="Mortality calculation method:",choices=c("Qx" = "qx","Mx" = "mx"),selected = "qx")
      )
    }
  })
  output$checkbox21 <- renderUI({
    if (input$plotType21 %in% c("Base mortality")) {
      tagList(checkboxInput(inputId = "log21", label = "Logarithmic scale:"))
    }
  })
  plotInput21<-function(){
    if (input$plotType21 == "Age pyramid") {
      dem1<-pop[pop$c_code == input$CCode21 & pop$Year == input$year21, ]
      dem2<-pop[pop$c_code == input$CCode22 & pop$Year == input$year22, ]
      ggplot() +
        geom_col(data=permillion(dem2),
                 aes(Age, Male+Female, fill = "Country2 - female"), col = "black") +
        geom_col(data=permillion(dem2),
                 aes(Age, Male, fill = "Country2 - male"), col = "black") +
        geom_col(data=permillion(dem1),
                 aes(Age, -(Male+Female), fill = "Country1 - female"), col = "black") +
        geom_col(data=permillion(dem1),
                 aes(Age, -(Male), fill = "Country1 - male"), col = "black") +
        scale_fill_manual("Legend",
                          values=c("Country1 - female"="firebrick1",
                                   "Country1 - male"= "dodgerblue",
                                   "Country2 - female"= "firebrick3",
                                   "Country2 - male"="deepskyblue3"))+
        coord_flip() +
        theme_bw() +
        labs(y = "Number of people", x = "Age", title = "Age pyramid [/1M people]") +
        theme(
          axis.text = element_text(size = 15),
          axis.title = element_text(size = 18, face = "bold"),
          title = element_text(size = 20),
          legend.position = c(0.15, 0.85),
          legend.background = element_rect(colour = "black"),
          legend.text = element_text(size = 14)
        )
    } else if (input$plotType21 == "Life expectancy at birth") {
      ggplot(data=E0[E0$c_code == input$CCode21, ]) +
        geom_line(aes(Year, Male, col = "Country1 - male"), size = 1.5) +
        geom_line(aes(Year, Female, col = "Country1 - female"), size = 1.5) +
        geom_line(aes(Year, Total, col = "Country1 - both"), size = 1.5) +
        geom_line(data=E0[E0$c_code == input$CCode22, ],aes(Year, Male, col = "Country2 - male"), size = 1.5) +
        geom_line(data=E0[E0$c_code == input$CCode22, ],aes(Year, Female, col = "Country2 - female"), size = 1.5) +
        geom_line(data=E0[E0$c_code == input$CCode22, ],aes(Year, Total, col = "Country2 - both"), size = 1.5) +
        theme_bw() +
        labs(y = "E0", x = "Year", title = "Life expectancy at birth") +
        scale_color_manual(name = "Country & Gender", values=c("Country1 - female"="firebrick1",
                                                               "Country1 - male"= "dodgerblue",
                                                               "Country1 - both"= "gray",
                                                               "Country2 - male"="blue4",
                                                               "Country2 - female"= "firebrick4",
                                                               "Country2 - both"="black")) +
        theme(
          axis.text = element_text(size = 15),
          axis.title = element_text(size = 18, face = "bold"),
          title = element_text(size = 20),
          legend.position = "top",
          legend.background = element_rect(colour = "black"),
          legend.text = element_text(size = 14)
        )
    } else if (input$plotType21 == "Births") {
      b1pm<-permillion(births[births$c_code == input$CCode21, ])
      b2pm<-permillion(births[births$c_code == input$CCode22, ])
      ggplot(data=b1pm) +
        geom_line(aes(Year, Male, col = "Country1 - male"), size = 1.5) +
        geom_line(aes(Year, Female, col = "Country1 - female"), size = 1.5) +
        geom_line(aes(Year, Total, col = "Country1 - both"), size = 1.5) +
        geom_line(data=b2pm,aes(Year, Male, col = "Country2 - male"), size = 1.5) +
        geom_line(data=b2pm,aes(Year, Female, col = "Country2 - female"), size = 1.5) +
        geom_line(data=b2pm,aes(Year, Total, col = "Country2 - both"), size = 1.5) +
        theme_bw() +
        labs(y = "Number of births", x = "Year", title = "Births [/1M people]") +
        scale_color_manual(name = "Country & Gender", values=c("Country1 - female"="firebrick1",
                                                               "Country1 - male"= "dodgerblue",
                                                               "Country1 - both"= "gray",
                                                               "Country2 - male"="blue4",
                                                               "Country2 - female"= "firebrick4",
                                                               "Country2 - both"="black")) +
        theme(
          axis.text = element_text(size = 15),
          axis.title = element_text(size = 18, face = "bold"),
          title = element_text(size = 20),
          legend.position = "top",
          legend.background = element_rect(colour = "black"),
          legend.text = element_text(size = 14)
        )
    } else if (input$plotType21 == "Death pyramid") {
      dem1<-deaths[deaths$c_code == input$CCode21 & deaths$Year == input$year21, ]
      dem2<-deaths[deaths$c_code == input$CCode22 & deaths$Year == input$year22, ]
      ggplot() +
        geom_col(data=permillion(dem2),
                 aes(Age, Male+Female, fill = "Country2 - female"), col = "black") +
        geom_col(data=permillion(dem2),
                 aes(Age, Male, fill = "Country2 - male"), col = "black") +
        geom_col(data=permillion(dem1),
                 aes(Age, -(Male+Female), fill = "Country1 - female"), col = "black") +
        geom_col(data=permillion(dem1),
                 aes(Age, -(Male), fill = "Country1 - male"), col = "black") +
        scale_fill_manual("Legend",
                          values=c("Country1 - female"="firebrick1",
                                   "Country1 - male"= "dodgerblue",
                                   "Country2 - female"= "firebrick3",
                                   "Country2 - male"="deepskyblue3"))+
        coord_flip() +
        theme_bw() +
        labs(y = "Number of people", x = "Age", title = "Death pyramid [/1M people]") +
        theme(
          axis.text = element_text(size = 15),
          axis.title = element_text(size = 18, face = "bold"),
          title = element_text(size = 20),
          legend.position = c(0.15, 0.15),
          legend.background = element_rect(colour = "black"),
          legend.text = element_text(size = 14)
        )
    } else if (input$plotType21 == "Base mortality") {
      mlt1<-mlt[mlt$c_code == input$CCode21 & mlt$Year == input$year21, ]
      flt1<-flt[flt$c_code == input$CCode21 & flt$Year == input$year21, ]
      mlt2<-mlt[mlt$c_code == input$CCode22 & mlt$Year == input$year22, ]
      flt2<-flt[flt$c_code == input$CCode22 & flt$Year == input$year22, ]
      if(input$method=="mx"){
        myPlot <- ggplot() +
          geom_line(data = mlt1 , aes(x = Age, y = mx, col = "Country1 - male"), size = 1.5) +
          geom_line(data = flt1, aes(x = Age, y = mx, col = "Country1 - female"), size = 1.5)+
          geom_line(data = mlt2 , aes(x = Age, y = mx, col = "Country2 - male"), size = 1.5) +
          geom_line(data = flt2, aes(x = Age, y = mx, col = "Country2 - female"), size = 1.5)
      }else{
        myPlot <- ggplot() +
          geom_line(data = mlt1 , aes(x = Age, y = qx, col = "Country1 - male"), size = 1.5) +
          geom_line(data = flt1, aes(x = Age, y = qx, col = "Country1 - female"), size = 1.5)+
          geom_line(data = mlt2 , aes(x = Age, y = qx, col = "Country2 - male"), size = 1.5) +
          geom_line(data = flt2, aes(x = Age, y = qx, col = "Country2 - female"), size = 1.5)
      }
      myPlot<-myPlot+
        theme_bw() +
        labs(y = "Base mortality", x = "Age", title = "Base mortality") +
        scale_color_manual(name = "Country & Gender",values = c("Country1 - male"="dodgerblue", 
                                                                "Country1 - female"="firebrick2",
                                                                "Country2 - male"="blue4",
                                                                "Country2 - female"="red4")) +
        theme(
          axis.text = element_text(size = 15),
          axis.title = element_text(size = 18, face = "bold"),
          title = element_text(size = 20),
          legend.position = "top",
          legend.background = element_rect(colour = "black"),
          legend.text = element_text(size = 14)
        )
      if (input$log21) {
        myPlot <- myPlot + scale_y_log10(
          breaks = trans_breaks("log10", function(x) 10^x),
          labels = trans_format("log10", math_format(10^.x))
        )
      }
      myPlot
    } else if (input$plotType21 == "Life expectancy") {
      mlt1<-mlt[mlt$c_code == input$CCode21 & mlt$Year == input$year21, ]
      flt1<-flt[flt$c_code == input$CCode21 & flt$Year == input$year21, ]
      mlt2<-mlt[mlt$c_code == input$CCode22 & mlt$Year == input$year22, ]
      flt2<-flt[flt$c_code == input$CCode22 & flt$Year == input$year22, ]
      ggplot() +
        geom_line(data = mlt1 , aes(x = Age, y = ex, col = "Country1 - male"), size = 1.5) +
        geom_line(data = flt1, aes(x = Age, y = ex, col = "Country1 - female"), size = 1.5)+
        geom_line(data = mlt2 , aes(x = Age, y = ex, col = "Country2 - male"), size = 1.5) +
        geom_line(data = flt2, aes(x = Age, y = ex, col = "Country2 - female"), size = 1.5)+
        theme_bw() +
        labs(y = "Base mortality", x = "Age", title = "Base mortality") +
        scale_color_manual(name = "Country & Gender",values = c("Country1 - male"="dodgerblue", 
                                                                "Country1 - female"="firebrick2",
                                                                "Country2 - male"="blue4",
                                                                "Country2 - female"="red4")) +
        theme(
          axis.text = element_text(size = 15),
          axis.title = element_text(size = 18, face = "bold"),
          title = element_text(size = 20),
          legend.position = "top",
          legend.background = element_rect(colour = "black"),
          legend.text = element_text(size = 14)
        )
    }
  }
  output$popPlot21 <- renderPlot(
    plotInput21(),
    height = 500,
    width = "auto"
  )
  output$downloadPlot2 <- downloadHandler(
    filename <- function() {
      if (input$plotType21 == "Age pyramid") {
        paste("pop_", input$CCode21, "_", str_sub(input$year21, -2), input$CCode22, "_", str_sub(input$year22, -2), ".png", sep = "")
      } else if (input$plotType21 == "Life expectancy at birth") {
        paste("e0_", input$CCode21,"_", input$CCode22, ".png", sep = "")
      } else if (input$plotType21 == "Births") {
        paste("births_", input$CCode21,"_", input$CCode22, ".png", sep = "")
      } else if (input$plotType21 == "Death pyramid") {
        paste("deaths_", input$CCode21, "_", str_sub(input$year21, -2), input$CCode22, "_", str_sub(input$year22, -2), ".png", sep = "")
      } else if (input$plotType21 == "Base mortality") {
        paste("mort_", input$CCode21, "_", str_sub(input$year21, -2), input$CCode22, "_", str_sub(input$year22, -2), ".png", sep = "")
      } else if (input$plotType21 == "Life expectancy") {
        paste("ex_", input$CCode21, "_", str_sub(input$year21, -2), input$CCode22, "_", str_sub(input$year22, -2), ".png", sep = "")
      }
    },
    content <- function(file) {
      png(file, width = 1000, height = 750,units="px", pointsize = 12,
          bg = "white", res = NA)
      plot1<-plotInput21()
      print(plot1)
      dev.off()},
    contentType = "image/png",
  )

}

# Run the application----
shinyApp(ui = ui, server = server)
