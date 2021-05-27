#Libraries
library(shiny)
library(tidyverse)
library(scales)
library(reshape2)

#Data
filenames <- c("Births.csv", "Deaths_1x1.csv", "E0per.csv", "fltper_1x1.csv", "mltper_1x1.csv", "Population.csv", "Countries.csv")
objectnames <- c("births", "deaths", "E0", "flt", "mlt", "pop", "countries")

for (i in 1:length(filenames)) {
  assign(objectnames[i], read.csv(filenames[i], sep = " ", dec = "."))
}

#Functions
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

#App
ui <- fluidPage(
  titlePanel("Mortality.org Data Exploration App"),
  sidebarLayout(
    sidebarPanel(
      selectInput("plotType",
        "Plot Type:",
        choices = c("Korfa", "Varhato szuleteskori elettartam valtozasa", "Szuletesek szamanak valtozasa", "Halalozasi korfa", "Alapmortalitas", "Varhato maradek elettartam", "Frailty matrix", "Internal mortality"),
        selected = "Korfa"
      ),
      selectInput("CCode",
        "Country code:",
        choices = countries$code,
        selected = "HUN"
      ),
      uiOutput("slider"),
      uiOutput("checkbox"),
      fluidRow(
        column(7,actionButton("stop","Stop")),
        column(3,actionButton("play","Play"))
      ),
      downloadLink("downloadData", "Download selected data")
    ),
    mainPanel(
      plotOutput("popPlot",height = "500px"),
      plotOutput("popPlot2",height = "500px")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
  output$slider <- renderUI({
    if (input$plotType %in% c("Korfa")) {
      tagList(
        sliderInput(
          inputId = "year", label = "Year:",
          min = min(filter(pop, c_code == input$CCode)$Year), max = max(filter(pop, c_code == input$CCode)$Year),
          value = max(filter(pop, c_code == input$CCode)$Year), step = 1
        )
      )
    } else if (input$plotType %in% c("Halalozasi korfa")) {
      tagList(
        sliderInput(
          inputId = "year", label = "Year:",
          min = min(filter(deaths, c_code == input$CCode)$Year), max = max(filter(deaths, c_code == input$CCode)$Year),
          value = max(filter(deaths, c_code == input$CCode)$Year), step = 1
        )
      )
    } else if (input$plotType %in% c("Alapmortalitas", "Varhato maradek elettartam", "Frailty matrix")) {
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
    if (input$plotType %in% c("Alapmortalitas")) {
      tagList(checkboxInput(inputId = "log", label = "Logaritmikus skala:"))
    }
  })

  output$popPlot <- renderPlot(
    {
      if (input$plotType == "Korfa") {
        ggplot(pop[pop$c_code == input$CCode & pop$Year == input$year, ]) +
          geom_col(aes(Age, Male), fill = "dodgerblue", col = "black") +
          geom_col(aes(Age, -Female), fill = "firebrick2", col = "black") +
          coord_flip() +
          theme_bw() +
          labs(y = "Nepesseg", x = "Eletkor", title = "Korfa") +
          theme(
            axis.text = element_text(size = 15),
            axis.title = element_text(size = 18, face = "bold"),
            title = element_text(size = 20),
            legend.position = c(0.25, 0.85),
            legend.background = element_rect(colour = "black"),
            legend.text = element_text(size = 14)
          )
      } else if (input$plotType == "Varhato szuleteskori elettartam valtozasa") {
        ggplot(E0[E0$c_code == input$CCode, ]) +
          geom_line(aes(Year, Male, col = "dodgerblue"), size = 1.5) +
          geom_line(aes(Year, Female, col = "firebrick2"), size = 1.5) +
          geom_line(aes(Year, Total, col = "black"), size = 1.5) +
          theme_bw() +
          labs(y = "E0", x = "Ev", title = "Varhato szuleteskori elettartam") +
          scale_color_manual(name = "Nem", labels = c("Osszesen", "Ferfi", "No"), values = c("black", "dodgerblue", "firebrick2")) +
          theme(
            axis.text = element_text(size = 15),
            axis.title = element_text(size = 18, face = "bold"),
            title = element_text(size = 20),
            legend.position = "top",
            legend.background = element_rect(colour = "black"),
            legend.text = element_text(size = 14)
          )
      } else if (input$plotType == "Szuletesek szamanak valtozasa") {
        ggplot(births[births$c_code == input$CCode, ]) +
          geom_line(aes(Year, Male, col = "dodgerblue"), size = 1.5) +
          geom_line(aes(Year, Female, col = "firebrick2"), size = 1.5) +
          geom_line(aes(Year, Total, col = "black"), size = 1.5) +
          theme_bw() +
          labs(y = "Szuletesek szama", x = "Ev", title = "Szuletesek szamanak valtozasa") +
          scale_color_manual(name = "Nem", labels = c("Osszesen", "Ferfi", "No"), values = c("black", "dodgerblue", "firebrick2")) +
          theme(
            axis.text = element_text(size = 15),
            axis.title = element_text(size = 18, face = "bold"),
            title = element_text(size = 20),
            legend.position = "top",
            legend.background = element_rect(colour = "black"),
            legend.text = element_text(size = 14)
          )
      } else if (input$plotType == "Halalozasi korfa") {
        ggplot(deaths[deaths$c_code == input$CCode & deaths$Year == input$year, ]) +
          geom_col(aes(Age, Male), fill = "dodgerblue", col = "black") +
          geom_col(aes(Age, -Female), fill = "firebrick2", col = "black") +
          coord_flip() +
          theme_bw() +
          labs(y = "Nepesseg", x = "Eletkor", title = "Korfa") +
          theme(
            axis.text = element_text(size = 15),
            axis.title = element_text(size = 18, face = "bold"),
            title = element_text(size = 20),
            legend.position = c(0.25, 0.85),
            legend.background = element_rect(colour = "black"),
            legend.text = element_text(size = 14)
          )
      } else if (input$plotType == "Alapmortalitas") {
        myPlot <- ggplot() +
          geom_line(data = mlt[mlt$c_code == input$CCode & mlt$Year == input$year, ], aes(x = Age, y = qx, col = "dodgerblue", lty = "solid"), size = 1.5) +
          geom_line(data = flt[flt$c_code == input$CCode & flt$Year == input$year, ], aes(x = Age, y = qx, col = "firebrick2", lty = "solid"), size = 1.5) +
          geom_line(data = mlt[mlt$c_code == input$CCode & mlt$Year == input$year, ], aes(x = Age, y = mx, col = "dodgerblue", lty = "dotted"), size = 1.5) +
          geom_line(data = flt[flt$c_code == input$CCode & flt$Year == input$year, ], aes(x = Age, y = mx, col = "firebrick2", lty = "dotted"), size = 1.5) +
          theme_bw() +
          labs(y = "Alapmortalitas", x = "Eletkor", title = "Alapmortalitas") +
          scale_color_manual(name = "Nem", labels = c("Ferfi", "No"), values = c("dodgerblue", "firebrick2")) +
          scale_linetype_manual(name = "Metodus", labels = c("mx", "qx"), values = c("dotted", "solid")) +
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
      } else if (input$plotType == "Varhato maradek elettartam") {
        ggplot() +
          geom_line(data = mlt[mlt$c_code == input$CCode & mlt$Year == input$year, ], aes(x = Age, y = ex, col = "dodgerblue"), size = 1.5) +
          geom_line(data = flt[flt$c_code == input$CCode & flt$Year == input$year, ], aes(x = Age, y = ex, col = "firebrick2"), size = 1.5) +
          theme_bw() +
          labs(y = "Alapmortalitas", x = "Eletkor", title = "Alapmortalitas") +
          scale_color_manual(name = "Nem", labels = c("Ferfi", "No"), values = c("dodgerblue", "firebrick2")) +
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
          labs(x = "Eletkor", y = "YPL", title = "A kor-YPL matrix", fill = "") +
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
    },
    height = 500,
    width = "auto"
  )

  output$popPlot2 <- renderPlot(
    {
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
    },
    height = 500,
    width = "auto"
  )
  
  output$downloadData <- downloadHandler(
    filename = function() {
      if(input$plotType=="Korfa"){
        paste("pop_", input$CCode, "_", str_sub(input$year, -2), ".csv", sep = "")
      }else if(input$plotType=="Varhato szuleteskori elettartam valtozasa"){
        paste("e0_", input$CCode, ".csv", sep = "")
      }else if(input$plotType=="Szuletesek szamanak valtozasa"){
        paste("births_", input$CCode, ".csv", sep = "")
      }else if(input$plotType=="Halalozasi korfa"){
        paste("deaths_", input$CCode, "_", str_sub(input$year, -2), ".csv", sep = "")
      }else if(input$plotType=="Alapmortalitas"){
        paste("mort_", input$CCode, "_", str_sub(input$year, -2), ".csv", sep = "")
      }else if(input$plotType=="Varhato maradek elettartam"){
        paste("ex_", input$CCode, "_", str_sub(input$year, -2), ".csv", sep = "")
      }else if(input$plotType== "Frailty matrix"){
        paste("matrix_", input$CCode, "_", str_sub(input$year, -2), ".csv", sep = "")
      }else if(input$plotType== "Internal mortality"){
        paste("mort_", input$CCode, "_", str_sub(input$year, -2), "_mod.csv", sep = "")
      }
    },
    content = function(file) {
      if(input$plotType=="Korfa"){
        write.table(pop[pop$c_code == input$CCode & pop$Year == input$year, ], file, sep = " ", dec = ".")
      }else if(input$plotType=="Varhato szuleteskori elettartam valtozasa"){
        write.table(E0[E0$c_code == input$CCode, ], file, sep = " ", dec = ".")
      }else if(input$plotType=="Szuletesek szamanak valtozasa"){
        write.table(births[births$c_code == input$CCode, ], file, sep = " ", dec = ".")
      }else if(input$plotType=="Halalozasi korfa"){
        write.table(deaths[deaths$c_code == input$CCode & deaths$Year == input$year, ], file, sep = " ", dec = ".")
      }else if(input$plotType=="Alapmortalitas"){
        dem <- pop %>%
          filter(c_code == CCode & Year == year) %>%
          inner_join(filter(mlt, c_code == CCode & Year == year), by = c("Age" = "Age")) %>%
          inner_join(filter(flt, c_code == CCode & Year == year), by = c("Age" = "Age")) %>%
          select(Age, Males = Male, Females = Female, Total, mort.Males = qx.x, mort.Females = qx.y)
        write.table(dem, file, sep = " ", dec = ".")
      }else if(input$plotType=="Varhato maradek elettartam"){
        dem <- pop %>%
          filter(c_code == CCode & Year == year) %>%
          inner_join(filter(mlt, c_code == CCode & Year == year), by = c("Age" = "Age")) %>%
          inner_join(filter(flt, c_code == CCode & Year == year), by = c("Age" = "Age")) %>%
          select(Age, Males = Male, Females = Female, Total, ex.Males = ex.x, ex.Females = ex.y)
        write.table(dem, file, sep = " ", dec = ".")
      }else if(input$plotType== "Frailty matrix"){
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
      }else if(input$plotType== "Internal mortality"){
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
  
  session2<-reactiveValues()
  session2$timer<-reactiveTimer(Inf)
  
  getNextYear<-function(obj) {
      if(input$year==max(filter(obj, c_code == input$CCode)$Year)){
        return(min(filter(obj, c_code == input$CCode)$Year))
      }else{
        return(input$year+1)  
      }
  }
  
  observeEvent(input$play,{
    session2$timer<-reactiveTimer(100)
    observeEvent(session2$timer(),{
      if(input$plotType=="Korfa"){
        updateSliderInput(session,
                          inputId = "year", 
                          label = "Year:",
                          min = min(filter(pop, c_code == input$CCode)$Year),
                          max = max(filter(pop, c_code == input$CCode)$Year),
                          value = getNextYear(pop),
                          step = 1)
      }else if(input$plotType=="Halalozasi korfa"){
        updateSliderInput(session,
                          inputId = "year", 
                          label = "Year:",
                          min = min(filter(deaths, c_code == input$CCode)$Year),
                          max = max(filter(deaths, c_code == input$CCode)$Year),
                          value = getNextYear(deaths),
                          step = 1)
      }else if(input$plotType %in% c("Alapmortalitas","Varhato maradek elettartam","Frailty matrix")){
        updateSliderInput(session,
                          inputId = "year", 
                          label = "Year:",
                          min = min(filter(mlt, c_code == input$CCode)$Year),
                          max = max(filter(mlt, c_code == input$CCode)$Year),
                          value = getNextYear(mlt),
                          step = 1)
      }
    })
  })
  
  observeEvent(input$stop,{
    session2$timer<-reactiveTimer(Inf)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
