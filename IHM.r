library(ggplot2)
library(dplyr)
library(scales)
library(shiny)

ui <- bootstrapPage(
    titlePanel("Envoyer un fichier"),
    fileInput("file1", "Choisir un fichier CSV",
        multiple = FALSE, accept = c("text/csv",
        "text/comma-separated-values,text/plain", ".csv")),
    titlePanel("Statistiques 1ère dose"),
    tags$hr(),
    titlePanel("Vaccination par âge - 1"),
    fluidRow(
        column(6,
            plotOutput("plotAge1"),
        ),
        column(6,
            plotOutput("plotAgeDate1"),
        )
    ),
    tags$hr(),
    titlePanel("Vaccination par région - 1"),
    fluidRow(
        column(6,
            plotOutput("plotRegion1"),
        ),
        column(6,
            plotOutput("plotRegionDate1"),
        )
    ),
    tags$hr(),
    titlePanel("Statistiques 2ème dose"),
    fluidRow(
        column(6,
            plotOutput("plotAge2"),
        ),
        column(6,
            plotOutput("plotAgeDate2"),
        )
    ),
    tags$hr(),
    titlePanel("Vaccination par région - 1"),
    fluidRow(
        column(6,
            plotOutput("plotRegion2"),
        ),
        column(6,
            plotOutput("plotRegionDate2"),
        )
    ),
)

server <- function(input, output) {
    output$plotAge1 <- renderPlot({
        ## Si pas de fichier, on ne fait rien
        if (is.null(input$file1)) {
            return()
        }
        df <- read.csv(input$file1$datapath, header = TRUE, sep = ";")
        df$jour <- as.Date(df$jour, format = "%Y-%m-%d")
        df$date <- as.integer(df$jour)
        df <- subset(df, clage_vacsi != 0)
        df$n_cum_dose1 <- df$n_cum_dose1_h + df$n_cum_dose1_f
        df <- df %>%
        group_by(date, jour, clage_vacsi) %>%
        summarise(n_cum_dose1 = sum(n_cum_dose1))
        df2 <- df %>% group_by(clage_vacsi) %>% summarise(max = (max(n_cum_dose1)))
        df <- merge(x = df, y = df2, by = "clage_vacsi", all.x = TRUE)
        df$n_cum_dose1 <- ((as.double(100)*df$n_cum_dose1) / df$max)
        ggplot(data = df, aes(jour, n_cum_dose1, group = clage_vacsi)) +
        geom_line(aes(color = as.factor(clage_vacsi)), linewidth = 1.2) +
        geom_label(aes(label = clage_vacsi),
                    data = df %>% filter(date == median(df$date)),
                    nudge_x = 0.35,
                    size = 2) +
        scale_x_date(
            labels = date_format(format = "%m-%Y"),
            breaks = date_breaks("2 month")
        ) +
        ggtitle("% de vaccination par tranche d'âge") +
        ylab("Pourcentage de la population vaccinée") +
        xlab("Date") +
        theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
        theme(legend.position = "bottom")
    })
    output$plotAgeDate1 <- renderPlot({
        ## Si pas de fichier, on ne fait rien
        if (is.null(input$file1)) {
            return()
        }
        df <- read.csv(input$file1$datapath, header = TRUE, sep = ";")
        df$jour <- as.Date(df$jour, format = "%Y-%m-%d")
        df$date <- as.integer(df$jour)
        df <- subset(df, clage_vacsi != 0)
        df$n_cum_dose1 <- df$n_cum_dose1_h + df$n_cum_dose1_f
        df <- df %>%
        group_by(date, jour, clage_vacsi) %>%
        summarise(n_cum_dose1 = sum(n_cum_dose1))
        df2 <- df %>% group_by(clage_vacsi) %>% summarise(max = (max(n_cum_dose1)))
        df <- merge(x = df, y = df2, by = "clage_vacsi", all.x = TRUE)
        df$n_cum_dose1 <- ((as.double(100)*df$n_cum_dose1) / df$max)
        dfFirstMarch <- df %>% filter(jour == "2021-03-01")
        dfFifteenJune <- df %>% filter(jour == "2021-06-15")
        dfFirstJanuary <- df %>% filter(jour == "2022-01-01")
        ggplot() +
        geom_boxplot(
            dfFirstMarch, mapping = aes(x = "001 - Début", y =  n_cum_dose1),
            color = "#009E73", fill = "#A4A4A4", outiler.shape = 8, outlier.size = 4, lwd = 2
        ) +
        geom_boxplot(
            dfFifteenJune, mapping = aes(x = "002 - Milieu", y = n_cum_dose1),
            color = "#E69F00", fill = "#A4A4A4", outiler.shape = 8, outlier.size = 4, lwd = 2
        ) +
        geom_boxplot(
            dfFirstJanuary, mapping = aes(x = "003 - Fin", y = n_cum_dose1),
            color = "#56B4E9", fill = "#A4A4A4", outiler.shape = 8, outlier.size = 4, lwd = 2
        )
    })
    output$plotRegion1 <- renderPlot({
        ## Si pas de fichier, on ne fait rien
        if (is.null(input$file1)) {
            return()
        }
        df <- read.csv(input$file1$datapath, header = TRUE, sep = ";")
        df$jour <- as.Date(df$jour, format = "%Y-%m-%d")
        df$date <- as.integer(df$jour)
        df <- subset(df, clage_vacsi != 0)
        df$n_cum_dose1 <- df$n_cum_dose1_h + df$n_cum_dose1_f
        df <- df %>%
        group_by(date, jour, reg) %>%
        summarise(n_cum_dose1 = sum(n_cum_dose1))
        df2 <- df %>% group_by(reg) %>% summarise(max = (max(n_cum_dose1)))
        df <- merge(x = df, y = df2, by = "reg", all.x = TRUE)
        df$n_cum_dose1 <- ((as.double(100)*df$n_cum_dose1) / df$max)
        ggplot(data = df, aes(jour, n_cum_dose1, group = reg)) +
        geom_line(aes(color = as.factor(reg)), linewidth = 1.2) +
        geom_label(aes(label = reg),
                    data = df %>% filter(date == median(df$date)),
                    nudge_x = 0.35,
                    size = 2) +
        scale_x_date(
            labels = date_format(format = "%m-%Y"),
            breaks = date_breaks("2 month")
        ) +
        ggtitle("% de vaccination par région") +
        ylab("Pourcentage de la population vaccinée") +
        xlab("Date") +
        theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
        theme(legend.position = "bottom")
    })
    output$plotRegionDate1 <- renderPlot({
        ## Si pas de fichier, on ne fait rien
        if (is.null(input$file1)) {
            return()
        }
        df <- read.csv(input$file1$datapath, header = TRUE, sep = ";")
        df$jour <- as.Date(df$jour, format = "%Y-%m-%d")
        df$date <- as.integer(df$jour)
        df <- subset(df, clage_vacsi != 0)
        df$n_cum_dose1 <- df$n_cum_dose1_h + df$n_cum_dose1_f
        df <- df %>%
        group_by(date, jour, reg) %>%
        summarise(n_cum_dose1 = sum(n_cum_dose1))
        df2 <- df %>% group_by(reg) %>% summarise(max = (max(n_cum_dose1)))
        df <- merge(x = df, y = df2, by = "reg", all.x = TRUE)
        df$n_cum_dose1 <- ((as.double(100)*df$n_cum_dose1) / df$max)
        dfFirstMarch <- df %>% filter(jour == "2021-03-01")
        dfFifteenJune <- df %>% filter(jour == "2021-06-15")
        dfFirstJanuary <- df %>% filter(jour == "2022-01-01")
        ggplot() +
        geom_boxplot(
            dfFirstMarch, mapping = aes(x = "001 - Début", y =  n_cum_dose1),
            color = "#009E73", fill = "#A4A4A4", outiler.shape = 8, outlier.size = 4, lwd = 2
        ) +
        geom_boxplot(
            dfFifteenJune, mapping = aes(x = "002 - Milieu", y = n_cum_dose1),
            color = "#E69F00", fill = "#A4A4A4", outiler.shape = 8, outlier.size = 4, lwd = 2
        ) +
        geom_boxplot(
            dfFirstJanuary, mapping = aes(x = "003 - Fin", y = n_cum_dose1),
            color = "#56B4E9", fill = "#A4A4A4", outiler.shape = 8, outlier.size = 4, lwd = 2
        )
    })
    output$plotAge2 <- renderPlot({
        ## Si pas de fichier, on ne fait rien
        if (is.null(input$file1)) {
            return()
        }
        df <- read.csv(input$file1$datapath, header = TRUE, sep = ";")
        df$jour <- as.Date(df$jour, format = "%Y-%m-%d")
        df$date <- as.integer(df$jour)
        df <- subset(df, clage_vacsi != 0)
        df$n_cum_dose1 <- df$n_cum_2_rappel_h + df$n_cum_2_rappel_f
        df <- df %>%
        group_by(date, jour, clage_vacsi) %>%
        summarise(n_cum_dose1 = sum(n_cum_dose1))
        df2 <- df %>% group_by(clage_vacsi) %>% summarise(max = (max(n_cum_dose1)))
        df <- merge(x = df, y = df2, by = "clage_vacsi", all.x = TRUE)
        df$n_cum_dose1 <- ((as.double(100)*df$n_cum_dose1) / df$max)
        ggplot(data = df, aes(jour, n_cum_dose1, group = clage_vacsi)) +
        geom_line(aes(color = as.factor(clage_vacsi)), linewidth = 1.2) +
        geom_label(aes(label = clage_vacsi),
                    data = df %>% filter(date == median(df$date)),
                    nudge_x = 0.35,
                    size = 2) +
        scale_x_date(
            labels = date_format(format = "%m-%Y"),
            breaks = date_breaks("2 month")
        ) +
        ggtitle("% de vaccination par tranche d'âge") +
        ylab("Pourcentage de la population vaccinée") +
        xlab("Date") +
        theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
        theme(legend.position = "bottom")
    })
    output$plotAgeDate2 <- renderPlot({
        ## Si pas de fichier, on ne fait rien
        if (is.null(input$file1)) {
            return()
        }
        df <- read.csv(input$file1$datapath, header = TRUE, sep = ";")
        df$jour <- as.Date(df$jour, format = "%Y-%m-%d")
        df$date <- as.integer(df$jour)
        df <- subset(df, clage_vacsi != 0)
        df$n_cum_dose1 <- df$n_cum_2_rappel_h + df$n_cum_2_rappel_f
        df <- df %>%
        group_by(date, jour, clage_vacsi) %>%
        summarise(n_cum_dose1 = sum(n_cum_dose1))
        df2 <- df %>% group_by(clage_vacsi) %>% summarise(max = (max(n_cum_dose1)))
        df <- merge(x = df, y = df2, by = "clage_vacsi", all.x = TRUE)
        df$n_cum_dose1 <- ((as.double(100)*df$n_cum_dose1) / df$max)
        dfFirstMarch <- df %>% filter(jour == "2021-11-01")
        dfFifteenJune <- df %>% filter(jour == "2022-03-01")
        dfFirstJanuary <- df %>% filter(jour == "2022-09-01")
        ggplot() +
        geom_boxplot(
            dfFirstMarch, mapping = aes(x = "001 - Début", y =  n_cum_dose1),
            color = "#009E73", fill = "#A4A4A4", outiler.shape = 8, outlier.size = 4, lwd = 2
        ) +
        geom_boxplot(
            dfFifteenJune, mapping = aes(x = "002 - Milieu", y = n_cum_dose1),
            color = "#E69F00", fill = "#A4A4A4", outiler.shape = 8, outlier.size = 4, lwd = 2
        ) +
        geom_boxplot(
            dfFirstJanuary, mapping = aes(x = "003 - Fin", y = n_cum_dose1),
            color = "#56B4E9", fill = "#A4A4A4", outiler.shape = 8, outlier.size = 4, lwd = 2
        )
    })
    output$plotRegion2 <- renderPlot({
        ## Si pas de fichier, on ne fait rien
        if (is.null(input$file1)) {
            return()
        }
        df <- read.csv(input$file1$datapath, header = TRUE, sep = ";")
        df$jour <- as.Date(df$jour, format = "%Y-%m-%d")
        df$date <- as.integer(df$jour)
        df <- subset(df, clage_vacsi != 0)
        df$n_cum_dose1 <- df$n_cum_2_rappel_h + df$n_cum_2_rappel_f
        df <- df %>%
        group_by(date, jour, reg) %>%
        summarise(n_cum_dose1 = sum(n_cum_dose1))
        df2 <- df %>% group_by(reg) %>% summarise(max = (max(n_cum_dose1)))
        df <- merge(x = df, y = df2, by = "reg", all.x = TRUE)
        df$n_cum_dose1 <- ((as.double(100)*df$n_cum_dose1) / df$max)
        ggplot(data = df, aes(jour, n_cum_dose1, group = reg)) +
        geom_line(aes(color = as.factor(reg)), linewidth = 1.2) +
        geom_label(aes(label = reg),
                    data = df %>% filter(date == median(df$date)),
                    nudge_x = 0.35,
                    size = 2) +
        scale_x_date(
            labels = date_format(format = "%m-%Y"),
            breaks = date_breaks("2 month")
        ) +
        ggtitle("% de vaccination par région") +
        ylab("Pourcentage de la population vaccinée") +
        xlab("Date") +
        theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
        theme(legend.position = "bottom")
    })
    output$plotRegionDate2 <- renderPlot({
        ## Si pas de fichier, on ne fait rien
        if (is.null(input$file1)) {
            return()
        }
        df <- read.csv(input$file1$datapath, header = TRUE, sep = ";")
        df$jour <- as.Date(df$jour, format = "%Y-%m-%d")
        df$date <- as.integer(df$jour)
        df <- subset(df, clage_vacsi != 0)
        df$n_cum_dose1 <- df$n_cum_2_rappel_h + df$n_cum_2_rappel_f
        df <- df %>%
        group_by(date, jour, reg) %>%
        summarise(n_cum_dose1 = sum(n_cum_dose1))
        df2 <- df %>% group_by(reg) %>% summarise(max = (max(n_cum_dose1)))
        df <- merge(x = df, y = df2, by = "reg", all.x = TRUE)
        df$n_cum_dose1 <- ((as.double(100)*df$n_cum_dose1) / df$max)
        dfFirstMarch <- df %>% filter(jour == "2021-11-01")
        dfFifteenJune <- df %>% filter(jour == "2022-03-01")
        dfFirstJanuary <- df %>% filter(jour == "2022-09-01")
        ggplot() +
        geom_boxplot(
            dfFirstMarch, mapping = aes(x = "001 - Début", y =  n_cum_dose1),
            color = "#009E73", fill = "#A4A4A4", outiler.shape = 8, outlier.size = 4, lwd = 2
        ) +
        geom_boxplot(
            dfFifteenJune, mapping = aes(x = "002 - Milieu", y = n_cum_dose1),
            color = "#E69F00", fill = "#A4A4A4", outiler.shape = 8, outlier.size = 4, lwd = 2
        ) +
        geom_boxplot(
            dfFirstJanuary, mapping = aes(x = "003 - Fin", y = n_cum_dose1),
            color = "#56B4E9", fill = "#A4A4A4", outiler.shape = 8, outlier.size = 4, lwd = 2
        )
    })
}

options(shiny.maxRequestSize = 300 * 1024^2)
shinyApp(ui, server)