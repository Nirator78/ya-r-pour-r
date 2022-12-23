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
    tags$hr(),
    titlePanel("Vaccination par année / mois / Sexe"),
    fluidRow(
        column(6,
            plotOutput("anneeMoisHomme"),
        ),
        column(6,
            plotOutput("anneeMoisFemme"),
        )
    ),
    tags$hr(),
    titlePanel("Vaccination Régression Homme/Femme"),
    fluidRow(
        column(12,
            plotOutput("anneeMoisHommeRegression"),
        ),
    ),
    tags$hr(),
    titlePanel("Prédiction vaccination Homme/Femme"),
    fluidRow(
        column(6,
            plotOutput("prediHomme"),
        ),
        column(6,
            plotOutput("prediFemme"),
        ),
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
            color = "#009E73", fill = "#A4A4A4", lwd = 2
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
    output$anneeMoisHomme <- renderPlot({
        ## Si pas de fichier, on ne fait rien
        if (is.null(input$file1)) {
            return()
        }
        df <- read.csv(input$file1$datapath, header = TRUE, sep = ";")
        df$date <- as.Date(df$jour, format = "%Y-%m-%d")
        df$annee <- format(df$date, "%Y")
        df$mois <- format(df$date, "%m")
        df$jour <- format(df$date, "%d")
        df$cum_dose1_homme_global <- cumsum(df$n_dose1_h)
        df$cum_dose1_femme_global <- cumsum(df$n_dose1_f)
        dfAnneeMoisHomme <-
            df %>% group_by(annee, mois) %>% 
            summarise(cum_dose1_homme_annee_mois = sum(n_dose1_h+n_rappel_h+n_2_rappel_h+n_3_rappel_h))
        ggplot(dfAnneeMoisHomme, aes(x = mois, y = cum_dose1_homme_annee_mois, fill = annee)) +
            geom_bar(stat = "identity", position = "dodge") +
            theme_minimal() +
            labs(title = "Evolution des doses par mois", x = "Mois", y = "Nombre de doses") +
            theme(plot.title = element_text(hjust = 0.5))
    })
    output$anneeMoisFemme <- renderPlot({
        ## Si pas de fichier, on ne fait rien
        if (is.null(input$file1)) {
            return()
        }
        df <- read.csv(input$file1$datapath, header = TRUE, sep = ";")
        df$date <- as.Date(df$jour, format = "%Y-%m-%d")
        df$annee <- format(df$date, "%Y")
        df$mois <- format(df$date, "%m")
        df$jour <- format(df$date, "%d")
        df$cum_dose1_homme_global <- cumsum(df$n_dose1_h)
        df$cum_dose1_femme_global <- cumsum(df$n_dose1_f)
        dfAnneeMoisFemme <- 
            df %>% group_by(annee, mois) %>%
            summarise(cum_dose1_femme_annee_mois = sum(n_dose1_f+n_rappel_f+n_2_rappel_f+n_3_rappel_f))
        ggplot(dfAnneeMoisFemme, aes(x = mois, y = cum_dose1_femme_annee_mois, fill = annee)) +
            geom_bar(stat = "identity", position = "dodge") +
            theme_minimal() +
            labs(title = "Evolution des doses par mois", x = "Mois", y = "Nombre de doses") +
            theme(plot.title = element_text(hjust = 0.5))

    })
    output$anneeMoisHommeRegression <- renderPlot({
        ## Si pas de fichier, on ne fait rien
        if (is.null(input$file1)) {
            return()
        }
        df <- read.csv(input$file1$datapath, header = TRUE, sep = ";")
        df$date <- as.Date(df$jour, format = "%Y-%m-%d")
        df$annee <- format(df$date, "%Y")
        df$mois <- format(df$date, "%m")
        df$jour <- format(df$date, "%d")
        df$cum_dose1_homme_global <- cumsum(df$n_dose1_h)
        df$cum_dose1_femme_global <- cumsum(df$n_dose1_f)
        dfAnneeMoisHomme <-
            df %>% group_by(annee, mois) %>% 
            summarise(cum_dose1_homme_annee_mois = sum(n_dose1_h+n_rappel_h+n_2_rappel_h+n_3_rappel_h))
        dfAnneeMoisFemme <-
            df %>% group_by(annee, mois) %>%
            summarise(cum_dose1_femme_annee_mois = sum(n_dose1_f+n_rappel_f+n_2_rappel_f+n_3_rappel_f))
        a <- cov(dfAnneeMoisHomme$cum_dose1_homme_annee_mois, dfAnneeMoisFemme$cum_dose1_femme_annee_mois) / var(dfAnneeMoisHomme$cum_dose1_homme_annee_mois)
        b <- mean(dfAnneeMoisFemme$cum_dose1_femme_annee_mois) - a * mean(dfAnneeMoisHomme$cum_dose1_homme_annee_mois)
        dfAnneeMoisHomme$y <- a * dfAnneeMoisHomme$cum_dose1_homme_annee_mois + b
        ggplot(dfAnneeMoisHomme, aes(x=dfAnneeMoisHomme$cum_dose1_homme_annee_mois, y=dfAnneeMoisFemme$cum_dose1_femme_annee_mois)) +
            geom_point() +
            geom_smooth(method=lm) +
            labs(title = "Evolution des doses par mois", x = "Nombre de doses par mois pour les hommes", y = "Nombre de doses par mois pour les femmes") +
            geom_text(aes(label=annee), vjust=-0.5, hjust=0.5, size=3) 
    })
    output$prediHomme <- renderPlot({
        ## Si pas de fichier, on ne fait rien
        if (is.null(input$file1)) {
            return()
        }
        df <- read.csv(input$file1$datapath, header = TRUE, sep = ";")
        df$date <- as.Date(df$jour, format = "%Y-%m-%d")
        df$annee <- format(df$date, "%Y")
        df$mois <- format(df$date, "%m")
        df$jour <- format(df$date, "%d")
        df$cum_dose1_homme_global <- cumsum(df$n_dose1_h)
        df$cum_dose1_femme_global <- cumsum(df$n_dose1_f)
        dfAnneeMoisHomme <-
            df %>% group_by(annee, mois) %>% 
            summarise(cum_dose1_homme_annee_mois = sum(n_dose1_h+n_rappel_h+n_2_rappel_h+n_3_rappel_h))
        dfAnneeMoisFemme <-
            df %>% group_by(annee, mois) %>%
            summarise(cum_dose1_femme_annee_mois = sum(n_dose1_f+n_rappel_f+n_2_rappel_f+n_3_rappel_f))
        a <- cov(dfAnneeMoisHomme$cum_dose1_homme_annee_mois, dfAnneeMoisFemme$cum_dose1_femme_annee_mois) / var(dfAnneeMoisHomme$cum_dose1_homme_annee_mois)
        b <- mean(dfAnneeMoisFemme$cum_dose1_femme_annee_mois) - a * mean(dfAnneeMoisHomme$cum_dose1_homme_annee_mois)
        dfAnneeMoisHomme$y <- a * dfAnneeMoisHomme$cum_dose1_homme_annee_mois + b
        dfAnneeMoisHomme$mois_annee <- paste(dfAnneeMoisHomme$annee, dfAnneeMoisHomme$mois, 1, sep = "-")
        dfAnneeMoisHomme$mois_annee <- as.Date(dfAnneeMoisHomme$mois_annee, format = "%Y-%m-%d")
        modeleHomme <- lm(cum_dose1_homme_annee_mois ~ poly(mois_annee,1), data = dfAnneeMoisHomme)
        newHomme <- data.frame(mois_annee = as.Date(c("2023-01-01","2023-02-01","2023-03-01","2023-04-01","2023-05-01","2023-06-01","2023-07-01","2023-08-01","2023-09-01","2023-10-01","2023-11-01","2023-12-01")))
        predHomme <- predict(modeleHomme, newHomme, interval = "prediction")
        predHomme <- data.frame(predHomme)
        newHomme <- data.frame(newHomme)
        predHomme$range <- (predHomme$upr + predHomme$fit)/2
        plot(dfAnneeMoisHomme$mois_annee, dfAnneeMoisHomme$cum_dose1_homme_annee_mois, col = "blue", pch = 20, cex = 1.5, xlab = "Mois", ylab = "Nombre de doses", main = "Evolution des doses par mois pour les hommes", xlim= c(as.Date('2020-12-01'), as.Date('2023-12-01')))
        lines(dfAnneeMoisHomme$mois_annee, dfAnneeMoisHomme$cum_dose1_homme_annee_mois, col = "blue", pch = 20, cex = 1.5, xlab = "Mois", ylab = "Nombre de doses", main = "Evolution des doses par mois pour les hommes", xlim= c(as.Date('2020-12-01'), as.Date('2023-12-01')))
        points(newHomme$mois_annee, predHomme$fit, col = "green")
        points(newHomme$mois_annee, predHomme$upr, col = "orange")
        points(newHomme$mois_annee, predHomme$range, col = "red")
    })
    output$prediFemme <- renderPlot({
        ## Si pas de fichier, on ne fait rien
        if (is.null(input$file1)) {
            return()
        }
        df <- read.csv(input$file1$datapath, header = TRUE, sep = ";")
        df$date <- as.Date(df$jour, format = "%Y-%m-%d")
        df$annee <- format(df$date, "%Y")
        df$mois <- format(df$date, "%m")
        df$jour <- format(df$date, "%d")
        df$cum_dose1_homme_global <- cumsum(df$n_dose1_h)
        df$cum_dose1_femme_global <- cumsum(df$n_dose1_f)
        dfAnneeMoisHomme <-
            df %>% group_by(annee, mois) %>% 
            summarise(cum_dose1_homme_annee_mois = sum(n_dose1_h+n_rappel_h+n_2_rappel_h+n_3_rappel_h))
        dfAnneeMoisFemme <-
            df %>% group_by(annee, mois) %>%
            summarise(cum_dose1_femme_annee_mois = sum(n_dose1_f+n_rappel_f+n_2_rappel_f+n_3_rappel_f))
        a <- cov(dfAnneeMoisHomme$cum_dose1_homme_annee_mois, dfAnneeMoisFemme$cum_dose1_femme_annee_mois) / var(dfAnneeMoisHomme$cum_dose1_homme_annee_mois)
        b <- mean(dfAnneeMoisFemme$cum_dose1_femme_annee_mois) - a * mean(dfAnneeMoisHomme$cum_dose1_homme_annee_mois)
        dfAnneeMoisHomme$y <- a * dfAnneeMoisHomme$cum_dose1_homme_annee_mois + b
        dfAnneeMoisFemme$mois_annee <- paste(dfAnneeMoisFemme$annee, dfAnneeMoisFemme$mois, 1, sep = "-")
        dfAnneeMoisFemme$mois_annee <- as.Date(dfAnneeMoisFemme$mois_annee, format = "%Y-%m-%d")
        modelFemme <- lm(cum_dose1_femme_annee_mois ~ poly(mois_annee,1), data = dfAnneeMoisFemme)
        newFemme <- data.frame(mois_annee = as.Date(c("2023-01-01","2023-02-01","2023-03-01","2023-04-01","2023-05-01","2023-06-01","2023-07-01","2023-08-01","2023-09-01","2023-10-01","2023-11-01","2023-12-01")))
        predFemme <- predict(modelFemme, newFemme, interval = "prediction")
        newFemme <- data.frame(newFemme)
        predFemme <- data.frame(predFemme)
        predFemme$range <- (predFemme$upr + predFemme$fit) /2
        plot(dfAnneeMoisFemme$mois_annee, dfAnneeMoisFemme$cum_dose1_femme_annee_mois, col = "blue", pch = 20, cex = 1.5, xlab = "Mois", ylab = "Nombre de doses", main = "Evolution des doses par mois pour les femmes", xlim= c(as.Date('2020-12-01'), as.Date('2023-12-01')))
        lines(dfAnneeMoisFemme$mois_annee, dfAnneeMoisFemme$cum_dose1_femme_annee_mois, col = "blue", pch = 20, cex = 1.5, xlab = "Mois", ylab = "Nombre de doses", main = "Evolution des doses par mois pour les femmes", xlim= c(as.Date('2020-12-01'), as.Date('2023-12-01')))
        points(newFemme$mois_annee, predFemme$fit, col = "green")
        points(newFemme$mois_annee, predFemme$upr, col = "orange")
        points(newFemme$mois_annee, predFemme$range, col = "red")
    })
}

options(shiny.maxRequestSize = 300 * 1024^2)
shinyApp(ui, server)