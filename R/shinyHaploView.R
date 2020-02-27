#' @title LD visualization application
#'
#' @description Runs an interactive visualizer for an LD object
#'
#' @param ldData An LD data frame.
#'
#' @import shiny
#' @importFrom plotly layout
#' @importFrom plotly plot_ly
#' @importFrom plotly plotlyOutput
#' @importFrom plotly renderPlotly
#'
#' @export


## Load packages ----
library(ggthemes)
library(plotly)
library(magrittr)
library(shiny)


## Load data ----
configPath <- "/home/btmonier/Temporary/test_rphg/configSQLite.txt"
phgObj <- rPHG::graphBuilder( configFile = configPath, methods = "CONSENSUS")
hapData <- phgObj %>% rPHG::numHaploPerRange()



# === Shiny application =============================================
ui <- fluidPage(
    ## Title
    shiny::h4("HaploViewer v0.0.1"),

    shiny::mainPanel(
        ## Drop down for chromosomes
        selectInput(
            inputId = "chrom_select",
            label = "Select Chromosome",
            choices = NULL
        ),

        ## Interactive plot declaration
        plotly::plotlyOutput("hapPlot")
    )
)

server <- function(input, output, session) {

    ## Get levels of chromosomes
    choices_chrom <- reactive({
        as.vector(levels(numHaplos$seqnames))
    })

    ## Update chromosome selection
    observe({
        updateSelectInput(
            session = session,
            inputId = "chrom_select",
            choices = choices_chrom()
        )
    })

    ## Haploplot
    output$hapPlot <- plotly::renderPlotly({
        tmp <- as.data.frame(hapData)

        # Shape proportions
        yfrac <- 0.1
        xfrac <- 0.001

        # Add shape data
        tmp$med <- apply(tmp[, 3:4], 1, stats::median)
        tmp$color <- "#91baff"
        tmp[seq(1, nrow(tmp), by = 2),]$color <- "#3e619b"

        tmp$numHaplotypes <- sample(x = 1:5, size = nrow(tmp), replace = TRUE)

        # Get limit data
        xbeg <- min(tmp$start)
        xend <- max(tmp$end)
        yend <- max(tmp$numHaplotypes)

        p <- ggplot(tmp) +
            geom_rect(
                mapping = aes(
                    xmin = start,
                    xmax = end,
                    ymin = 0,
                    ymax = max(tmp$numHaplotypes)
                ),
                fill = tmp$color,
                alpha = 0.4
            ) +
            geom_point(aes(med, numHaplotypes, text = paste0("Ref Range ID: ", refRange_id))) +
            geom_path(aes(med, numHaplotypes)) +
            xlab("Physical Position (bp)") +
            ylab("Number of Haplotypes")
        ggplotly(p, tooltip = "text")

        # print(
        #     ggplotly(
        #         ggplot(data = tmp) +
        #             ylim(-(yend * yfrac), yend) +
        #             scale_x_continuous(limits = c(xbeg, xend)) +
        #             geom_rect(
        #                 mapping = aes(
        #                     xmin = .data$start,
        #                     xmax = .data$end,
        #                     ymin = 0,
        #                     ymax = -(yend * yfrac)
        #                 ),
        #                 fill = tmp$color
        #             ) +
        #             geom_path(aes(x = .data$med, y = .data$numHaplotypes)) +
        #             geom_point(
        #                 aes(
        #                     x = .data$med,
        #                     y = .data$numHaplotypes
        #                 ),
        #                 size = 1
        #                 ) +
        #             facet_grid(seqnames ~ .) +
        #             xlab("Physical Position (bp)") +
        #             ylab("Number of Haplotypes")
        #     )
        # )
    })

}

shinyApp(ui = ui, server = server)
