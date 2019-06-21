#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
# https://shiny.rstudio.com/tutorial/
# ended at about 22 minutes

library(shiny)
library(ggplot2)
library(plotly)
library(purrr)
library(dplyr)
library(DT)
library(dprocsim)

# Define UI for application
ui <- fluidPage(
    titlePanel("Draw your own CDFs"),
    # *Input() functions
    sidebarLayout(
        sidebarPanel(
            h3("Data Info"),
            numericInput("xmin", label = "Minimum X Value", value = 0),
            numericInput("xmax", label = "Maximum X Value", value = 1),
            p("Optional: You can also upload your own data (in a single vector .csv file).
              Upon upload, the empirical CDF will be drawn on both plots."),
            fileInput("userdat", "Upload .csv",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values",
                                 ".csv")),
            h3("CDF Info"),
            p("For approximating CDFs, the following information will be used on both bounds."),
            numericInput("seq", label = "Distance between grid points for function approximation.", value = .02),
            checkboxInput("splines", label = "Check to use splines. Default is linear interpolation with approx()."),
            numericInput("spar", label = "Smoothing Parameter for splines (0-1)", value = .5, min = 0, max = 1),
            h3("First bound"),
            h4("Click Draw1 button to draw the line connecting the first set of points."),

            actionButton("draw1", label = "Draw1", icon =icon("pencil-ruler")),
            h3("Second bound"),
            checkboxInput("pts2", label = "Check to begin drawing second set of points for bound 2."),
            h4("Click Draw2 button to draw the line connecting the second set of points."),
            actionButton("draw2", label = "Draw2", icon =icon("pencil-ruler"),
                         style="background-color: #bce0f2;"),
            h4("Click Draw Mean button to draw the mean of the upper and lower bounds."),
            actionButton("drawmean", label = "Draw Mean", icon =icon("pencil-ruler")),
            p("Click Download button to download all available data"),
            downloadButton('downloadData', 'Download', icon = icon("download")),
            width = 3 # width of sidebar panel
        ),
        mainPanel(
            fluidRow(

                column(8,  h3("Click to generate points"),
                       p("To reset the dragging plot, click this plot again."),
                       plotOutput("clickplot", click = "plot_clicks",
                                      width = 600, height = 400)),
                column(4, h3("Clicked points below"), dataTableOutput("datatab"))
            ),
            fluidRow(
                column(8, h3("Drag points to adjust"),
                       p("To remove a point, drag it off the plot. Note: clicking above plot will cause the below plot to reset. All changes will be lost."),
                       plotlyOutput("dragplot", width = "600px", height = "400px")),
                column(4, h3("Dragged points:"), dataTableOutput("datatab2"), p("Click Download button to download"))
            )
        )
    )
    # sliderInput(inputId = "num", label = "Choose a Number",
    #             value = 25, min = 1, max = 100),
    # textInput(inputId = "title", label = "Write a title",
    #           value = "Histogram of Random Normal Values"),
    # actionButton(inputId = "norm", label = "Normal"),
    # actionButton(inputId = "unif", label = "Uniform"),
    # # *Output() functions
    # plotOutput(outputId = "hist"),
    # verbatimTextOutput("stats")
    # action button
)

# Define server logic required to assemble inputs into outputs
server <- function(input, output) {
    # Server rules:
    # 1. save objects to display to output$
    # 2. build objects to display with render*()
    # 3. use input values with input$

    # Reactive Values: a value that changes with input
    # reactive values work with reactive functions: functions that
    # take ractive values and know what to do with them
    # "Operation not allowed without an active reactive context" means:
    # you tried to use a reactive value without also using a reactive function
    # that goes with it.
    # 1. reactive values notify. 2. reactive functions respond.
    # reactive functions:
    # 1. Use a code chunk {} to build & rebuild an object
    #  What code will the function use?
    # 2. The object will respond to changes in a set of reactive values.
    #  Which reactive values will the object respond to?
    # Display output with render*() functions: on re-run all code in {} will be run
    # Question: How to change one reactive value without changing rendered output?
    ## Modularize code with reactive()
    # data <- reactive({rnorm(input$num)})
    # call like a function; it caches its value
    # prevent reactions with isolate() - circumvent updating. prevent title field from updating the plot
    # isolate outputs normal r values - breaks link from input to output
    # will be called when something else reactive is changed.
    # observe event triggers code to run on server observeEvent(x, {}):
    # x is a reactive value to respond to. code in {} is treated as isolated
    # app should never depend on the value of the action button.
    # observe({}): just give a signle block of code, then it will rerun anytime a
    # reactive value changes.
    # stop at 1:18:36
    # delay reactions with eventReactive()
    # eventReactive(input$go, {rnorm(input$num)})
    # first arg is value(s) to respond to.
    # second arg is the code used to build object. will be treated as isolated
    # manage state with reactiveValues()
    # inputs change based on user values, you can't overwrite input values in general
    # own list of reactive values: create a list of reactive values to manipulate programmatically
    #

    click_points_data <- data.frame(x = numeric(), y = numeric(), bound = character(), method = character())

    # collect clicked points
    click_points <- reactive({
        newpoint <- input$plot_clicks
        if (!is.null(newpoint))
            click_points_data <<- data.frame(
                x = c(click_points_data$x, newpoint$x),
                y = c(click_points_data$y, newpoint$y),
                bound = c(click_points_data$bound, paste0("bound ", as.numeric(input$pts2)+1)),
                method = c(click_points_data$method, "clicked"),
                stringsAsFactors = FALSE)
        click_points_data
    })

    # second set of points for bound.
    click_points_data2 <- data.frame(x = numeric(), y = numeric(),  bound = character(), method = character())

    click_points2 <- eventReactive(input$pts2, {
        newpoint2 <- input$plot_clicks
        if (!is.null(newpoint2))
            click_points_data2 <<- data.frame(
                x = c(click_points_data2$x, newpoint2$x),
                y = c(click_points_data2$y, newpoint2$y),
                bound = c(click_points_data2$bound, paste0("bound ", as.numeric(input$pts2)+1)),
                method = c(click_points_data2$method, "clicked"),
                stringsAsFactors = FALSE)
        click_points_data2
    })


    # rv is a "persistent state"
    rv <- reactiveValues(
        #data = rnorm(input$num) # cannot pass inputs to reactiveValues?
        points2 = data.frame(x = numeric(0), y = numeric(0),
                             bound = character(0), method = character(0), stringsAsFactors = FALSE)
        )

    # add to rv everytime plot is clicked
    observeEvent(input$plot_clicks, {
        rv$points2 <-  click_points() %>%
            filter(x >= input$xmin , x <= input$xmax, y >= 0, y <= 1 ) # remove points outside the range
    })


    userData <- reactive({
        if(!is.null(input$userdat)){
            input$userdat
        df <- read.csv(input$userdat$datapath, stringsAsFactors = F)
        df2 <- tibble(x = df[,1], weight = (1/nrow(df)))
        df2 <- df2 %>% group_by(x) %>%
            summarize(weight2 = sum(weight)) %>%
            mutate(y = cumsum(weight2))
        df2
        }
        })

    curve_1_fun <- NULL

    approx_curve1 <- eventReactive(input$draw1, {
        curve1_dat <- augment_cdf(dat = rv$points2 %>% filter(bound == "bound 1"), xrange = c(input$xmin, input$xmax))
        if (input$splines){
            approx_curve1_dat <- smooth.spline(x = curve1_dat$x, y = curve1_dat$y, spar = input$spar)
            approxed_data <- predict(approx_curve1_dat, seq(input$xmin, input$xmax, by = input$seq))
            approxed_data <- data.frame(x = approxed_data$x, y = approxed_data$y,
                                        bound = "bound 1", method = "splines", stringsAsFactors = F)
            approxed_data$y[approxed_data$y > 1] <- 1
            approxed_data$y[approxed_data$y < 0] <- 0
            curve_1_fun <<- function(x){predict(approx_curve1_dat, x)}
        } else {
            approx_curve1_dat <- approxfun(x = curve1_dat$x, y = curve1_dat$y)
            approxed_data <- data.frame(x = seq(input$xmin, input$xmax, by = input$seq),
                                        y = approx_curve1_dat(seq(input$xmin, input$xmax, by = input$seq)),
                                        bound = "bound 1", method = "linear", stringsAsFactors = F)
            approxed_data <- augment_cdf(approxed_data, xrange = c(input$xmin, input$xmax))
            curve_1_fun <<- approx_curve1_dat
        }
         approxed_data
    })

    curve_2_fun <- NULL

    approx_curve2 <- eventReactive(input$draw2, {
        curve2_dat <- augment_cdf(dat = rv$points2 %>% filter(bound == "bound 2"), xrange = c(input$xmin, input$xmax))
        if (input$splines){
            approx_curve2_dat <- smooth.spline(x = curve2_dat$x, y = curve2_dat$y, spar = input$spar)
            approxed_data2 <- predict(approx_curve2_dat, seq(input$xmin, input$xmax, by = input$seq))
            approxed_data2 <- data.frame(x = approxed_data2$x, y = approxed_data2$y,
                                        bound = "bound 2", method = "splines", stringsAsFactors = F)
            approxed_data2$y[approxed_data2$y > 1] <- 1
            approxed_data2$y[approxed_data2$y < 0] <- 0
            curve_2_fun <<- function(x){predict(approx_curve2_dat, x)}
        } else {
            approx_curve2_dat <- approxfun(x = curve2_dat$x, y = curve2_dat$y)
            approxed_data2 <- data.frame(x = seq(input$xmin, input$xmax, by = input$seq),
                                        y = approx_curve2_dat(seq(input$xmin, input$xmax, by = input$seq)),
                                        bound = "bound 2", method = "linear", stringsAsFactors = F)
            curve_2_fun <<- approx_curve2_dat
            }
        approxed_data2
    })

    curve_12_mean <- NULL

    mean_bounds <- eventReactive(input$drawmean,{
        if (!is.null(approx_curve1()) & !is.null(approx_curve2())){
            new_y <- (approx_curve1()$y + approx_curve2()$y)/2
            my_meth <- ifelse(input$splines, "splines", "linear")
            mean_bds <- data.frame(x = seq(input$xmin, input$xmax, by = input$seq),
                                   y = new_y,
                                   bound = "mean", method = my_meth, stringsAsFactors = F )
            curve_12_mean <<- function(x){
                (curve_1_fun(x) + curve_2_fun(x)) / 2
            }
            mean_bds
        }

    })

    output$clickplot <- renderPlot({ # braces around code to pass many lines of code to render plot
        p <- ggplot() +
            #  Clicked points are red circles
            geom_point(data = click_points(), aes(x = x, y = y, color = as.factor(bound)), shape = 1) +
            scale_color_manual(values = c("red", "blue")) +
            labs(x = "", y = "", color = "Bound") +
            xlim(input$xmin,input$xmax) +
            ylim(0,1) # restrict to CDFs
        if(!is.null(input$userdat)){
         p <- p + geom_line(data = userData(), aes(x = x, y = y))
        }
        if (input$draw1){
            p <- p + geom_line(data = approx_curve1(), aes(x = x, y = y), color = "red")
        }
        if (input$draw2){
            p <- p + geom_line(data = approx_curve2(), aes(x = x, y = y), color = "blue")
        }
        if (input$draw1 & input$draw2 & input$drawmean){
            p <- p + geom_line(data = mean_bounds(), aes(x = x, y = y)) +
                geom_area(data = approx_curve1(), aes(x = x, y = y), fill = "blue", alpha = .3) +
                geom_ribbon(data = approx_curve2(), aes(x = x , ymin = y, ymax = 1), fill = "red", alpha = .3)
        }
        p
    })

    output$dragplot <- renderPlotly({
        circles <- pmap(list(click_points()$x, click_points()$y, c("red", "blue")[as.numeric(click_points()$bound == "bound 2") + 1]),
                        function(a,b,c){
                          list(
                            type = "circle",
                            # anchor circles at (mpg, wt)
                            xanchor = a,
                            yanchor = b,
                            # give each circle a 2 pixel diameter
                            x0 = -4, x1 = 4,
                            y0 = -4, y1 = 4,
                            xsizemode = "pixel",
                            ysizemode = "pixel",
                            # other visual properties
                            fillcolor = c,
                            line = list(color = "transparent")
                        )}
        )

       py <- plot_ly() %>%
            add_trace(x = userData()$x, y = userData()$y,
                      type = 'scatter', mode = 'lines', name = 'Empirical CDF',
                      line = list(color = '#45171D')) %>%
            layout(shapes = circles,
                   xaxis = list(range = c(input$xmin,input$xmax)),
                   yaxis = list(range = c(0,1))) %>%
            config(edits = list(shapePosition = TRUE))
       if (input$draw1){
          py <-  py %>%
               add_trace(x = approx_curve1()$x, y= approx_curve1()$y,
                        type='scatter', mode = 'lines', name = 'Bound 1',
                          line = list(color = 'red'))
       }
       if (input$draw2){
           py <-  py %>%
               add_trace(x = approx_curve2()$x, y= approx_curve2()$y,
                         type='scatter', mode = 'lines', name = 'Bound 2',
                         line = list(color = 'blue'))
       }
       if (input$draw1 & input$draw2 & input$drawmean){
           py <- py %>%
               add_trace(x = mean_bounds()$x, y = mean_bounds()$y,
                         type='scatter', mode = 'lines', name = 'Mean',
                         line = list(color = 'black'))
       }
       py
    })


    # observe and edit the dragged values

    observe({
        ed <- event_data("plotly_relayout")
        ed
        shape_anchors <- ed[grepl("^shapes.*anchor$", names(ed))]
        if (length(shape_anchors) != 2) return()
        row_index <- unique(readr::parse_number(names(shape_anchors)) + 1)
        pts <- as.numeric(shape_anchors)
        rv$points2$x[row_index] <- pts[1]
        rv$points2$y[row_index] <- pts[2]
    })


    selectedPoints <- reactive({
        # A workaround to deal with case where click_points() has zero rows
         click_points() %>%
            filter(x >= input$xmin , x <= input$xmax, y >= 0, y <= 1 ) # remove points outside the range
    })

    output$datatab <- renderDataTable({
        datatable(selectedPoints(),
                  options = list(pageLength = 5, lengthChange=FALSE)) %>%
            formatRound(c(1:2), 3)
    })


    output$datatab2 <- renderDataTable({
        datatable(filter(rv$points2, x>=input$xmin, x <= input$xmax, y >=0, y <=1), ## remove points dragged outside of plot region
                  options = list(pageLength = 5, lengthChange=FALSE)) %>%
            formatRound(c(1:2), 3)
    })

    # Download all the data


    all_data <-  reactive({
        # bound: bound1, bound2, mean
        # method: clicked, splines, linear
        bind_rows(
            click_points(),
            click_points2(),
            approx_curve1(),
            approx_curve2(),
            mean_bounds()
            )# x, y, bound, method
    })

    output$downloadData <- downloadHandler(

        # This function returns a string which tells the client
        # browser what name to use when saving the file.
        filename = function() {
            paste0(paste(Sys.Date(), "my-cdf-mean", input$xmin, input$xmax, sep = "-"), ".csv")
        },

        # This function should write data to a file given to it by
        # the argument 'file'.
        content = function(file) {

            # Write to a file specified by the 'file' argument
            write.table(all_data(), file, sep = ",",
                        row.names = FALSE)
        }
    )



}

# Run the application
shinyApp(ui = ui, server = server)
