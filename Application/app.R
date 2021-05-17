library(shiny)
library(shinydashboard)
library(shinyWidgets)
source("help_functions.R")



ui <- dashboardPage( skin = 'red',
    dashboardHeader(
        title = "Used Car Selling"
    ),
    dashboardSidebar(
        menuItem('Basic', tabName = 'basic', icon = icon('dollar-sign')),
        menuItem('Detailed', tabName = 'detailed', icon = icon('car')),
        menuItem('Data', tabName = 'data', icon = icon('table'))
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = 'basic',
                    fluidRow(

                            box(

                                list(h4("Date range"),
                                           tags$div( align = 'left',
                                                       dateRangeInput("date_range",
                                                                 label = "Select Posting Date Range",
                                                                 start = min_date),
                                                                 end = max_date)),


                                list(h4("Manufacturers"),
                                            tags$div(align = 'left',
                                                     class = 'multicol',
                                                     actionLink("selectall","Select All"),
                                                     checkboxGroupInput(inputId  = 'multi_manuselector',
                                                                        label    = "Select the manufacturers:",
                                                                        choices  = sort(manu_list),
                                                                        selected=sort(manu_list),
                                                                        inline   = T) ))),
                             box(

                                 list(h4("Average Price per Manufacturer"),
                                            tags$div(align = 'right'),
                                            plotOutput('manu_plot')))
                    ),

                    fluidRow(

                        box(


                            list(h3("Price Summary"),
                                 tags$div(align = 'left'),
                                 selectInput(inputId = 'single_manuselector',
                                             label = 'Select one manufacturer:',
                                             sort(manu_list))),

                            list(
                                 tags$div(align = 'left'),
                                 verbatimTextOutput('summary_text'))) ,

                        box(

                            list(h4("Prices per Year of Manufacturing"),
                                 tags$div(align = 'right'),
                                 plotOutput('plot_price_year'))
                        )
                    )
        ),

        tabItem(tabName = 'detailed',

                fluidRow(

                    box(

                        list(h4("Date range"),
                             tags$div( align = 'left',
                                       dateRangeInput("date_range2",
                                                      label = "Select Posting Date Range",
                                                      start = min_date),
                                       end = max_date)),

                        list(h4("Manufacturers"),
                             tags$div(align = 'left',
                                      class = 'multicol',
                                      actionLink("selectall2","Select All"),
                                      checkboxGroupInput(inputId  = 'multi_manuselector2',
                                                         label    = "Select the manufacturers:",
                                                         choices  = sort(manu_list),
                                                         selected=sort(manu_list),
                                                         inline   = T) ))
                        # ,
                        # list(h4("Car Type"),
                        #      tags$div(align = 'left',
                        #               selectInput(inputId = "car_type",
                        #                           label = "Select the car type",
                        #                           choices = type_list,
                        #                           selected = type_list[1])))
                        ),

                    box( setSliderColor(c("Tomato", "Tomato"), c(1, 2)),
                    #
                    #     list(h3("Region"),
                    #          tags$div(align = 'left',
                    #                   selectInput(inputId = 'region_select',
                    #                               label = "Select Region",
                    #                               choices = region_list)
                    #
                    #
                    #          )),

                        list(h3("Year of Manufacturing"),
                             tags$div(align = 'left',
                                      sliderInput(inputId = 'year_input',
                                          label = "Give the Year of Manufacturing",
                                          min = 1950,
                                          max = 2020,
                                          value = c(1970, 2019)
                                      )
                                      )),
                        list(h3("Odometer Standing"),
                             tags$div(align = 'left',
                                      sliderInput(inputId = 'odometer',
                                                  label = "Give Odometer Standing Range",
                                                  min = 0,
                                                  max = 500000,
                                                  value = c(100000, 400000))

                             )

                        )
                    )
                    ),
                fluidRow(

                    box(
                        list(h4("Car Condition"),

                            tags$div(
                                align = 'left',
                                checkboxGroupInput(inputId = 'condition',
                                             label = 'Select Condition',
                                             choices = condition_list,
                                             selected = condition_list)
                            )
                        ),


                        list(h4("Fuel"),
                             tags$div(
                                 align = 'left',
                                 checkboxGroupInput(inputId = 'fuel',
                                              label = "Select Fuel",
                                              choices = fuel_list,
                                              selected = fuel_list
                                              )
                             )

                        )),
                    box(

                        list(h4("Cylinders"),
                             tags$div(
                                 align = 'left',
                                 checkboxGroupInput(inputId = 'cylinder',
                                              label = "Select Cyclinder",
                                              choices = cyclinder_list,
                                              selected = cyclinder_list)
                             )

                        ),



                        list(h4("Transmission"),
                             tags$div(
                                 align = 'left',
                                 radioButtons(inputId = "transmission",
                                              label = "Select Transmission",
                                              choices = transmission_list)
                             )

                        ))

                    ),


                list(h4("Price Distribution"),
                    tags$div(align = 'center'),
                    plotOutput('price_hist'))

                # list(h4("Avg_price"),
                #      tags$div(align = 'left'),
                #      verbatimTextOutput('avg_pr')
                #      )


                ),

        tabItem(tabName = 'data',

                    DT::dataTableOutput('table_DT')

        )

                )
)





                )








server <- function(input, output, session) {

    # Reactives

    cars_reactive <- reactive({
        cars_rc <- cars %>% filter((posting_date >= input$date_range[1]) & (posting_date <= input$date_range[2]))
        return(cars_rc)
    })

    cars_reactive2 <- reactive({
        cars_rc <- cars %>% filter((posting_date >= input$date_range2[1]) & (posting_date <= input$date_range2[2]))
        cars_rc$price <- round(cars_rc$price, -2)
         return(cars_rc)
    })



    manufacturer_reactive <- reactive({
        df<- get_avg_price_per_manufacturer(cars_reactive(), input$multi_manuselector)
        return(df)
    })


    price_summary <- reactive({
        sum_list <- summary_list(cars_reactive(), input$single_manuselector)
        return(sum_list)
    })

    price_per_year_df <- reactive({
        df_yrpr <- df_price_changes_through_years(cars_reactive(), input$single_manuselector)
        return(df_yrpr)
    })

    price_hist_df <- reactive({
        df <- cars_reactive2()

        df <- filter(df, manufacturer %in% manu_list,
                     condition %in% condition_list,
                     cylinders %in% cyclinder_list,
                     fuel %in% fuel_list,
                     region %in% region_list,
                     transmission %in% transmission_list,
                     type %in% type_list)
        # df <- df_price_histogram(df_reactive_2, input$region_select, input$year_input,
        #                          input$multi_manuselector2, input$condition, input$cylinder,
        #                          input$fuel, input$odometer, input$transmission, input$car_type)

        # regi <- req(input$region_select)
        yr <- req(input$year_input)
        manu <- req(input$multi_manuselector2)
        con <- req(input$condition)
        cyl <- req(input$cylinder)
        fu <- req(input$fuel)
        odo <- req(input$odometer)
        transm <- req(input$transmission)
        # ty <- req(input$car_type)

        #df <- filter(df, region %in% regi)
        df <- filter(df, year >= yr[1], year >= yr[2])
        df <- filter(df, manufacturer %in% manu)
        df <- filter(df, condition %in% con)
        df <- filter(df, cylinders %in% cyl)
        df <- filter(df, fuel %in% fu)
        df <- filter(df, odometer >= odo[1], odometer <= odo[2])
        df <- filter(df, transmission %in% transm)
        # df <- filter(df, type %in% ty)

        return(df)
    })

    # observe event
    observe({
        if(input$selectall == 0) return(NULL)
        else if (input$selectall%%2 != 0)
        {
            list(h3("Manufacturers"),
                 tags$div(align = 'left',
                          class = 'multicol',
                          updateCheckboxGroupInput(inputId  = 'multi_manuselector',
                                             label    = "Select the manufacturers:",
                                             choices  = sort(manu_list),
                                             inline   = T) ))
        }
        else{

            list(h3("Manufacturers"),
                 tags$div(align = 'left',
                          class = 'multicol',
                          updateCheckboxGroupInput(inputId  = 'multi_manuselector',
                                                   label    = "Select the manufacturers:",
                                                   choices  = sort(manu_list),
                                                   selected=sort(manu_list),
                                                   inline   = T) ))
        }

    })


    observe({
        if(input$selectall2 == 0) return(NULL)
        else if (input$selectall2%%2 != 0)
        {
            list(h3("Manufacturers"),
                 tags$div(align = 'left',
                          class = 'multicol',
                          updateCheckboxGroupInput(inputId  = 'multi_manuselector',
                                                   label    = "Select the manufacturers:",
                                                   choices  = sort(manu_list),
                                                   inline   = T) ))
        }
        else{

            list(h3("Manufacturers"),
                 tags$div(align = 'left',
                          class = 'multicol',
                          updateCheckboxGroupInput(inputId  = 'multi_manuselector',
                                                   label    = "Select the manufacturers:",
                                                   choices  = sort(manu_list),
                                                   selected=sort(manu_list),
                                                   inline   = T) ))
        }

    })


    # Outputs

    output$manu_plot <- renderPlot(
        plot_avg_price_per_manufacturer(manufacturer_reactive())
)
    output$summary_text <- renderPrint(
        price_summary()
    )

    output$plot_price_year <- renderPlot(

        plot_price_changes_through_years(price_per_year_df())
    )

    output$price_hist <- renderPlot(
        plot_price_hist(price_hist_df())
    )

    # output$avg_pr <- renderPrint(
    #     avg_df <- price_hist_df(),
    #     print(mean(avg_df))
    # )

    output$table_DT <- renderDT(
        cars_DT
    )

}


shinyApp(ui, server)
