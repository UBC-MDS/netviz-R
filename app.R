library(dash)
library(ggplot2)
library(dashHtmlComponents)
library(plotly)

app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)
app$title("NetViz")

movies <- readr::read_csv(here::here('data', 'processed/netflix_movies_genres.csv'))

map <- function(type) {
    country_df <- movies %>%
        filter(genre == type) %>%
        dplyr::group_by(country) %>% 
        dplyr::count() %>% 
        dplyr::rename(count = n, name = country)
    
    country_code <- readr::read_csv("https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv") 
    country_code <- country_code %>% 
        dplyr::select(COUNTRY, CODE) %>% 
        dplyr::rename(name = COUNTRY, code = CODE)
    
    country_df <- merge(country_df, country_code, by="name")
    
    fig <- plotly::plot_ly(country_df, type='choropleth', locations=~code, z=~count, text=~name,
                           colorscale='Viridis', zmin = 0, zmax=500,
                           marker=list(line=list(
                               width=0)))
    
    fig <- fig %>% plotly::colorbar(title = "Movie Count")
    fig <- fig %>% plotly::layout(clickmode = 'event+select', 
                                  autosize = FALSE, 
                                  dragmode = "select")
        
}

table <- function(genre) {
    data <- movies[movies$genre == genre, ]
    cols <- c("title", "description", "director")
    data <- data[, cols]
    data <- data[order(data$title),]
    columns <- cols %>%
        purrr::map(function(col) list(name = col, id = col))
    tooltip_data <- apply(data, 1, function(x) purrr::map(x, function(row) list(label = row, value = row)))
    
    list(data, columns, tooltip_data)
}

rating_plot <- function(selected_genre) {
    plot_rating <- ggplot(movies %>% 
                              filter(genre==selected_genre)) +
        aes(y = forcats::fct_infreq(rating),
            fill = genre) +
        geom_bar(stat='count', show.legend =FALSE) +
        labs(title = paste0('Rating distribution of ', selected_genre), 
             x='Number of movies',
             y='Rating') +
        theme(legend.position = 'none')
    ggplotly(plot_rating) %>% layout()
}

area_plot <- function(type) {
    p <- movies %>%
        filter(genre == type) %>%
        ggplot(aes(x=release_year, 
                   color = genre)) +
        geom_line(stat='count') +
        labs(title = paste0( 'Release year plot of ', type),
             x = "Year",
             y = "Frequency",
             color = 'Genre') +
        theme(legend.position = 'none')
    ggplotly(p)
}


genre_dropdown <- dccDropdown(
    options = unique(movies$genre),
    value = 'Comedies',
    id = "col-select",
    style=list(
        "verticalAlign" = "left",
        "border-width"= "20",
        "width" = "100%",
        "height" = "5px",
        "margin" = "3px",
        'position' = 'relative',
        'bottom' = '2px'
    )
)

label_genre <- htmlLabel(
    'Genre of the movie',
    htmlFor = 'col-select',
    style = list(
        fontWeight = 'bold', 
        textAlign = 'center', 
        fontSize = "120%", 
        color = '#5a9c39',
        position = 'relative',
        width = '100%'
    )
) 


app$layout(
    dbcContainer(
        list(
            htmlH1(
                "NetViz - Netflix Movies Visualization Dashboard",
                style = list(
                    textAlign = 'center',
                    color = '#bd1818',
                    fontSize = '360%'
                )
            ),
            htmlBr(),
            htmlDiv(
                dbcRow(list(
                    dbcCol(
                        list(
                            label_genre,
                            genre_dropdown
                            ),
                        style = list('max-width' = '20%')
                        ),
                    dbcCol(
                        list(
                            dbcRow(
                                list(
                                    dbcCol(list(
                                        dbcCard(
                                            dbcCardBody(
                                                htmlH5('Rating'),
                                                style = list(
                                                    textAlign = 'center', 
                                                    fontSize = "120%", 
                                                    color = '#ffd508'
                                                )
                                            ),
                                            style = list(backgroundColor = '#e63900')
                                        ),
                                        dccGraph(id='rating_bar_chart')
                                    ),
                                    style = list('max-width' = '50%')
                                    ),
                                    dbcCol(list(
                                        dbcCard(
                                            dbcCardBody(
                                                htmlH5('Netflix Movies over time'),
                                                style = list(
                                                    textAlign = 'center', 
                                                    fontSize = "120%", 
                                                    color = '#ffd508'
                                                )
                                            ),
                                            style = list(backgroundColor = '#e63900')
                                        ),
                                        dccGraph(id='plot_area')
                                    ),
                                    style = list('max-width' = '50%')
                                    )
                                )
                            ),
                            htmlBr(),
                            htmlBr(),
                            dbcRow(
                                list(
                                    dbcCol(
                                        list(
                                            dbcCard(
                                                dbcCardBody(
                                                    htmlH5('Movie releases by country'),
                                                    style = list(
                                                        textAlign = 'Center',
                                                        fontSize = '120%',
                                                        color = '#ffd508'
                                                    )
                                                ),
                                                style = list(backgroundColor = '#e63900')
                                            ),
                                            dccGraph(id='map')
                                        ),
                                        style = list('max-width' = '100%')
                                    )
                                )
                            ),
                            htmlBr(),
                            htmlBr(),
                            dbcRow(
                                list(
                                    dbcCol(
                                        list(
                                            dbcCard(
                                                dbcCardBody(
                                                    htmlH5('Table of Examples'),
                                                    style = list(
                                                        textAlign = 'Center',
                                                        fontSize = '120%',
                                                        color = '#ffd508'
                                                    )
                                                ),
                                                style = list(backgroundColor = '#ff524a')
                                            ),
                                            dashDataTable(
                                                id = "table",
                                                style_cell = list(
                                                    overflow = 'hidden',
                                                    textOverflow = 'ellipsis',
                                                    maxWidth = 0
                                                ),
                                            page_size = 10)
                                        )
                                    )
                                )
                            )
                        )
                    ))
                ))
            )
        )
    )

app$callback(
    output('rating_bar_chart', 'figure'),
    list(input('col-select', 'value')),
    rating_plot
)

app$callback(
    output('plot_area', 'figure'),
    list(input('col-select', 'value')),
    area_plot
)

app$callback(
    output('map', 'figure'),
    list(input('col-select', 'value')),
    map
)

app$callback(
    list(output('table', 'data'),
         output('table', 'columns'),
         output('table', 'tooltip_data')),
    list(input('col-select', 'value')),
    table
)

app$run_server(host = '0.0.0.0', debug = FALSE)
