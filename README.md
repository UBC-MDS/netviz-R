# NetViz (R version)
Lianna Hovhannisyan, Lynn Wu, Simon Guo, Nobby Nguyen 

## About 

Hi! :wave: Welcome to the NetViz project! This project aims at building an interactive dashboard for :clapper:[Netflix movies data](https://www.kaggle.com/shivamb/netflix-shows). The dashboard can be accessed from [here](https://dsci532-2022-group3-netviz-r.herokuapp.com/).

To learn about our project, jump straight to one of the sections below, or just scroll down to find out more.

- [About](#about)
- [Description](#description)
- [Motivation behind project](#motivation-behind-project)
- [Installation & running from local](#installation-and-running-from-local)
- [Get involved](#get-involved)
- [Contributing](#contributing)
- [Contributors](#contributors)
- [License](#license)



## Description

NetViz provides some summarized information on Netflix movies given by the chosen genre. From a dropdown list, users can filter out a genre that they are interested in. Currently there are four visualizations available on NetViz. First, users can find a rating plot that displays information on the distribution of each rating type of the filtered movies. The ratings are based on the [maturity ratings for movies on Netflix](https://help.netflix.com/en/node/2064) as the data is collected directly from the Netflix API. Second, users can use the time-series trending plot to discover number of movies by release year. Third, users can find a world map showing the distribution of Netflix movies with color-coded scheme indicating the amount of movies produced in each country/region. Last but not least, users who are interested to learn more about their movie selection can refer to the table on the individual movies, which provides detailed information including a short description and the director.

Here is a demo of the dashboard:

![2022-03-17 00 15 42](https://user-images.githubusercontent.com/78331839/158756564-7f4caf66-0f8a-4032-808b-348aaa239d5b.gif)

## Motivation behind project

Since the end of 2019, we have seen two rather tumultuous years in the entertainment industry. Lockdown measures enforced due to the Covid-19 pandemic brought about a surge in TV watching and online streaming. Therefore, more and more media companies are investing in their own streaming services to accommodate the increasing demand of online streaming of the users.

In order to pick up on the new trend of online streaming in the next few years, it is integral for new streaming platforms to know where the industry is going. As regional differences have brought some troublesome for users in different countries; hence global consolidation streaming is expected to be a new trend. As such, insights extracted from data of global giant name as Netflix are great resources for these platforms to decide their business strategy.

To overcome this obstacle, we propose building a data visualization app that allows stakeholders in the streaming companies to explore a dataset of Netflix to determine the popular content that they need to provide to their users, or to invest in which genre of the original content. The app will show the distribution of movie genres over time, as well as the movie rating to target the right market segment.

## Installation and running from local

NetViz is powered by `Dash`. New to `Dash`? Visit [here](https://dash.plotly.com/introduction) to learn more about `Dash`.

If you wish to install and run NetViz from local, here are the steps.

- In terminal, enter the following command:
```
git clone https://github.com/UBC-MDS/netviz-R.git
```
- Go to the netviz folder directory:
```
cd netviz-R/
```
- Run the `app.R` file in terminal:
```
Rscript app.R
```
- In the returning message, copy the website shown to open the dashboard:

![image](https://user-images.githubusercontent.com/78331839/158541797-cdd349ed-6235-4c4f-8cb3-6622993deb94.png)

## Get involved

Let us know what you think about NetViz! We welcome any feedback with our project. If you would like to help make this project better, you can get involved in the following ways:
- Tell us what you like or dislike about NetViz dashboard.
- Share your thoughts on what features or plots you would like to see on NetViz (e.g. Is there a specific level of filtering that interests you?)

If you have a bug to report or an idea, feel free to browse the open issues before opening a new one. Your messages will be reviewed and replied by the project team in a timely manner.

## Contributing

Interested in contributing? Check out the contributing guidelines in [CONTRIBUTING](https://github.com/UBC-MDS/netviz/blob/main/CONTRIBUTING.md) document. Please note that this project is released with a [Code of Conduct](https://github.com/UBC-MDS/netviz/blob/main/CODE_OF_CONDUCT.md). By contributing to this project, you agree to abide by its terms.

## Contributors

Main Contributors:
- Lianna Hovhannisyan: @liannah
- Simon Guo: @y248guo
- Nobby Nguyen: @nobbynguyen
- Lynn Wu: @lynnwbl

## License

The `NetViz` app was created by Lianna Hovhannisyan, Simon Guo, Nobby Nguyen, Lynn Wu within the Master of Data Science program at the University of British Columbia (2021-2022). It is licensed under the terms of the MIT license.
