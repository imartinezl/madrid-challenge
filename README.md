# Madrid Open Data Challenge

This repo contains the code for an open data challenge promoted by [Kopuru](http://kopuru.com/).

The [challenge](http://kopuru.com/desafio/reto-open-data-optimizacion-de-la-recogida-de-vidrio-en-madrid-central/) here is to calculate and show on a map which is the shortest route for the collection of glass containers within the district of Madrid Central. In doing so, we could help the City Council of Madrid through the use of data to improve the service to its citizens.


## Challenge Objectives

- To create a visualization platform that delimits the area of Central Madrid and collects the geolocation of the different types of containers.

- To calculate and visualize the route to be followed identifying the starting point, route, end point and distance travelled. The team that identifies the shortest route (measured in meters) respecting the direction of the streets will win.


## General Idea

So, this is actually not a big deal. First we will create a model of the system resembling the very well known TSP (Traveling Salesman Problem). Then, we will choose one of the available optimization methods to solve the TSP problem. More details below.

## Data Adquisition

[datos.madrid.es](https://datos.madrid.es/portal/site/egob/) is a service provided by the city council of Madrid with the purpose of providing a large amount of data and information that can be reused by the general public. In this dataset you can find the addresses and geolocalized points of each of the containers of paper-cardboard, glass, containers, organic and remains available in the districts of the city. 

- [Clothes Containers](https://datos.madrid.es/egob/new/detalle/auxiliar/mapa.jsp?geoUrl=https://datos.madrid.es/egob/catalogo/204410-0-contenedores-ropa.geo): In this dataset you can find the addresses, collection schedules and geolocalized points of each of the used clothing containers that are located at various points in the city, in municipal outbuildings and markets, recycling points, mobile recycling points and proximity recycling points.

- [Battery Containers](https://datos.madrid.es/egob/new/detalle/auxiliar/mapa.jsp?geoUrl=https://datos.madrid.es/egob/catalogo/209799-0-contenedores_pilas_marquesinas.geo): Alkaline/saline batteries and "button" batteries to be disposed of should be taken to a mobile recycling point, as their presence along with the rest of the waste can be dangerous. the city of Madrid also has a network of devices specially prepared for depositing batteries in the bus stop marquees. This set of data provides this information.

- [Oil Containers](https://datos.madrid.es/egob/new/detalle/auxiliar/mapa.jsp?geoUrl=https://datos.madrid.es/egob/catalogo/300196-0-contenedor-aceitevegetal-usado.geo): In this dataset you can find the addresses, collection times and geolocalized points for each of the used vegetable oil containers that are available in the districts. In total there are more than 380 different locations in the city.

- [Recycling Points](https://datos.madrid.es/egob/new/detalle/auxiliar/mapa.jsp?geoUrl=https://datos.madrid.es/egob/catalogo/200284-0-puntos-limpios.geo): Information on data, location, characteristics, timetables, location coordinates and services of the different municipal recycling points in the city of Madrid.

- [Mobile Recycling Points](https://datos.madrid.es/egob/new/detalle/auxiliar/mapa.jsp?geoUrl=https://datos.madrid.es/egob/catalogo/300101-0-puntos-limpios.geo): Information on data, location, characteristics, timetables and days available, location coordinates and services of the different mobile municipal clean points in the city of Madrid.
The mobile recycling points are small, easily identifiable trucks that go to specific places in the districts for a few hours on certain days to receive the waste that cannot be deposited in normal street containers.

- [Proximity Recycling Points](https://datos.madrid.es/egob/new/detalle/auxiliar/mapa.jsp?geoUrl=https://datos.madrid.es/egob/catalogo/300198-0-puntos-proximidad.geo): At the proximity recycling points, ten in total, may be deposited twelve different types of domestic waste, used vegetable oil, used clothing and footwear, waste from small electrical and electronic devices, used batteries, fluorescent and energy-saving light bulbs, toner cartridges and aerosols, X-rays, and CDs, DVDs and videotapes. They will be located in limited municipal enclosures, distributed throughout the city's neighbourhoods. Their use is subject to the opening hours of the centres where they are installed, with the exception of the Barceló Market, which is installed in the public thoroughfare.


## Data Preprocessing

## Container Visualization

## Route Optimization

Inspired by Todd W. Schneider [approach](https://github.com/toddwschneider/shiny-salesman), where he designs a Shiny app to solve the traveling salesman problem with simulated annealing.

### Built with
- [R](https://www.r-project.org/) - Programming Language / 3.6.0
- [RStudio](https://www.rstudio.com/) - IDE for R / 1.2.1335
- [dplyr](https://dplyr.tidyverse.org/) - A grammar of data manipulation / 0.8.1
- [plyr](https://cran.r-project.org/web/packages/plyr/index.html) - Tools for Splitting, Applying and Combining Data / 1.8.4
- [reshape2](https://cran.r-project.org/web/packages/reshape2/index.html) - Flexibly Reshape Data / 1.4.3
- [leaflet](https://rstudio.github.io/leaflet/) - Interactive Maps / 2.0.2
- [ggplot2](https://ggplot2.tidyverse.org/) - Create graphics with R / 3.1.1
- [shiny](https://shiny.rstudio.com/) - Interactive web apps with R / 1.3.2
- [shinythemes](https://rstudio.github.io/shinythemes/) - Themes for Shiny / 1.1.2
- [shinycssloaders](https://github.com/andrewsali/shinycssloaders) - CSS loader animations for Shiny outputs  / 0.2.0
- [igraph](https://igraph.org/r/) - Network Analysis and Visualization / 1.2.4.1
- [geosphere](https://cran.r-project.org/web/packages/geosphere/index.html) - Spherical Trigonometry / 1.5-7
- [stringr](https://stringr.tidyverse.org/index.html) - Library for string manipulations / 1.4.0
- [stringi](https://cran.r-project.org/web/packages/stringi/index.html) - Character String Processing Facilities / 1.4.3
- [zeallot](https://cran.r-project.org/web/packages/zeallot/index.html) - Multiple, Unpacking, and Destructuring Assignment / 0.1.0
- [rgdal](https://cran.r-project.org/web/packages/rgdal/index.html) - Bindings for the 'Geospatial' Data Abstraction Library / 1.4-3
- [sp](https://cran.r-project.org/web/packages/sp/index.html) - Classes and Methods for Spatial Data / 1.3-1
- [htmltools](https://cran.r-project.org/web/packages/htmltools/index.html) - Tools for HTML / 0.3.6
- [viridis](https://cran.r-project.org/web/packages/viridis/index.html) - Default Color Maps from 'matplotlib' / 0.5.1 


