FROM rstudio/plumber:v1.0.0
MAINTAINER Przemyslaw Cias <przemyslaw.cias@gmail.com>


RUN apt-get update -qq && apt-get install -y pandoc

RUN cd ~
#RUN mkdir ~/Rlilbs
#RUN R -e "install.packages(c('plumber','broom','rio','urltools','zoo','xts','tidyr','dplyr','lubridate', 'tidyquant','reactable','svDialogs','rnbp','data.table'), lib='~/Rlibs')"

COPY / /
WORKDIR "app/"

RUN R -e "install.packages(c('devtools','Rcpp','plumber','broom','rio','urltools','zoo','xts','tidyr','dplyr','lubridate', 'tidyquant','reactable','svDialogs','rnbp','data.table'));devtools::install_version('slackr', version='2.2.0')"

EXPOSE 8000

CMD ["plumber.R"]
