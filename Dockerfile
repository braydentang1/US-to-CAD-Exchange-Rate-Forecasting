# Using Rocker/tidyverse as base

FROM rocker/tidyverse:3.6.3 

# Update the image

RUN apt-get update 

# RStudio authentication                            
CMD ["/bin/bash"] 

RUN apt-get install libudunits2-dev -y

# Need this package for caret
RUN apt install libgdal-dev -y 

RUN Rscript -e "install.packages(c('caret', 'forecast', 'zoo', 'smooth', 'lubridate', 'alfred',\
'shiny', 'shinyWidgets', 'plotly', 'shinythemes', 'shinyhelper', 'shinyBS', 'DT', 'yardstick', 'rsconnect'))"

COPY credentials.R .	

CMD ["Rscript", "credentials.R"]
