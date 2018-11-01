FROM rocker/tidyverse:3.4.0

RUN apt-get update -y
RUN apt-get install -y dpkg-dev zlib1g-dev libssl-dev libffi-dev
RUN apt-get install -y curl libcurl4-openssl-dev

RUN R -e "devtools::install_github('Sage-Bionetworks/neurolincsscoring')"

COPY exec/tracking_summary_perfect_tracks.R /usr/local/bin/
