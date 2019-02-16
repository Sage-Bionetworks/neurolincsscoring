FROM rocker/tidyverse:latest

RUN apt-get update -y
RUN apt-get install -y dpkg-dev zlib1g-dev libssl-dev libffi-dev
RUN apt-get install -y curl libcurl4-openssl-dev

ENV R_REMOTES_NO_ERRORS_FROM_WARNINGS=true
RUN R -e "devtools::install_github('Sage-Bionetworks/neurolincsscoring')"

COPY exec/tracking_summary_perfect_tracks.R /usr/local/bin/
