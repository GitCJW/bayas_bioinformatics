FROM drcjw/bayas_rdeps:1.0.0

LABEL org.opencontainers.image.authors="chrstoph.waterkamp@uni-due.de"

COPY Docker/dep/ /root/Docker/dep/
RUN Rscript /root/Docker/dep/stan_toolchain.R


RUN R -q -e "\
  options(repos=c(CRAN='https://cloud.r-project.org')); \
  remotes::install_github('GitCJW/shinybayas@v0.2.9', upgrade='never', dependencies=TRUE); \
  remotes::install_github('GitCJW/bayesianssd@v0.1.1', upgrade='never', dependencies=TRUE); \
"

COPY BAYAS/CSS/ /root/BAYAS/CSS/
COPY BAYAS/Data/ /root/BAYAS/Data/
COPY BAYAS/Datenschutz/ /root/BAYAS/Datenschutz/
COPY BAYAS/Images/ /root/BAYAS/Images/
COPY BAYAS/Javascript/ /root/BAYAS/Javascript/
COPY BAYAS/Report/ /root/BAYAS/Report/
COPY BAYAS/Scripts/ /root/BAYAS/Scripts/
COPY BAYAS/Video/ /root/BAYAS/Video/
COPY BAYAS/Planning_models/ /root/BAYAS/Planning_models/
COPY BAYAS/Data_user/ /root/BAYAS/Data_user/
COPY BAYAS/Icons/ /root/BAYAS/Icons/

COPY Docker/Rprofile.site /usr/lib/R/etc/

EXPOSE 3838 

RUN R --version

#For nginx
CMD ["R", "-e", "shiny::runApp('/root/BAYAS/Scripts'"]
