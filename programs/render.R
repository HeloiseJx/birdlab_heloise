# RENDER ------------------------------------------------------------------

rmarkdown::render(here::here("programs", "restit_birdlab_solideo_RMD.Rmd"),
                  output_file=here::here("reporting", "restit_solideo.html"))
