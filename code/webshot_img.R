output_file <- Sys.getenv("QUARTO_PROJECT_OUTPUT_FILES")


if (!nzchar(output_file)) {
  quit()
} else {

  if (exists(here::here(output_file))) {
    cat(sprintf("%s file exists\\n", output_file))
  } else {
    cat(sprintf("%s file does not exist\\n", output_file))
  }
  
  webshot2::webshot(
    url = here::here(output_file),
    file = sprintf("%s-screenshot.png", here::here(output_file)),
    vwidth = 1920,
    vheight = 1005
  )
}
