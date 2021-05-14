##' @title Get instructions

##' @return
##' @author Shir Dekel
##' @export
get_instructions <- function() {

  instructions_main <- div(
    p("Imagine that you are an executive in a large company composed of many individual businesses. You will see various projects from these businesses and have to decide whether you would like to invest in them."),
    p("Imagine that making good investment decisions will result in you receiving a generous bonus and a potential promotion, and that doing poorly will result in you receiving a large pay cut and a potential demotion. We want to know what choices you would actually make in these scenarios.")
  ) %>%
    as.character()

  instructions <- trial_instructions(
    pages = c(
      "We will now give you the instructions for the task. Use the arrow buttons to browse these instructions",
      instructions_main,
      "Press the 'Next' button to begin."
    ),
    show_clickable_nav = TRUE,
    data = insert_property(stage = "instructions")
  )

  return(instructions)

}
