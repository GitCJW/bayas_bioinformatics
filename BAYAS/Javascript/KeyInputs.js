$(document).on("keydown", function (e) {
   Shiny.onInputChange("keyDown", e.which);
});'

$(document).on("keyup", function (e) {
   Shiny.onInputChange("keyUp", e.which);
});