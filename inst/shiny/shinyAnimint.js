var animintBinding = new Shiny.OutputBinding();

animintBinding.find = function(scope) {
  return $(scope).find(".shinyAnimint");
};

animintBinding.renderValue = function(el, data) {
  // remove the old graph 
  // http://stackoverflow.com/questions/14422198/how-do-i-remove-all-children-elements-from-a-node-and-them-apply-them-again-with
  var old_plot = d3.select(el).selectAll("*").remove();
  // add the new plot
  var json_file = "animintAssets/" + data.jsonFile
  var selector_string = "#" + el.id;
  var plot = new animint(selector_string, json_file);
};

Shiny.outputBindings.register(animintBinding, "cpsievert.animintBinding");
