var animintBinding = new Shiny.OutputBinding();

animintBinding.find = function(scope) {
  return $(scope).find(".shinyAnimint");
};

animintBinding.renderValue = function(el, data) {
  // remove the old graph
  var svg = d3.select(el).select("svg");      
	svg.remove();
  // make sure it goes blank
  $(el).html("");
  // add a dire
  var json_file = "animintAssets/" + data.jsonFile
  var plot = new animint(el, json_file);
  
};

Shiny.outputBindings.register(animintBinding, "cpsievert.animintBinding");
