// Define functions to render linked interactive plots using d3.
// Another script should define e.g.
// <script>
//   var plot = new animint("#plot","path/to/plot.json");
// </script>
// Constructor for animint Object.
var animint = function (to_select, json_file) {
  var dirs = json_file.split("/");
  dirs.pop(); //if a directory path exists, remove the JSON file from dirs
  var element = d3.select(to_select);
  this.element = element;
  var Widgets = {};
  this.Widgets = Widgets;
  var Selectors = {};
  this.Selectors = Selectors;
  var Plots = {};
  this.Plots = Plots;
  var Geoms = {};
  this.Geoms = Geoms;
  // SVGs must be stored separately from Geoms since they are
  // initialized first, with the Plots.
  var SVGs = {};
  this.SVGs = SVGs;
  var Animation = {};
  this.Animation = Animation;
  var all_geom_names = {};
  this.all_geom_names = all_geom_names;

  var css = document.createElement('style');
  css.type = 'text/css';
  var styles = [".axis path{fill: none;stroke: black;shape-rendering: crispEdges;}",
            ".axis line{fill: none;stroke: black;shape-rendering: crispEdges;}",
            ".axis text {font-family: sans-serif;font-size: 11px;}"];

  // 'margins' are fixed across panels and do not
  // include title/axis/label padding (since these are not
  // fixed across panels). They do, however, account for
  // spacing between panels
  var margin = {
    left: 0,
    right: 10,
    top: 10,
    bottom: 0
  };
  var plotdim = {
    width: 0,
    height: 0,
    xstart: 0,
    xend: 0,
    ystart: 0,
    yend: 0,
    graph: {
      width: 0,
      height: 0
    },
    margin: margin,
    xlab: {
      x: 0,
      y: 0
    },
    ylab: {
      x: 0,
      y: 0
    },
    title: {
      x: 0,
      y: 0
    }
  };

  var add_geom = function (g_name, g_info) {
    // Determine what style to use to show the selection for this
    // geom. This is a hack and should be removed when we implement
    // the selected.color, selected.size, etc aesthetics.
    if(g_info.aes.hasOwnProperty("fill") &&
       g_info.geom == "rect" &&
       g_info.aes.hasOwnProperty("clickSelects")){
      g_info.select_style = "stroke";
    }else{
      g_info.select_style = "opacity";
    }
    // Add a row to the loading table.
    g_info.tr = Widgets["loading"].append("tr");
    g_info.tr.append("td").text(g_name);
    g_info.tr.append("td").attr("class", "chunk");
    g_info.tr.append("td").attr("class", "downloaded").text(0);
    g_info.tr.append("td").text(g_info.total);
    g_info.tr.append("td").attr("class", "status").text("initialized");
    // Save this geom and load it!
    g_info.data = {};
    g_info.download_status = {};
    Geoms[g_name] = g_info;
    update_geom(g_name, null);
  }
  var add_plot = function (p_name, p_info) {
    // Each plot may have one or more legends. To make space for the
    // legends, we put each plot in a table with one row and two
    // columns: tdLeft and tdRight.
    var plot_table = element.append("table").style("display", "inline-block");
    var plot_tr = plot_table.append("tr");
    var tdLeft = plot_tr.append("td");
    var tdRight = plot_tr.append("td").attr("id", p_name+"_legend");
    var svg = tdLeft.append("svg")
      .attr("id", p_name)
      .attr("height", p_info.options.height)
      .attr("width", p_info.options.width);

    // divvy up width/height based on the panel layout
    var nrows = Math.max.apply(null, p_info.layout.ROW);
    var ncols = Math.max.apply(null, p_info.layout.COL);
    var panel_names = p_info.layout.PANEL;
    var npanels = Math.max.apply(null, panel_names);

    // Draw the title
    var titlepadding = measureText(p_info.title, 20).height + 10;
    // why are we giving the title padding if it is undefined?
    if (p_info.title === undefined)  titlepadding = 0;
    plotdim.title.x = p_info.options.width / 2;
    plotdim.title.y = titlepadding / 2;
    svg.append("text")
      .text(p_info.title)
      .attr("id", "plottitle")
      .attr("class", "title")
      .attr("font-family", "sans-serif")
      .attr("font-size", "20px")
      .attr("transform", "translate(" + (plotdim.title.x) + "," + (
        plotdim.title.y) + ")")
      .style("text-anchor", "middle");

    // Note axis names are "shared" across panels (just like the title)
    var xtitlepadding = 5 + measureText(p_info["xtitle"], 11).height;
    var ytitlepadding = 5 + measureText(p_info["ytitle"], 11).height;

    // grab max text size over axis labels and facet strip labels
    var axispaddingy = 5;
    if(p_info.hasOwnProperty("ylabs") && p_info.ylabs.length){
      axispaddingy += Math.max.apply(null, p_info.ylabs.map(function(entry){
	     return measureText(entry, 11).width;
      }));
    }
    var axispaddingx = 10 + 9;
    if(p_info.hasOwnProperty("xlabs") && p_info.xlabs.length){
      // TODO: throw warning if text height is large portion of plot height?
      axispaddingx += Math.max.apply(null, p_info.xlabs.map(function(entry){
	     return measureText(entry, 11, p_info.xangle).height;
      }));
      // TODO: carefully calculating this gets complicated with rotating xlabs
      margin.right += 5;
    }
    var strip_height =  Math.max.apply(null, p_info.strips.top.map(function(entry){
      return measureText(entry, 11).height;
    }))
    var strip_width =  Math.max.apply(null, p_info.strips.right.map(function(entry){
      return measureText(entry, 11).height;
    }))
    plotdim.margin = margin;

    // track the number of x/y axes to account for when calculating
    // height/width of graphing region
    var n_xaxes = 0;
    var n_yaxes = 0;
    for (var layout_i = 0; layout_i < npanels; layout_i++) {
      if (p_info.layout.COL[layout_i] == 1) n_xaxes += p_info.layout.AXIS_X[layout_i];
      if (p_info.layout.ROW[layout_i] == 1) n_yaxes += p_info.layout.AXIS_Y[layout_i];
    }

    // the *entire graph* height/width
    var graph_width = p_info.options.width - ncols * (margin.left + margin.right) -
                      p_info.strips.n.right * strip_width -
                      n_yaxes * axispaddingy - ytitlepadding;
    var graph_height = p_info.options.height - nrows * (margin.top + margin.bottom) -
                        titlepadding - (p_info.strips.n.top * strip_height) -
                        n_xaxes * axispaddingx - xtitlepadding;

    // Impose the pixelated aspect ratio of the graph upon the width/height
    // proportions calculated by the compiler. This has to be done on the
    // rendering side since the precomputed proportions apply to the *graph*
    // and the graph size depends upon results of measureText()
    if (p_info.layout.coord_fixed[0]) {
      var aspect = (graph_height / nrows) / (graph_width / ncols);
    } else {
      var aspect = 1;
    }
    var wp = p_info.layout.width_proportion.map(function(x){
      return x * Math.min(1, aspect);
    })
    var hp = p_info.layout.height_proportion.map(function(x){
      return x * Math.min(1, 1/aspect);
    })

    // track the proportion of the graph that should be 'blank'
    // this is mainly used to implement coord_fixed()
    var graph_height_blank = 1;
    var graph_width_blank = 1;
    for (var layout_i = 0; layout_i < npanels; layout_i++) {
      if (p_info.layout.COL[layout_i] == 1) graph_height_blank -= hp[layout_i];
      if (p_info.layout.ROW[layout_i] == 1) graph_width_blank -= wp[layout_i];
    }
    // cumulative portion of the graph used
    var graph_width_cum = (graph_width_blank / 2) * graph_width;
    var graph_height_cum = (graph_height_blank / 2) * graph_height;

    // Bind plot data to this plot's SVG element
    svg.plot = p_info;
    Plots[p_name] = p_info;
    p_info.geoms.forEach(function (g_name) {
      var layer_g_element = svg.append("g").attr("class", g_name);
      panel_names.forEach(function(PANEL){
	     layer_g_element.append("g").attr("class", "PANEL" + PANEL);
      });
      SVGs[g_name] = svg;
    });

    // If we are to draw more than one panel,
    // create a grouping for strip labels
    if (npanels > 1) {
      svg.append("g")
        .attr("class", "strip")
        .attr("id", "topStrip");
      svg.append("g")
        .attr("class", "strip")
        .attr("id", "rightStrip");
    }
    // this will hold x/y scales for each panel
    // eventually we inject this into Plots[p_name]
    var scales = {};
    n_xaxes = 0;
    n_yaxes = 0;
    // Draw a plot outline for every panel
    for (var layout_i = 0; layout_i < npanels; layout_i++) {
      var panel_i = layout_i + 1;
      var axis  = p_info["axis" + panel_i];

      //forces values to be in an array
      var xaxisvals = [];
      var xaxislabs = [];
      var yaxisvals = [];
      var yaxislabs = [];
      var outbreaks, outlabs;

      //function to write labels and breaks to their respective arrays
      var axislabs = function(breaks, labs, axis){
        if(axis=="x"){
          outbreaks = xaxisvals;
          outlabs = xaxislabs;
        } else {
          outbreaks = yaxisvals;
          outlabs = yaxislabs;
        } // set appropriate variable names
        if (isArray(breaks)) {
          breaks.forEach(function (d) {
            outbreaks.push(d);
          })
        } else {
          //breaks can be an object!
          for (key in breaks) {
            outbreaks.push(breaks[key]);
          }
        }
        if (labs){
          labs.forEach(function (d) {
            outlabs.push(d);
            // push each label provided into the array
          });
        } else {
          outbreaks.forEach(function (d) {
            outlabs.push("");
            // push a blank string to the array for each axis tick
            // if the specified label is null
          });
        }
      }

    axislabs(axis.x, axis.xlab, "x");
    axislabs(axis.y, axis.ylab, "y");

    // compute the current panel height/width
    plotdim.graph.height = graph_height * hp[layout_i];
    plotdim.graph.width = graph_width * wp[layout_i];

    var current_row = p_info.layout.ROW[layout_i];
    var current_col = p_info.layout.COL[layout_i];
    var draw_x = p_info.layout.AXIS_X[layout_i];
    var draw_y = p_info.layout.AXIS_Y[layout_i];
    // panels are drawn using a "typewriter approach" (left to right & top to bottom)
    // if the carriage is returned (ie, there is a new row), change some parameters:
    var new_row = current_col <= p_info.layout.COL[layout_i - 1]
    if (new_row) {
      n_yaxes = 0;
      graph_width_cum = (graph_width_blank / 2) * graph_width;
      graph_height_cum = graph_height_cum + plotdim.graph.height;
    }
    n_xaxes = n_xaxes + draw_x;
    n_yaxes = n_yaxes + draw_y;

    // calculate panel specific locations to be used in placing axes, labels, etc.
    plotdim.xstart =  current_col * plotdim.margin.left +
                      (current_col - 1) * plotdim.margin.right +
                      graph_width_cum + n_yaxes * axispaddingy + ytitlepadding;
    // room for right strips should be distributed evenly across panels to preserve aspect ratio
    plotdim.xend = plotdim.xstart + plotdim.graph.width;
    plotdim.ystart = current_row * (plotdim.margin.top + strip_height) +
                     (current_row - 1) * plotdim.margin.bottom +
                     graph_height_cum + titlepadding +
                     Math.min(p_info.strips.n.top, current_row) * strip_height;
    // room for xaxis title should be distributed evenly across panels to preserve aspect ratio
    plotdim.yend = plotdim.ystart + plotdim.graph.height;
    // always add to the width (note it may have been reset earlier)
    graph_width_cum = graph_width_cum + plotdim.graph.width;

    // draw the y-axis title (and add padding) when drawing the first panel
    if (layout_i === 0) {
      svg.append("text")
        .text(p_info["ytitle"])
        .attr("class", "label")
        .attr("id", "ytitle")
        .style("text-anchor", "middle")
        .style("font-size", "11px")
        .attr("transform", "translate(" + (plotdim.xstart - axispaddingy - ytitlepadding / 2)
          + "," + (p_info.options.height / 2) + ")rotate(270)");
    }
    // draw the x-axis title when drawing the last panel
    if (layout_i === (npanels - 1)) {
      svg.append("text")
        .text(p_info["xtitle"])
        .attr("class", "label")
        .attr("id", "xtitle")
        .style("text-anchor", "middle")
        .style("font-size", "11px")
        .attr("transform", "translate(" + plotdim.title.x
          + "," + (plotdim.yend + axispaddingx) + ")");
    }

      var draw_strip = function(strip, side) {
        if (strip == "") {
          return(null);
        }
        // assume right is top until it isn't
        var x = (plotdim.xstart + plotdim.xend) / 2;
        var y = plotdim.ystart - strip_height / 2;
        var rotate = 0;
        if (side == "right") {
          x = plotdim.xend + strip_width / 2;
          y = (plotdim.ystart + plotdim.yend) / 2;
          rotate = 90;
        }
        //create a group
        svg.select("#" + side + "Strip")
          .selectAll("." + side + "Strips")
          .data(strip)
          .enter()
            .append("text")
            .style("text-anchor", "middle")
            .style("font-size", "11px")
            .text(function(d) { return d; })
            // NOTE: there could be multiple strips per panel
            // TODO: is there a better way to manage spacing?
            .attr("transform", "translate(" + x + "," + y + ")rotate(" + rotate + ")");
        }
      draw_strip([p_info.strips.top[layout_i]], "top");
      draw_strip([p_info.strips.right[layout_i]], "right");

      // for each of the x and y axes, there is a "real" and fake
      // version. The real version will be used for plotting the
      // data, and the fake version is just for the display of the
      // axes.
      scales[panel_i] = {};
      scales[panel_i].x = d3.scale.linear()
        .domain([0, 1])
        .range([plotdim.xstart, plotdim.xend]);
      scales[panel_i].x_fake = d3.scale.linear()
        .domain(axis.xrange)
        .range([plotdim.xstart, plotdim.xend]);
      scales[panel_i].y = d3.scale.linear()
        .domain([0, 1])
        .range([plotdim.yend, plotdim.ystart]);
      scales[panel_i].y_fake = d3.scale.linear()
        .domain([axis.yrange[1], axis.yrange[0]])
        .range([plotdim.ystart, plotdim.yend]);
      if(draw_x){
	var xaxis = d3.svg.axis()
          .scale(scales[panel_i].x)
          .tickValues(xaxisvals)
          .tickFormat(function (d) {
            return xaxislabs[xaxisvals.indexOf(d)].toString();
          })
          .orient("bottom");
	var xaxis_g = svg.append("g")
          .attr("class", "axis")
          .attr("id", "xaxis")
          .attr("transform", "translate(0," + plotdim.yend + ")")
          .call(xaxis);
	xaxis_g.selectAll("text")
	  .style("text-anchor", p_info.xanchor)
	  .attr("transform", "rotate(" + p_info.xangle + " 0 9)");
      }
      if(draw_y){
	     var yaxis = d3.svg.axis()
          .scale(scales[panel_i].y)
          .tickValues(yaxisvals)
          .tickFormat(function (d) {
            return yaxislabs[yaxisvals.indexOf(d)].toString();
          })
          .orient("left");
	     svg.append("g")
          .attr("class", "axis")
          .attr("id", "yaxis")
          .attr("transform", "translate(" + (plotdim.xstart) + ",0)")
          .call(yaxis);
      }

    	if(!axis.xline) {
    	  styles.push("#"+p_name+" #xaxis"+" path{stroke:none;}");
    	}
    	if(!axis.xticks) {
    	  styles.push("#"+p_name+" #xaxis .tick"+" line{stroke:none;}");
    	}
    	if(!axis.yline) {
    	  styles.push("#"+p_name+" #yaxis"+" path{stroke:none;}");
    	}
    	if(!axis.yticks) {
    	  styles.push("#"+p_name+" #yaxis .tick"+" line{stroke:none;}");
    	}

    } //end of for loop

    Plots[p_name].scales = scales;

  } //end of add_plot()

  var add_selector = function (s_name, s_info) {
    Selectors[s_name] = s_info;
    if(s_info.type == "multiple"){
      if(!isArray(s_info.selected)){
	s_info.selected = [s_info.selected];
      }
    }
  }
  var get_tsv = function(g_info, chunk_id){
    return g_info.classed + "_chunk" + chunk_id + ".tsv";
  }
  // update_geom is called from add_geom and update_selector. It
  // downloads data if necessary, and then calls draw_geom.
  var update_geom = function (g_name, selector_name) {
    var g_info = Geoms[g_name];
    // First apply chunk_order selector variables.
    var chunk_id = g_info.chunks;
    g_info.chunk_order.forEach(function (v_name) {
      if(chunk_id == null){
	return; //no data in a higher up chunk var.
      }
      var value = Selectors[v_name].selected;
      if(chunk_id.hasOwnProperty(value)){
	chunk_id = chunk_id[value];
      }else{
	chunk_id = null; // no data to show in this subset.
      }
    });
    if(chunk_id == null){
      draw_panels(g_info, [], selector_name); //draw nothing.
      return;
    }
    var tsv_name = get_tsv(g_info, chunk_id);
    // get the data if it has not yet been downloaded.
    g_info.tr.select("td.chunk").text(tsv_name);
    if(g_info.data.hasOwnProperty(tsv_name)){
      draw_panels(g_info, g_info.data[tsv_name], selector_name);
    }else{
      g_info.tr.select("td.status").text("downloading");
      var svg = SVGs[g_name];
      var loading = svg.append("text")
	.attr("class", "loading"+tsv_name)
	.text("Downloading "+tsv_name+"...")
	.attr("font-size", 9)
	.attr("y", 10)
	.style("fill", "red")
	//.attr("x", svg.attr("width")/2)
      ;
      download_chunk(g_info, tsv_name, function(chunk){
      	loading.remove();
	draw_panels(g_info, chunk, selector_name);
      });
    }
  }
  var draw_panels = function(g_info, chunk, selector_name) {
    // derive the plot name from the geometry name
    var g_names = g_info.classed.split("_");
    var p_name = g_names[g_names.length - 1];
    var panels = Plots[p_name].layout.PANEL;
    panels.forEach(function(panel) {
      draw_geom(g_info, chunk, selector_name, panel);
    });
  }
  var download_sequence = function(g_name, s_name, seq){
    var g_info = Geoms[g_name];
    var s_info = Selectors[s_name];
    g_info.seq_i = seq.indexOf(s_info.selected);
    g_info.seq_count = 0;
    g_info.seq = seq;
    download_next(g_name);
  }
  var download_next = function(g_name){
    var g_info = Geoms[g_name];
    var selector_value = g_info.seq[g_info.seq_i];
    var chunk_id = g_info.chunks[selector_value];
    var tsv_name = get_tsv(g_info, chunk_id);
    g_info.seq_count += 1;
    if(g_info.seq_count > g_info.seq.length){
      return;
    }
    g_info.seq_i += 1;
    if(g_info.seq_i == g_info.seq.length){
      g_info.seq_i = 0;
    }
    download_chunk(g_info, tsv_name, function(chunk){
      download_next(g_name);
    })
  }
  // download_chunk is called from update_geom and download_sequence.
  var download_chunk = function(g_info, tsv_name, funAfter){
    if(g_info.download_status.hasOwnProperty(tsv_name)){
      funAfter();
      return; // do not download twice.
    }
    g_info.download_status[tsv_name] = "downloading";
    //prefix tsv file with appropriate path
    var tsv_file = dirs.concat(tsv_name).join("/");
    function is_interactive_aes(v_name){
      if(v_name.indexOf("clickSelects") > -1){
	return true;
      }
      if(v_name.indexOf("showSelected") > -1){
	return true;
      }
      return false;
    }
    d3.tsv(tsv_file, function (error, response) {
      // First convert to correct types.
      g_info.download_status[tsv_name] = "processing";
      response.forEach(function (d) {
        for (var v_name in g_info.types) {
	  // interactive aesthetics (clickSelects, showSelected, etc)
	  // stay as characters, others may be converted.
	  if(!is_interactive_aes(v_name)){
            var r_type = g_info.types[v_name];
            if (r_type == "integer") {
              d[v_name] = parseInt(d[v_name]);
            } else if (r_type == "numeric") {
              d[v_name] = parseFloat(d[v_name]);
            } else if (r_type == "factor") {
              //keep it as a character.
            } else if (r_type == "rgb") {
              //keep it as a character.
            } else if (r_type == "linetype") {
              //keep it as a character.
            } else if (r_type == "label") {
              //keep it as a character
            } else if (r_type == "character") {
              //keep it as a character
            } else if (r_type == "character" & v_name == "outliers") {
              d[v_name] = parseFloat(d[v_name].split(" @ "));
            } else {
              throw "unsupported R type " + r_type;
            }
	  }
        }
      });
      var nest = d3.nest();
      g_info.nest_order.forEach(function (v_name) {
        nest.key(function (d) {
          return d[v_name];
        });
      });
      var chunk = nest.map(response);
      g_info.data[tsv_name] = chunk;
      g_info.tr.select("td.downloaded").text(d3.keys(g_info.data).length);
      g_info.download_status[tsv_name] = "saved";
      funAfter(chunk);
    });
  }
  // update_geom is responsible for obtaining a chunk of downloaded
  // data, and then calling draw_geom to actually draw it.
  var draw_geom = function(g_info, chunk, selector_name, PANEL){
    g_info.tr.select("td.status").text("displayed");
    var svg = SVGs[g_info.classed];
    // derive the plot name from the geometry name
    var g_names = g_info.classed.split("_");
    var p_name = g_names[g_names.length - 1];
    var scales = Plots[p_name].scales[PANEL];
    var selected_arrays = [ [] ]; //double array necessary.
    g_info.subset_order.forEach(function (aes_name) {
      var selected, values, old_array, value, new_array;
      //if (aes_name != "group") { // why do we need this?
	if(aes_name == "PANEL"){
	  selected = PANEL;
	}else{
          var v_name = g_info.aes[aes_name];
          selected = Selectors[v_name].selected;
	}
	if(isArray(selected)){
	  values = selected;
	}else{
	  values = [selected];
	}
	var new_arrays = [];
	values.forEach(function(value){
	  selected_arrays.forEach(function(old_array){
	    new_array = old_array.concat(value);
	    new_arrays.push(new_array);
	  })
	})
	selected_arrays = new_arrays;
    //}
    })
    var data = []
    selected_arrays.forEach(function(value_array){
      var some_data = chunk;
      value_array.forEach(function(value){
        if (some_data.hasOwnProperty(value)) {
          some_data = some_data[value];
        } else {
          some_data = [];
        }
      })
      if(isArray(some_data)){
	data = data.concat(some_data);
      }else{
	if(isArray(data)){
	  data = {};
	}
	for(k in some_data){
	  data[k] = some_data[k];
	}
      }
    });
    var aes = g_info.aes;
    var toXY = function (xy, a) {
      return function (d) {
        return scales[xy](d[a]);
      }
    }
    var layer_g_element = svg.select("g." + g_info.classed);
    var panel_g_element = layer_g_element.select("g.PANEL" + PANEL);
    var elements = panel_g_element.selectAll(".geom");
    // TODO: standardize this code across aes/styles.
    var base_opacity = 1;
    if (g_info.params.alpha) {
      base_opacity = g_info.params.alpha;
    }
    //alert(g_info.classed+" "+base_opacity);
    var get_alpha = function (d) {
      var a;
      if (aes.hasOwnProperty("alpha") && d.hasOwnProperty("alpha")) {
        a = d["alpha"];
      } else {
        a = base_opacity;
      }

      return a;
    }
    var size = 2;
    if(g_info.geom == "text"){
      size = 12;
    }
    if (g_info.params.hasOwnProperty("size")) {
      size = g_info.params.size;
    }
    var get_size = function (d) {
      if (aes.hasOwnProperty("size") && d.hasOwnProperty("size")) {
        return d["size"];
      }
      return size;
    }

    var linetype = "solid";
    if (g_info.params.linetype) {
      linetype = g_info.params.linetype;
    }

    var get_dasharray = function (d) {
      var lt;
      if (aes.hasOwnProperty("linetype") && d.hasOwnProperty(
        "linetype")) {
        try {
          lt = d["linetype"];
        } catch (err) {
          lt = g_info.params.linetype;
        }
      } else {
        lt = linetype;
      }

      return linetypesize2dasharray(lt, get_size(d));
    }
    var colour = "black";
    var fill = "black";
    var get_colour = function (d) {
      if (d.hasOwnProperty("colour")) {
        return d["colour"]
      }
      return colour;
    }
    var get_fill = function (d) {
      if (d.hasOwnProperty("fill")) {
        return d["fill"];
      }
      return fill;
    }
    if (g_info.params.colour) {
      colour = g_info.params.colour;
    }
    if (g_info.params.fill) {
      fill = g_info.params.fill;
    }else if(g_info.params.colour){
      fill = g_info.params.colour;
    }
    var text_anchor = "middle";
    if (g_info.params.hjust == 0) {
      text_anchor = "start";
    }
    if (g_info.params.hjust == 1) {
      text_anchor = "end";
    }

    var eActions, eAppend;
    var key_fun = null;
    var id_fun = function(d){
      return d.id;
    };
    if(g_info.aes.hasOwnProperty("key")){
      key_fun = function(d){
        return d.key;
      };
    }
    if (g_info.geom == "line" || g_info.geom == "path" || g_info.geom == "polygon" || g_info.geom == "ribbon") {

      // Lines, paths, polygons, and ribbons are a bit special. For
      // every unique value of the group variable, we take the
      // corresponding data rows and make 1 path. The tricky part is
      // that to use d3 I do a data-bind of some "fake" data which are
      // just group ids, which is the kv variable in the code below

      // // case of only 1 line and no groups.
      // if(!aes.hasOwnProperty("group")){
      //     kv = [{"key":0,"value":0}];
      //     data = {0:data};
      // }else{
      //     // we need to use a path for each group.
      //     var kv = d3.entries(d3.keys(data));
      //     kv = kv.map(function(d){
      // 	d[aes.group] = d.value;
      // 	return d;
      //     });
      // }

      // For an example consider breakpointError$error which is
      // defined using this R code

      // geom_line(aes(segments, error, group=bases.per.probe,
      //    clickSelects=bases.per.probe), data=only.error, lwd=4)

      // Inside update_geom the variables take the following values
      // (pseudo-Javascript code)

      // var kv = [{"key":"0","value":"133","bases.per.probe":"133"},
      //           {"key":"1","value":"2667","bases.per.probe":"2667"}];
      // var data = {"133":[array of 20 points used to draw the line for group 133],
      //             "2667":[array of 20 points used to draw the line for group 2667]};

      // I do elements.data(kv) so that when I set the d attribute of
      // each path, I need to select the correct group before
      // returning anything.

      // e.attr("d",function(group_info){
      //     var one_group = data[group_info.value];
      //     return lineThing(one_group);
      // })

      // To make color work I think you just have to select the group
      // and take the color of the first element, e.g.

      // .style("stroke",function(group_info){
      //     var one_group = data[group_info.value];
      //     var one_row = one_group[0];
      //     return get_color(one_row);
      // }

      //In order to get d3 lines to play nice, bind fake "data" (group
      //id's) -- the kv variable. Then each separate object is plotted
      //using path (case of only 1 thing and no groups).
      if (!aes.hasOwnProperty("group")) {
	       // There is either 1 or 0 groups.
         if(data.length == 0){
          kv = [];
	       } else {
          kv = [{
            "key": 0,
            "value": 0
          }];
          data = {
            0: data
          };
        }
      } else {
        // we need to use a path for each group.
        var kv = d3.entries(d3.keys(data));
        kv = kv.map(function (d) {
          //d[aes.group] = d.value;

          // Need to store the clickSelects value that will
          // be passed to the selector when we click on this
          // item.
          d.clickSelects = data[d.value][0].clickSelects;
          return d;
        });
      }

      // line, path, and polygon use d3.svg.line(),
      // ribbon uses d3.svg.area()
      // we have to define lineThing accordingly.
      if (g_info.geom == "ribbon") {
        var lineThing = d3.svg.area()
          .x(toXY("x", "x"))
          .y(toXY("y", "ymax"))
          .y0(toXY("y", "ymin"))
	;
      } else {
        var lineThing = d3.svg.line()
          .x(toXY("x", "x"))
          .y(toXY("y", "y"))
	;
      }
      //select the correct group before returning anything.
      if(key_fun != null){
	key_fun = function(group_info){
	  var one_group = data[group_info.value];
	  var one_row = one_group[0];
	  //take key from first value in the group.
	  return one_row.key;
	};
      }
      id_fun = function(group_info){
	var one_group = data[group_info.value];
	var one_row = one_group[0];
	//take key from first value in the group.
	return one_row.id;
      };
      elements = elements.data(kv, key_fun);
      eActions = function (e) {
        e.attr("d", function (d) {
          var one_group = data[d.value];
	  // filter NaN since they make the whole line disappear!
	  var no_na = one_group.filter(function(d){
	    if(g_info.geom == "ribbon"){
	      return !isNaN(d.x) && !isNaN(d.ymin) && !isNaN(d.ymax);
	    }else{
	      return !isNaN(d.x) && !isNaN(d.y);
	    }
	  })
          return lineThing(no_na);
        })
          .style("fill", function (group_info) {
            if (g_info.geom == "line" || g_info.geom == "path") {
              return "none";
            }
            var one_group = data[group_info.value];
            var one_row = one_group[0];
	    // take color for first value in the group
            return get_fill(one_row);
          })
          .style("stroke-width", function (group_info) {
            var one_group = data[group_info.value];
            var one_row = one_group[0];
	    // take size for first value in the group
            return get_size(one_row);
          })
          .style("stroke", function (group_info) {
            var one_group = data[group_info.value];
            var one_row = one_group[0];
	    // take color for first value in the group
            return get_colour(one_row);
          })
          .style("stroke-dasharray", function (group_info) {
            var one_group = data[group_info.value];
            var one_row = one_group[0];
	    // take linetype for first value in the group
            return get_dasharray(one_row);
          })
          .style("stroke-width", function (group_info) {
            var one_group = data[group_info.value];
            var one_row = one_group[0];
	    // take line size for first value in the group
            return get_size(one_row);
          });
      }
      eAppend = "path";
    } else if (g_info.geom == "segment") {
      elements = elements.data(data, key_fun);
      eActions = function (e) {
        e.attr("x1", function (d) {
          return scales.x(d["x"]);
        })
          .attr("x2", function (d) {
            return scales.x(d["xend"]);
          })
          .attr("y1", function (d) {
            return scales.y(d["y"]);
          })
          .attr("y2", function (d) {
            return scales.y(d["yend"]);
          })
          .style("stroke-dasharray", get_dasharray)
          .style("stroke-width", get_size)
          .style("stroke", get_colour);
      }
      eAppend = "line";
    } else if (g_info.geom == "linerange") {
      elements = elements.data(data, key_fun);
      eActions = function (e) {
        e.attr("x1", function (d) {
          return scales.x(d["x"]);
        })
          .attr("x2", function (d) {
            return scales.x(d["x"]);
          })
          .attr("y1", function (d) {
            return scales.y(d["ymax"]);
          })
          .attr("y2", function (d) {
            return scales.y(d["ymin"]);
          })
          .style("stroke-dasharray", get_dasharray)
          .style("stroke-width", get_size)
          .style("stroke", get_colour);
      }
      eAppend = "line";
    } else if (g_info.geom == "vline") {
      elements = elements.data(data, key_fun);
      eActions = function (e) {
        e.attr("x1", toXY("x", "xintercept"))
          .attr("x2", toXY("x", "xintercept"))
          .attr("y1", scales.y.range()[0])
          .attr("y2", scales.y.range()[1])
          .style("stroke-dasharray", get_dasharray)
          .style("stroke-width", get_size)
          .style("stroke", get_colour);
      }
      eAppend = "line";
    } else if (g_info.geom == "hline") {
      //pretty much a copy of geom_vline with obvious modifications
      elements = elements.data(data, key_fun);
      eActions = function (e) {
        e.attr("y1", toXY("y", "yintercept"))
          .attr("y2", toXY("y", "yintercept"))
          .attr("x1", scales.x.range()[0] + plotdim.margin.left)
          .attr("x2", scales.x.range()[1] - plotdim.margin.right)
          .style("stroke-dasharray", get_dasharray)
          .style("stroke-width", get_size)
          .style("stroke", get_colour);
      }
      eAppend = "line";
    } else if (g_info.geom == "text") {
      elements = elements.data(data, key_fun);
      // TODO: how to support vjust? firefox doensn't support
      // baseline-shift... use paths?
      // http://commons.oreilly.com/wiki/index.php/SVG_Essentials/Text
      eActions = function (e) {
        e.attr("x", toXY("x", "x"))
          .attr("y", toXY("y", "y"))
	  .style("fill", get_colour)
	  .attr("font-size", get_size)
          .style("text-anchor", text_anchor)
          .text(function (d) {
            return d.label;
          });
      }
      eAppend = "text";
    } else if (g_info.geom == "point") {
      elements = elements.data(data, key_fun);
      eActions = function (e) {
        e.attr("cx", toXY("x", "x"))
          .attr("cy", toXY("y", "y"))
          .attr("r", get_size)
          .style("fill", get_fill)
          .style("stroke", get_colour);
      }
      eAppend = "circle";
    } else if (g_info.geom == "jitter") {
      elements = elements.data(data, key_fun);
      eActions = function (e) {
        e.attr("cx", toXY("x", "x"))
          .attr("cy", toXY("y", "y"))
          .attr("r", get_size)
          .style("fill", get_fill)
          .style("stroke", get_colour);
      }
      eAppend = "circle";
    } else if (g_info.geom == "tallrect") {
      elements = elements.data(data, key_fun);
      eActions = function (e) {
        e.attr("x", toXY("x", "xmin"))
          .attr("width", function (d) {
            return scales.x(d["xmax"]) - scales.x(d["xmin"]);
          })
          .attr("y", scales.y.range()[1])
          .attr("height", scales.y.range()[0] - scales.y.range()[1])
          .style("fill", get_fill)
          .style("stroke-width", get_size)
          .style("stroke", get_colour);
      }
      eAppend = "rect";
    } else if (g_info.geom == "widerect") {
      elements = elements.data(data, key_fun);
      eActions = function (e) {
        e.attr("y", toXY("y", "ymin"))
          .attr("height", function (d) {
            return scales.x(d["ymax"]) - scales.x(d["ymin"]);
          })
          .attr("x", scales.x.range()[0])
          .attr("width", scales.x.range()[1] - scales.x.range()[0])
          .style("fill", get_fill)
          .style("stroke-width", get_size)
          .style("stroke", get_colour);
      }
      eAppend = "rect";
    } else if (g_info.geom == "rect") {
      elements = elements.data(data, key_fun);
      eActions = function (e) {
        e.attr("x", toXY("x", "xmin"))
          .attr("width", function (d) {
            return Math.abs(scales.x(d.xmax) - scales.x(d.xmin));
          })
          .attr("y", toXY("y", "ymax"))
          .attr("height", function (d) {
            return Math.abs(scales.y(d.ymin) - scales.y(d.ymax));
          })
          .style("stroke-dasharray", get_dasharray)
          .style("stroke-width", get_size)
          .style("fill", get_fill);
	if(g_info.select_style != "stroke"){
          e.style("stroke", get_colour);
	}
      }
      eAppend = "rect";
    } else if (g_info.geom == "boxplot") {

      // TODO: currently boxplots are unsupported (we intentionally
      // stop with an error in the R code). The reason why is that
      // boxplots are drawn using multiple geoms and it is not
      // straightforward to deal with that using our current JS
      // code. After all, a boxplot could be produced by combing 3
      // other geoms (rects, lines, and points) if you really wanted
      // it.

      fill = "white";

      elements = elements.data(data);
      eActions = function (e) {
        e.append("line")
          .attr("x1", function (d) {
            return scales.x(d["x"]);
          })
          .attr("x2", function (d) {
            return scales.x(d["x"]);
          })
          .attr("y1", function (d) {
            return scales.y(d["ymin"]);
          })
          .attr("y2", function (d) {
            return scales.y(d["lower"]);
          })
          .style("stroke-dasharray", get_dasharray)
          .style("stroke-width", get_size)
          .style("stroke", get_colour);
        e.append("line")
          .attr("x1", function (d) {
            return scales.x(d["x"]);
          })
          .attr("x2", function (d) {
            return scales.x(d["x"]);
          })
          .attr("y1", function (d) {
            return scales.y(d["upper"]);
          })
          .attr("y2", function (d) {
            return scales.y(d["ymax"]);
          })
          .style("stroke-dasharray", get_dasharray)
          .style("stroke-width", get_size)
          .style("stroke", get_colour);
        e.append("rect")
          .attr("x", function (d) {
            return scales.x(d["xmin"]);
          })
          .attr("width", function (d) {
            return scales.x(d["xmax"]) - scales.x(d["xmin"]);
          })
          .attr("y", function (d) {
            return scales.y(d["upper"]);
          })
          .attr("height", function (d) {
            return Math.abs(scales.y(d["upper"]) - scales.y(d["lower"]));
          })
          .style("stroke-dasharray", get_dasharray)
          .style("stroke-width", get_size)
          .style("stroke", get_colour)
          .style("fill", get_fill);
        e.append("line")
          .attr("x1", function (d) {
            return scales.x(d["xmin"]);
          })
          .attr("x2", function (d) {
            return scales.x(d["xmax"]);
          })
          .attr("y1", function (d) {
            return scales.y(d["middle"]);
          })
          .attr("y2", function (d) {
            return scales.y(d["middle"]);
          })
          .style("stroke-dasharray", get_dasharray)
          .style("stroke-width", get_size)
          .style("stroke", get_colour);
      }
    } else {
      return "unsupported geom " + g_info.geom;
    }
    elements.exit().remove();
    var enter = elements.enter();
    var linkActions = function(a_elements){
      a_elements
	.attr("xlink:href", function(d){ return d.href; })
	.attr("target", "_blank")
	.attr("class", "geom")
      ;
    }
    if(g_info.aes.hasOwnProperty("href")){
      enter = enter.append("svg:a")
	.append("svg:"+eAppend)
      ;
    }else{
      enter = enter.append(eAppend)
	.attr("class", "geom")
      ;
    }
    var has_clickSelects = g_info.aes.hasOwnProperty("clickSelects");
    if (has_clickSelects) {
      var selected_funs = {
	"opacity":{
	  "mouseout":function (d) {
            return ifSelectedElse(d, g_info.aes.clickSelects,
				  get_alpha(d), get_alpha(d) - 1/2);
	  },
	  "mouseover":function (d) {
            return get_alpha(d);
	  }
	},
	"stroke":{
	  "mouseout":function(d){
	    return ifSelectedElse(d, g_info.aes.clickSelects,
				  "black", "transparent");
	  },
	  "mouseover":function(d){
	    return "black";
	  }
	}
      };
      // My original design for clicking/interactivity/transparency:
      // Basically I wanted a really simple way to show which element
      // in a group of clickable geom elements is currently
      // selected. So I decided that all the non-selected elements
      // should have alpha transparency 0.5 less than normal, and the
      // selected element should have normal alpha transparency. Also,
      // the element currently under the mouse has normal alpha
      // transparency, to visually indicate that it can be
      // clicked. Looking at my examples, you will see that I
      // basically use this in two ways:

      // 1. By specifying
      // geom_vline(aes(clickSelects=variable),alpha=0.5), which
      // implies a normal alpha transparency of 0.5. So all the vlines
      // are hidden (normal alpha 0.5 - 0.5 = 0), except the current
      // selection and the current element under the mouse pointer are
      // drawn a bit faded with alpha=0.5.

      // 2. By specifying e.g. geom_point(aes(clickSelects=variable)),
      // that implies a normal alpha=1. Thus the current selection and
      // the current element under the mouse pointer are fully drawn
      // with alpha=1 and the others are shown but a bit faded with
      // alpha=0.5 (normal alpha 1 - 0.5 = 0.5).

      // Edit 19 March 2014: Now there are two styles to show the
      // selection, depending on the geom. For most geoms it is as
      // described above. But for geoms like rects with
      // aes(fill=numericVariable), using opacity to indicate the
      // selection results in a misleading decoding of the fill
      // variable. So in this case we set stroke to "black" for the
      // current selection.

      // TODO: user-configurable selection styles.

      var style_funs = selected_funs[g_info.select_style];
      var over_fun = function(e){
	e.style(g_info.select_style, style_funs["mouseover"]);
      }
      var out_fun = function(e){
	e.style(g_info.select_style, style_funs["mouseout"]);
      }
      elements.call(out_fun)
        .on("mouseover", function (d) {
          d3.select(this).call(over_fun);
        })
        .on("mouseout", function (d) {
          d3.select(this).call(out_fun);
        })
        .on("click", function (d) {
	  // The main idea of how clickSelects works: when we click
	  // something, we call update_selector with the clicked
	  // value.
          var v_name = g_info.aes.clickSelects;
          update_selector(v_name, d.clickSelects);
        })
      ;
    } else { //no clickSelects for this geom.
      // Assign opacity. treat lines and ribbons (groups of points)
      // specially.
      if (g_info.geom == "line" || g_info.geom == "ribbon") {
        enter.style("opacity", function (group_info) {
          var one_group = data[group_info.value];
          var one_row = one_group[0]; // take aesthetic for first value in the group
          return get_alpha(one_row);
        })
      } else {
        enter.style("opacity", get_alpha);
      }
    }
    var has_tooltip = g_info.aes.hasOwnProperty("tooltip");
    if(has_clickSelects || has_tooltip){
      elements
        .text("")
        .append("svg:title")
        .text(function (d) {
	  if(has_tooltip){
	    return d.tooltip;
	  }else{
            var v_name = g_info.aes.clickSelects;
            return v_name + " " + d.clickSelects;
	  }
        })
      ;
    }
    //Set attributes of only the entering elements. This is needed to
    //prevent things from flying around from the upper left when they
    //enter the plot.
    eActions(enter);  //DO NOT DELETE!
    if(Selectors.hasOwnProperty(selector_name)){
      var milliseconds = Selectors[selector_name].duration;
      elements = elements.transition().duration(milliseconds);
    }
    if(g_info.aes.hasOwnProperty("id")){
      elements.attr("id", id_fun);
    }
    if(g_info.aes.hasOwnProperty("href")){
      // elements are <a>, children are e.g. <circle>
      var linked_geoms = elements.select(eAppend);
      //d3.select(linked_geoms).data(data, key_fun); // WHY did we need this?
      eActions(linked_geoms);
      linkActions(elements);
    }else{
      // elements are e.g. <circle>
      eActions(elements); // Set the attributes of all elements (enter/exit/stay)
    }
  }
  var update_selector = function (v_name, value) {
    var s_info = Selectors[v_name];
    value = value + "";
    if(s_info.type == "single"){
      // value is the new selection.
      s_info.selected = value;
    }else{
      // value should be added or removed from the selection.
      var i_value = s_info.selected.indexOf(value);
      if(i_value == -1){
	// not found, add to selection.
	s_info.selected.push(value);
      }else{
	// found, remove from selection.
	s_info.selected.splice(i_value, 1);
      }
    }
    s_info.update.forEach(function(g_name){
      update_geom(g_name, v_name);
    });
  }
  var ifSelectedElse = function (d, v_name, selected, not_selected) {
    var is_selected;
    var value = d.clickSelects + "";
    var s_info = Selectors[v_name];
    if(s_info.type == "single"){
      is_selected = value == s_info.selected;
    }else{
      is_selected = s_info.selected.indexOf(value) != -1;
    }
    if(is_selected){
      return selected;
    } else {
      return not_selected;
    }
  }
  var animateIfLoaded = function () {
    var v_name = Animation.variable;
    var cur = Selectors[v_name].selected;
    var next = Animation.next[cur];
    // Before starting the animation, make sure all the geoms have
    // loaded.
    var geomLoaded = function(x){
      return d3.keys(Geoms).indexOf(x)!=-1;
    }
    if(all_geom_names.every(geomLoaded)){
      update_selector(v_name, next);
    }
  }

  //The main idea of how legends work:

  // 1. In getLegend in animint.R I export the legend entries as a
  // list of rows that can be used in a data() bind in D3.

  // 2. Here in add_legend I create a <table> for every legend, and
  // then I bind the legend entries to <tr>, <td>, and <svg> elements.
  var add_legend = function(p_name, p_info){
    // case of multiple legends, d3 reads legend structure in as an array
    var tdRight = element.select("td#"+p_name+"_legend");
    var legendkeys = d3.keys(p_info.legend);
    for(var i=0; i<legendkeys.length; i++){
      // the table that contains one row for each legend element.
      var legend_table = tdRight.append("table").append("tr")
        .append("th").attr("align", "left")
        .text(p_info.legend[legendkeys[i]].title);
      var l_info = p_info.legend[legendkeys[i]];
      // the legend table with breaks/value/label.
      var legendgeoms = l_info.geoms;
      var legend_rows = legend_table.selectAll("tr")
        .data(l_info.entries)
	.sort(function(d) {return d["order"];})
	.enter()
	.append("tr")
      ;
      var legend_svgs = legend_rows.append("td")
        .append("svg")
  	.attr("id", function(d){return "legend-"+d["label"];})
  	.attr("height", 14)
  	.attr("width", 20)
      ;
      var pointscale = d3.scale.linear().domain([0,7]).range([1,4]);
      // scale points so they are visible in the legend. (does not
      // affect plot scaling)
      var linescale = d3.scale.linear().domain([0,6]).range([1,4]);
      // scale lines so they are visible in the legend. (does not
      // affect plot scaling)
      if(legendgeoms.indexOf("polygon")>-1){
        // aesthetics that would draw a rect
        legend_svgs.append("rect")
	  .attr("x", 2)
	  .attr("y", 2)
	  .attr("width", 10)
	  .attr("height", 10)
          .style("stroke-width", function(d){return d["polygonsize"]||1;})
          .style("stroke-dasharray", function(d){
	    return linetypesize2dasharray(d["polygonlinetype"]||"solid",
					  d["size"]||2);
	  })
          .style("stroke", function(d){return d["polygoncolour"] || "#000000";})
          .style("fill", function(d){return d["polygonfill"] || "#FFFFFF";})
          .style("opacity", function(d){return d["polygonalpha"]||1;});
      }
      if(legendgeoms.indexOf("text")>-1){
        // aesthetics that would draw a rect
        legend_svgs.append("text")
	  .attr("x", 10)
	  .attr("y", 14)
          .style("fill", function(d){return d["textcolour"]||1;})
	  .style("text-anchor", "middle")
	  .attr("font-size", function(d){return d["textsize"]||1;})
	  .text("a")
	;
      }
      if(legendgeoms.indexOf("path")>-1){
        // aesthetics that would draw a line
        legend_svgs.append("line")
          .attr("x1", 1).attr("x2", 19).attr("y1", 7).attr("y2", 7)
          .style("stroke-width", function(d){
	    return linescale(d["pathsize"])||2;
	  })
          .style("stroke-dasharray", function(d){
	    return linetypesize2dasharray(d["pathlinetype"]||"solid",
					  d["pathsize"] || 2);
	  })
          .style("stroke", function(d){return d["pathcolour"] || "#000000";})
          .style("opacity", function(d){return d["pathalpha"]||1;});
      }
      if(legendgeoms.indexOf("point")>-1){
        // aesthetics that would draw a point
        legend_svgs.append("circle")
	  .attr("cx", 10)
	  .attr("cy", 7)
          .attr("r", function(d){return pointscale(d["pointsize"])||4;})
          .style("stroke", function(d){return d["pointcolour"] || "#000000";})
          .style("fill", function(d){
	    return d["pointfill"] || d["pointcolour"] || "#000000";
	  })
          .style("opacity", function(d){return d["pointalpha"]||1;});
      }
      legend_rows.append("td")
	.attr("align", "left")
	.text(function(d){ return d["label"];})
      ;
    }
  }

  // Download the main description of the interactive plot.
  d3.json(json_file, function (error, response) {
    if(response.hasOwnProperty("title")){
      d3.select("title").text(response.title);
    }
    // Add plots.
    for (var p_name in response.plots) {
      add_plot(p_name, response.plots[p_name]);
      add_legend(p_name, response.plots[p_name]);
      // Append style sheet to document head.
      css.appendChild(document.createTextNode(styles.join(" ")));
      document.head.appendChild(css);
    }
    // Then add selectors and start downloading the first data subset.
    for (var s_name in response.selectors) {
      add_selector(s_name, response.selectors[s_name]);
    }
    // loading table.
    element.append("br");
    var show_hide_table = element.append("button")
      .text("Show download status table")
    ;
    show_hide_table
      .on("click", function(){
	if(this.textContent == "Show download status table"){
	  loading.style("display", "");
	  show_hide_table.text("Hide download status table");
	}else{
	  loading.style("display", "none");
	  show_hide_table.text("Show download status table");
	}
      })
    ;
    var loading = element.append("table")
      .style("display", "none")
    ;
    Widgets["loading"] = loading;
    var tr = loading.append("tr");
    tr.append("th").text("geom");
    tr.append("th").attr("class", "chunk").text("selected chunk");
    tr.append("th").attr("class", "downloaded").text("downloaded");
    tr.append("th").attr("class", "total").text("total");
    tr.append("th").attr("class", "status").text("status");
    // Add geoms and construct nest operators.
    for (var g_name in response.geoms) {
      add_geom(g_name, response.geoms[g_name]);
    }
    // Animation control widgets.
    var show_message = "Show animation controls";
    var show_hide_animation_controls = element.append("button")
      .text(show_message)
      .attr("id", "show_hide_animation_controls")
      .on("click", function(){
	if(this.textContent == show_message){
	  time_table.style("display", "");
	  show_hide_animation_controls.text("Hide animation controls");
	}else{
	  time_table.style("display", "none");
	  show_hide_animation_controls.text(show_message);
	}
      })
    ;
    var time_table = element.append("table")
      .style("display", "none")
    ;
    var first_tr = time_table.append("tr");
    var first_th = first_tr.append("th");
    if(response.time){
      Animation.next = {};
      Animation.ms = response.time.ms;
      Animation.variable = response.time.variable;
      Animation.sequence = response.time.sequence;
      Widgets["play_pause"] = first_th
	.append("button")
	.attr("id", "play_pause")
	.on("click", function(){
	  if(this.textContent == "Play"){
	    play();
	  }else{
	    pause();
	  }
	})
      ;
    }
    first_tr.append("th").text("milliseconds");
    if(response.time){
      var second_tr = time_table.append("tr");
      second_tr.append("td").text("updates");
      second_tr.append("td")
	.append("input")
	.attr("id", "updates_ms")
	.attr("type", "text")
	.attr("value", Animation.ms)
	.on("change", function(){
	  Animation.pause();
	  Animation.ms = this.value;
	  Animation.play();
	})
      ;
    }
    for(s_name in Selectors){
      var s_info = Selectors[s_name];
      if(!s_info.hasOwnProperty("duration")){
	s_info.duration = 0;
      }
    }
    var selector_array = d3.keys(Selectors);
    var duration_rows = time_table.selectAll("tr.duration")
      .data(selector_array)
      .enter()
      .append("tr")
    ;
    duration_rows
      .append("td")
      .text(function(s_name){return s_name;})
    ;
    var duration_tds = duration_rows.append("td");
    var duration_inputs = duration_tds
      .append("input")
      .attr("id", function(s_name){
	return "duration_ms_" + s_name;
      })
      .attr("type", "text")
      .on("change", function(s_name){
	Selectors[s_name].duration = this.value;
      })
      .attr("value", function(s_name){
	return Selectors[s_name].duration;
      })
    ;
    // If this is an animation, then start downloading all the rest of
    // the data, and start the animation.
    if (response.time) {
      var i, prev, cur;
      Selectors[Animation.variable].update.forEach(function(g_name){
	var g_info = Geoms[g_name];
	// If there is only 1 chunk we don't need to download anything
	// else.
	if(g_info.chunk_order.length == 0){
	  return;
	}
	if(g_info.chunk_order.length != 1){
	  throw "do not know how to handle more than 1 chunk variable";
	}
	if(g_info.chunk_order[0] != Animation.variable){
	  return; //ignore if this geom is chunked on a non-anim variable.
	}
	download_sequence(g_name, Animation.variable, Animation.sequence);
      });
      for (var i = 0; i < Animation.sequence.length; i++) {
        if (i == 0) {
          prev = Animation.sequence[Animation.sequence.length-1];
        } else {
          prev = Animation.sequence[i - 1];
        }
        cur = Animation.sequence[i];
        Animation.next[prev] = cur;
      }
      all_geom_names = d3.keys(response.geoms);

      var timer;
      Animation.timer = timer;
      function play(){
	// as shown on http://bl.ocks.org/mbostock/3808234
	timer = setInterval(animateIfLoaded, Animation.ms);
	Widgets["play_pause"].text("Pause");
      }
      Animation.play = play;
      function pause(){
	clearInterval(timer);
	Widgets["play_pause"].text("Play");
      }
      Animation.pause = pause;

      // This code starts/stops the animation timer when the page is
      // hidden, inspired by
      // http://stackoverflow.com/questions/1060008
      function onchange (evt) {
	if(document.visibilityState == "hidden"){
	  pause();
	}else{
	  play();
	}
      }
      document.addEventListener("visibilitychange", onchange);

      Animation.play();
    }
  });
}

// create a dummy element, apply the appropriate classes,
// and then measure the element
// Inspired from http://jsfiddle.net/uzddx/2/
var measureText = function(pText, pFontSize, pAngle, pStyle) {
    if (!pText || pText.length === 0) return {height: 0, width: 0};
    if (pAngle === null || isNaN(pAngle)) pAngle = 0;

    var container = d3.select('body').append('svg')
      // do we need to set the class so that styling is applied?
      //.attr('class', classname);

    container.append('text')
      .attr({x: -1000, y: -1000})
      .attr("transform", "rotate(" + pAngle + ")")
      .attr("style", pStyle)
      .attr("font-size", pFontSize)
      .text(pText);

    var bbox = container.node().getBBox();
    container.remove();

    return {height: bbox.height, width: bbox.width};
}

var linetypesize2dasharray = function (lt, size) {
  var isInt = function(n) { return typeof n === 'number' && parseFloat(n) == parseInt(n, 10) && !isNaN(n); }
  if(isInt(lt)){ // R integer line types.
    var o = {
      0: size * 0 + "," + size * 10,
      1: 0,
      2: size * 4 + "," + size * 4,
      3: size + "," + size * 2,
      4: size + "," + size * 2 + "," + size * 4 + "," + size * 2,
      5: size * 8 + "," + size * 4,
      6: size * 2 + "," + size * 2 + "," + size * 6 + "," + size * 2
    };
  } else { //R defined line types
    var o = {
      "blank": size * 0 + "," + size * 10,
      "none": size * 0 + "," + size * 10,
      "solid": 0,
      "dashed": size * 4 + "," + size * 4,
      "dotted": size + "," + size * 2,
      "dotdash": size + "," + size * 2 + "," + size * 4 + "," + size * 2,
      "longdash": size * 8 + "," + size * 4,
      "twodash": size * 2 + "," + size * 2 + "," + size * 6 + "," + size * 2,
      "22": size * 2 + "," + size * 2,
      "42": size * 4 + "," + size * 2,
      "44": size * 4 + "," + size * 4,
      "13": size + "," + size * 3,
      "1343": size + "," + size * 3 + "," + size * 4 + "," + size * 3,
      "73": size * 7 + "," + size * 3,
      "2262": size * 2 + "," + size * 2 + "," + size * 6 + "," + size * 2,
      "12223242": size + "," + size * 2 + "," + size * 2 + "," + size * 2 + "," + size * 3 + "," + size * 2 + "," + size * 4 + "," + size * 2,
      "F282": size * 15 + "," + size * 2 + "," + size * 8 + "," + size * 2,
      "F4448444": size * 15 + "," + size * 4 + "," + size * 4 + "," + size * 4 + "," + size * 8 + "," + size * 4 + "," + size * 4 + "," + size * 4,
      "224282F2": size * 2 + "," + size * 2 + "," + size * 4 + "," + size * 2 + "," + size * 8 + "," + size * 2 + "," + size * 16 + "," + size * 2,
      "F1": size * 16 + "," + size
    };
  }

  if (lt in o){
    return o[lt];
  } else{ // manually specified line types
    str = lt.split("");
    strnum = str.map(function (d) {
      return size * parseInt(d, 16);
    });
    return strnum;
  }
}

var isArray = function(o) {
  return Object.prototype.toString.call(o) === '[object Array]';
}


