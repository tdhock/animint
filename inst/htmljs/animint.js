// Define functions to render linked interactive plots using d3.
// Another script should define e.g.
// <script>
//   var plot = new animint("#plot","path/to/plot.json");
// </script>
// Constructor for animint Object.
var animint = function (to_select, json_file) {

  function wait_until_then(timeout, condFun, readyFun) {
    function checkFun() {
      if(condFun()) {
        readyFun();
      } else{
        setTimeout(checkFun, timeout);
      }
    }
    checkFun();
  }

  function convert_R_types(resp_array, types){
    return resp_array.map(function (d) {
      for (var v_name in d) {
      	if(!is_interactive_aes(v_name)){
          var r_type = types[v_name];
          if (r_type == "integer") {
            d[v_name] = parseInt(d[v_name]);
          } else if (r_type == "numeric") {
            d[v_name] = parseFloat(d[v_name]);
          } else if (r_type == "factor" || r_type == "rgb" 
		     || r_type == "linetype" || r_type == "label" 
		     || r_type == "character") {
            // keep it as a character
          } else if (r_type == "character" & v_name == "outliers") {
            d[v_name] = parseFloat(d[v_name].split(" @ "));
          } 
      	}
      }
      return d;
    });
  }

  // replacing periods in variable with an underscore this makes sure
  // that selector doesn't confuse . in name with css selectors
  function safe_name(unsafe_name){
    return unsafe_name.replace(/[ .]/g, '_');
  }
  function legend_class_name(selector_name){
    return safe_name(selector_name) + "_variable";
  }

  function is_interactive_aes(v_name){
    if(v_name.indexOf("clickSelects") > -1){
      return true;
    }
    if(v_name.indexOf("showSelected") > -1){
      return true;
    }
    return false;
  }

  var linetypesize2dasharray = function (lt, size) {
    var isInt = function(n) {
      return typeof n === 'number' && parseFloat(n) == parseInt(n, 10) && !isNaN(n);
    };
    if(isInt(lt)){ // R integer line types.
      if(lt == 1){
        return null;
      }
      var o = {
        0: size * 0 + "," + size * 10,
        2: size * 4 + "," + size * 4,
        3: size + "," + size * 2,
        4: size + "," + size * 2 + "," + size * 4 + "," + size * 2,
        5: size * 8 + "," + size * 4,
        6: size * 2 + "," + size * 2 + "," + size * 6 + "," + size * 2
      };
    } else { // R defined line types
      if(lt == "solid" || lt === null){
        return null;
      }
      var o = {
        "blank": size * 0 + "," + size * 10,
        "none": size * 0 + "," + size * 10,
        "dashed": size * 4 + "," + size * 4,
        "dotted": size + "," + size * 2,
        "dotdash": size + "," + size * 2 + "," + size * 4 + "," + size * 2,
        "longdash": size * 8 + "," + size * 4,
        "twodash": size * 2 + "," + size * 2 + "," + size * 6 + "," + size * 2,
        "22": size * 2 + "," + size * 2,
        "42": size * 4 + "," + size * 2,
        "44": size * 4 + "," + size * 4,"13": size + "," + size * 3,
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
  };

  var isArray = function(o) {
    return Object.prototype.toString.call(o) === '[object Array]';
  };

  // create a dummy element, apply the appropriate classes,
  // and then measure the element
  // Inspired from http://jsfiddle.net/uzddx/2/
  var measureText = function(pText, pFontSize, pAngle, pStyle) {
    if (!pText || pText.length === 0) return {height: 0, width: 0};
    if (pAngle === null || isNaN(pAngle)) pAngle = 0;

    var container = element.append('svg');
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
  };

  var nest_by_group = d3.nest().key(function(d){ return d.group; });
  var dirs = json_file.split("/");
  dirs.pop(); //if a directory path exists, remove the JSON file from dirs
  var element = d3.select(to_select);
  this.element = element;
  var viz_id = element.attr("id");
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

  //creating an array to contain the selectize widgets
  var selectized_array = [];
  var data_object_geoms = {
    "line":true,
    "path":true,
    "ribbon":true,
    "polygon":true
  };
  var css = document.createElement('style');
  css.type = 'text/css';
  var styles = [".axis path{fill: none;stroke: black;shape-rendering: crispEdges;}",
            ".axis line{fill: none;stroke: black;shape-rendering: crispEdges;}",
            ".axis text {font-family: sans-serif;font-size: 11px;}"];

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
    // Determine if data will be an object or an array.
    if(g_info.geom in data_object_geoms){
      g_info.data_is_object = true;
    }else{
      g_info.data_is_object = false;
    }
    // Add a row to the loading table.
    g_info.tr = Widgets["loading"].append("tr");
    g_info.tr.append("td").text(g_name);
    g_info.tr.append("td").attr("class", "chunk");
    g_info.tr.append("td").attr("class", "downloaded").text(0);
    g_info.tr.append("td").text(g_info.total);
    g_info.tr.append("td").attr("class", "status").text("initialized");

    // load chunk tsv
    g_info.data = {};
    g_info.download_status = {};
    Geoms[g_name] = g_info;
    // Determine whether common chunk tsv exists
    // If yes, load it
    if(g_info.hasOwnProperty("columns") && g_info.columns.common){
      var common_tsv = get_tsv(g_info, "_common");
      g_info.common_tsv = common_tsv;
      var common_path = getTSVpath(common_tsv);
      d3.tsv(common_path, function (error, response) {
	var converted = convert_R_types(response, g_info.types);
	g_info.data[common_tsv] = nest_by_group.map(converted);
      });
    } else {
      g_info.common_tsv = null;
    }
    // Save this geom and load it!
    update_geom(g_name, null);
  };
  var add_plot = function (p_name, p_info) {
    // Each plot may have one or more legends. To make space for the
    // legends, we put each plot in a table with one row and two
    // columns: tdLeft and tdRight.
    var plot_table = element.append("table").style("display", "inline-block");
    var plot_tr = plot_table.append("tr");
    var tdLeft = plot_tr.append("td");
    var tdRight = plot_tr.append("td").attr("class", p_name+"_legend");
    if(viz_id === null){
      p_info.plot_id = p_name;
    }else{
      p_info.plot_id = viz_id + "_" + p_name;
    }
    var svg = tdLeft.append("svg")
      .attr("id", p_info.plot_id)
      .attr("height", p_info.options.height)
      .attr("width", p_info.options.width);

    // divvy up width/height based on the panel layout
    var nrows = Math.max.apply(null, p_info.layout.ROW);
    var ncols = Math.max.apply(null, p_info.layout.COL);
    var panel_names = p_info.layout.PANEL;
    var npanels = Math.max.apply(null, panel_names);

    // Note axis names are "shared" across panels (just like the title)
    var xtitlepadding = 5 + measureText(p_info["xtitle"], 11).height;
    var ytitlepadding = 5 + measureText(p_info["ytitle"], 11).height;

    // 'margins' are fixed across panels and do not
    // include title/axis/label padding (since these are not
    // fixed across panels). They do, however, account for
    // spacing between panels
    var text_height_pixels = measureText("foo", 11).height;
    var margin = {
      left: 0,
      right: text_height_pixels * p_info.panel_margin_lines,
      top: text_height_pixels * p_info.panel_margin_lines,
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

    // Draw the title
    var titlepadding = measureText(p_info.title, 20).height + 10;
    // why are we giving the title padding if it is undefined?
    if (p_info.title === undefined) titlepadding = 0;
    plotdim.title.x = p_info.options.width / 2;
    plotdim.title.y = titlepadding / 2;
    svg.append("text")
      .text(p_info.title)
      .attr("class", "plottitle")
      .attr("font-family", "sans-serif")
      .attr("font-size", "20px")
      .attr("transform", "translate(" + plotdim.title.x + "," + 
        plotdim.title.y + ")")
      .style("text-anchor", "middle");

    // grab max text size over axis labels and facet strip labels
    var axispaddingy = 5;
    if(p_info.hasOwnProperty("ylabs") && p_info.ylabs.length){
      axispaddingy += Math.max.apply(null, p_info.ylabs.map(function(entry){
	// + 5 to give a little extra space to avoid bad axis labels
	// in shiny.
	return measureText(entry, 11).width + 5;
      }));
    }
    var axispaddingx = 10 + 20;
    if(p_info.hasOwnProperty("xlabs") && p_info.xlabs.length){
      // TODO: throw warning if text height is large portion of plot height?
      axispaddingx += Math.max.apply(null, p_info.xlabs.map(function(entry){
	     return measureText(entry, 11, p_info.xangle).height;
      }));
      // TODO: carefully calculating this gets complicated with rotating xlabs
      //margin.right += 5;
    }
    plotdim.margin = margin;
    
    var strip_heights = p_info.strips.top.map(function(entry){ 
      return measureText(entry, 11).height;
    });
    var strip_widths = p_info.strips.right.map(function(entry){ 
      return measureText(entry, 11).height; 
    });

    // compute the number of x/y axes, max strip height per row, and
    // max strip width per columns, for calculating height/width of
    // graphing region.
    var row_strip_heights = [];
    var col_strip_widths = [];
    var n_xaxes = 0;
    var n_yaxes = 0;
    var current_row, current_col;
    for (var layout_i = 0; layout_i < npanels; layout_i++) {
      current_row = p_info.layout.ROW[layout_i] - 1;
      current_col = p_info.layout.COL[layout_i] - 1;
      if(row_strip_heights[current_row] === undefined){
	row_strip_heights[current_row] = [];
      }
      if(col_strip_widths[current_col] === undefined){
	col_strip_widths[current_col] = [];
      }
      row_strip_heights[current_row].push(strip_heights[layout_i]);
      col_strip_widths[current_col].push(strip_widths[layout_i]);
      if (p_info.layout.COL[layout_i] == 1) {
	n_xaxes += p_info.layout.AXIS_X[layout_i];
      }
      if (p_info.layout.ROW[layout_i] == 1) {
	n_yaxes += p_info.layout.AXIS_Y[layout_i];
      }
    }
    function cumsum_array(array_of_arrays){
      var cumsum = [], max_value, cumsum_value = 0;
      for(var i=0; i<array_of_arrays.length; i++){
	cumsum_value += d3.max(array_of_arrays[i]);
	cumsum[i] = cumsum_value;
      }
      return cumsum;
    }
    var cum_height_per_row = cumsum_array(row_strip_heights);
    var cum_width_per_col = cumsum_array(col_strip_widths);
    var strip_width = d3.max(cum_width_per_col);
    var strip_height = d3.max(cum_height_per_row);

    // the *entire graph* height/width
    var graph_width = p_info.options.width - 
        ncols * (margin.left + margin.right + strip_width) -
        n_yaxes * axispaddingy - ytitlepadding;
    var graph_height = p_info.options.height - 
        nrows * (margin.top + margin.bottom + strip_height) -
        titlepadding - n_xaxes * axispaddingx - xtitlepadding;

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

    // create a grouping for strip labels (even if there are none).
    var topStrip = svg.append("g")
      .attr("class", "topStrip")
    ;
    var rightStrip = svg.append("g")
      .attr("class", "rightStrip")
    ;

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
      };

      if(axis["xticks"]){
	axislabs(axis.x, axis.xlab, "x");
      }
      if(axis["yticks"]){
	axislabs(axis.y, axis.ylab, "y");
      }

      // compute the current panel height/width
      plotdim.graph.height = graph_height * hp[layout_i];
      plotdim.graph.width = graph_width * wp[layout_i];

      current_row = p_info.layout.ROW[layout_i];
      current_col = p_info.layout.COL[layout_i];
      var draw_x = p_info.layout.AXIS_X[layout_i];
      var draw_y = p_info.layout.AXIS_Y[layout_i];
      // panels are drawn using a "typewriter approach" (left to right
      // & top to bottom) if the carriage is returned (ie, there is a
      // new row), change some parameters:
      var new_row = current_col <= p_info.layout.COL[layout_i - 1]
      if (new_row) {
	n_yaxes = 0;
	graph_width_cum = (graph_width_blank / 2) * graph_width;
	graph_height_cum += graph_height * hp[layout_i-1];
      }
      n_xaxes += draw_x;
      n_yaxes += draw_y;

      // calculate panel specific locations to be used in placing
      // axes, labels, etc.
      plotdim.xstart =  current_col * plotdim.margin.left +
        (current_col - 1) * plotdim.margin.right +
        graph_width_cum + n_yaxes * axispaddingy + ytitlepadding;
      // room for right strips should be distributed evenly across
      // panels to preserve aspect ratio
      plotdim.xend = plotdim.xstart + plotdim.graph.width;
      // total height of strips drawn thus far
      var strip_h = cum_height_per_row[current_row-1];
      plotdim.ystart = current_row * plotdim.margin.top +
        (current_row - 1) * plotdim.margin.bottom +
        graph_height_cum + titlepadding + strip_h;
      // room for xaxis title should be distributed evenly across
      // panels to preserve aspect ratio
      plotdim.yend = plotdim.ystart + plotdim.graph.height;
      // always add to the width (note it may have been reset earlier)
      graph_width_cum = graph_width_cum + plotdim.graph.width;

      // get the x position of the y-axis title (and add padding) when
      // rendering the first plot.
      if (layout_i === 0) {
	var ytitle_x = (plotdim.xstart - axispaddingy - ytitlepadding / 2);
	var xtitle_left = plotdim.xstart;
	var ytitle_top = plotdim.ystart;
      }
      // get the y position of the x-axis title when drawing the last
      // panel.
      if (layout_i === (npanels - 1)) {
	var xtitle_y = (plotdim.yend + axispaddingx);
	var xtitle_right = plotdim.xend;
	var ytitle_bottom = plotdim.yend;
      }

      var draw_strip = function(strip, side) {
        if (strip == "") {
          return(null);
        }
        var x, y, rotate, stripElement;
        if (side == "right") {
          x = plotdim.xend + strip_widths[layout_i] / 2 - 2;
          y = (plotdim.ystart + plotdim.yend) / 2;
          rotate = 90;
	  stripElement = rightStrip;
        }else{ //top
	  x = (plotdim.xstart + plotdim.xend) / 2;
          y = plotdim.ystart - strip_heights[layout_i] / 2 + 3;
	  rotate = 0;
	  stripElement = topStrip;
	}
	var trans_text = "translate(" + x + "," + y + ")";
	var rot_text = "rotate(" + rotate + ")";
	stripElement
          .selectAll("." + side + "Strips")
          .data(strip)
          .enter()
          .append("text")
          .style("text-anchor", "middle")
          .style("font-size", "11px")
          .text(function(d) { return d; })
        // NOTE: there could be multiple strips per panel
        // TODO: is there a better way to manage spacing?
          .attr("transform", trans_text + rot_text)
	;
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
          .orient("bottom")
	;
	var xaxis_g = svg.append("g")
          .attr("class", "xaxis axis")
          .attr("transform", "translate(0," + plotdim.yend + ")")
          .call(xaxis);
	if(axis["xline"] == false){
	  var axis_path = xaxis_g.select("path.domain");
	  axis_path.remove();
	}
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
	var yaxis_g = svg.append("g")
          .attr("class", "yaxis axis")
          .attr("transform", "translate(" + (plotdim.xstart) + ",0)")
          .call(yaxis);
	if(axis["yline"] == false){
	  var axis_path = yaxis_g.select("path.domain");
	  axis_path.remove();
	}
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
      
      // creating g element for background, grid lines, and border
      // uses insert to draw it right before plot title
      var background = svg.insert("g", ".plottitle")
        .attr("class", "background");
      
      // drawing background
      if(Object.keys(p_info.panel_background).length > 1) {
        background.append("rect")
          .attr("x", plotdim.xstart)
          .attr("y", plotdim.ystart)
          .attr("width", plotdim.xend - plotdim.xstart)
          .attr("height", plotdim.yend - plotdim.ystart)
          .attr("class", "background_rect")
          .style("fill", p_info.panel_background.fill)
          .style("stroke", p_info.panel_background.colour)
          .style("stroke-dasharray", function() {
            return linetypesize2dasharray(p_info.panel_background.linetype,
                                          p_info.panel_background.size);
          });
      }
      
      // function to draw major/minor grid lines 
      var grid_line = function(grid_background, grid_class) {
        // if grid lines are defined
        if(Object.keys(grid_background).length > 1) {
          var col = grid_background.colour;
          var lt = grid_background.linetype;
          var size = grid_background.size;
          var cap = grid_background.lineend;
          // group for grid lines
          var grid = background.append("g")
            .attr("class", grid_class);

          // group for horizontal grid lines
          var grid_hor = grid.append("g")
            .attr("class", "hor");
          // draw horizontal grid lines if they are defined
          if(typeof grid_background.loc.y != "undefined") {
            // coercing y lines to array if necessary
            if(typeof grid_background.loc.y == "number") grid_background.loc.y = [grid_background.loc.y];
            // drawing lines
            grid_hor.selectAll("line")
              .data(function() { return d3.values(grid_background.loc.y); })
              .enter()
              .append("line")
              .attr("x1", plotdim.xstart)
              .attr("x2", plotdim.xend)
              .attr("y1", function(d) { return scales[panel_i].y(d); })
              .attr("y2", function(d) { return scales[panel_i].y(d); })
              .style("stroke", col)
              .style("stroke-linecap", cap)
              .style("stroke-width", size)
              .style("stroke-dasharray", function() {
                return linetypesize2dasharray(lt, size);
              });;
          }

          // group for vertical grid lines
          var grid_vert = grid.append("g")
            .attr("class", "vert");
          // draw vertical grid lines if they are defined
          if(typeof grid_background.loc.x != "undefined") {
            // coercing x lines to array if necessary
            if(typeof grid_background.loc.x == "number") grid_background.loc.x = [grid_background.loc.x];
            // drawing lines
            grid_vert.selectAll("line")
              .data(function() { return d3.values(grid_background.loc.x); })
              .enter()
              .append("line")
              .attr("x1", function(d) { return scales[panel_i].x(d); })
              .attr("x2", function(d) { return scales[panel_i].x(d); })
              .attr("y1", plotdim.ystart)
              .attr("y2", plotdim.yend)
              .style("stroke", col)
              .style("stroke-linecap", cap)
              .style("stroke-width", size)
              .style("stroke-dasharray", function() {
                return linetypesize2dasharray(lt, size);
              });;
          }
        }
      }
      // drawing the grid lines
      grid_line(p_info.grid_minor, "grid_minor");
      grid_line(p_info.grid_major, "grid_major");
      
      // drawing border
      // uses insert to draw it right before the #plottitle
      if(Object.keys(p_info.panel_border).length > 1) {
        background.append("rect")
          .attr("x", plotdim.xstart)
          .attr("y", plotdim.ystart)
          .attr("width", plotdim.xend - plotdim.xstart)
          .attr("height", plotdim.yend - plotdim.ystart)
          .attr("class", "border_rect")
          .style("fill", p_info.panel_border.fill)
          .style("stroke", p_info.panel_border.colour)
          .style("stroke-dasharray", function() {
            return linetypesize2dasharray(p_info.panel_border.linetype,
                                          p_info.panel_border.size);
          });
      }

    } //end of for(layout_i
    // After drawing all backgrounds, we can draw the axis labels.
    if(p_info["ytitle"]){
      svg.append("text")
	.text(p_info["ytitle"])
	.attr("class", "ytitle")
	.style("text-anchor", "middle")
	.style("font-size", "11px")
	.attr("transform", "translate(" + 
	      ytitle_x +
	      "," +
	      (ytitle_top + ytitle_bottom)/2 + 
	      ")rotate(270)")
      ;
    }
    if(p_info["xtitle"]){
      svg.append("text")
	.text(p_info["xtitle"])
	.attr("class", "xtitle")
	.style("text-anchor", "middle")
	.style("font-size", "11px")
	.attr("transform", "translate(" + 
	      (xtitle_left + xtitle_right)/2 +
	      "," + 
	      xtitle_y + 
	      ")")
      ;
    }
    Plots[p_name].scales = scales;
  }; //end of add_plot()

  function update_legend_opacity(v_name){
    var s_info = Selectors[v_name];
    s_info.legend_tds.style("opacity", s_info.legend_update_fun);
  }

  var add_selector = function (s_name, s_info) {
    Selectors[s_name] = s_info;
    if(s_info.type == "multiple"){
      if(!isArray(s_info.selected)){
        s_info.selected = [s_info.selected];
      }
      // legend_update_fun is evaluated in the context of the
      // td.legend_entry_label.
      s_info.legend_update_fun = function(d){
	var i_value = s_info.selected.indexOf(this.textContent);
	if(i_value == -1){
	  return 0.5;
	}else{
	  return 1;
	}
      }
    }else{
      s_info.legend_update_fun = function(d){
	if(this.textContent == s_info.selected){
	  return 1;
	}else{
	  return 0.5;
	}
      }
    }
    s_info.legend_tds = 
      element.selectAll("tr."+legend_class_name(s_name)+" td.legend_entry_label")
    ;
    update_legend_opacity(s_name);
  }; //end of add_selector()

  function get_tsv(g_info, chunk_id){
    return g_info.classed + "_chunk" + chunk_id + ".tsv";
  }
  function getTSVpath(tsv_name){
    return dirs.concat(tsv_name).join("/");
  }
  
  /**
   * copy common chunk tsv to varied chunk tsv, returning an array of
   * objects.
  */
  function copy_chunk(g_info, varied_chunk) {
    var varied_by_group = nest_by_group.map(varied_chunk);
    var common_by_group = g_info.data[g_info.common_tsv];
    var new_varied_chunk = [];
    for(group_id in varied_by_group){
      var varied_one_group = varied_by_group[group_id];
      var common_one_group = common_by_group[group_id];
      var common_i = 0;
      for(var varied_i=0; varied_i < varied_one_group.length; varied_i++){
	// there are two cases: each group of varied data is of length
	// 1, or of length of the common data.
	if(common_one_group.length == varied_one_group.length){
	  common_i = varied_i;
	}
	var varied_obj = varied_one_group[varied_i];
	var common_obj = common_one_group[common_i];
	for(col in common_obj){
	  if(col != "group"){
	    varied_obj[col] = common_obj[col];
	  }
	}
	new_varied_chunk.push(varied_obj);
      }
    }
    return new_varied_chunk;
  }

  // update_geom is called from add_geom and update_selector. It
  // downloads data if necessary, and then calls draw_geom.
  var update_geom = function (g_name, selector_name) {
    var g_info = Geoms[g_name];
    // First apply chunk_order selector variables.
    var chunk_id = g_info.chunks;
    g_info.chunk_order.forEach(function (v_name) {
      if(chunk_id == null){
        return; // no data in a higher up chunk var.
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
	      //.attr("x", svg.attr("width")/2)
        .attr("y", 10)
        .style("fill", "red");
      download_chunk(g_info, tsv_name, function(chunk){
      	loading.remove();
	      draw_panels(g_info, chunk, selector_name);
      });
    }
  };
  var draw_panels = function(g_info, chunk, selector_name) {
    // derive the plot name from the geometry name
    var g_names = g_info.classed.split("_");
    var p_name = g_names[g_names.length - 1];
    var panels = Plots[p_name].layout.PANEL;
    panels.forEach(function(panel) {
      draw_geom(g_info, chunk, selector_name, panel);
    });
  };

  function download_next(g_name){
    var g_info = Geoms[g_name];
    var selector_value = Animation.sequence[g_info.seq_i];
    var chunk_id = g_info.chunks[selector_value];
    var tsv_name = get_tsv(g_info, chunk_id);
    g_info.seq_count += 1;
    if(Animation.sequence.length == g_info.seq_count){
      Animation.done_geoms[g_name] = 1;
      return;
    }
    g_info.seq_i += 1;
    if(g_info.seq_i == Animation.sequence.length){
      g_info.seq_i = 0;
    }
    if(typeof(chunk_id) == "string"){
      download_chunk(g_info, tsv_name, function(chunk){
	download_next(g_name);
      })
    }else{
      download_next(g_name);
    }
  }

  // download_chunk is called from update_geom and download_next.
  function download_chunk(g_info, tsv_name, funAfter){
    if(g_info.download_status.hasOwnProperty(tsv_name)){
      funAfter();
      return; // do not download twice.
    }
    g_info.download_status[tsv_name] = "downloading";
    // prefix tsv file with appropriate path
    var tsv_file = getTSVpath(tsv_name);
    d3.tsv(tsv_file, function (error, response) {
      // First convert to correct types.
      g_info.download_status[tsv_name] = "processing";
      response = convert_R_types(response, g_info.types);
      wait_until_then(500, function(){
	if(g_info.common_tsv) {
          return g_info.data.hasOwnProperty(g_info.common_tsv);
	}else{
	  return true;
	}
      }, function(){
	if(g_info.common_tsv) {
          // copy data from common tsv to varied tsv
          response = copy_chunk(g_info, response);
	}
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
    });
  }//download_chunk.

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
      var selected, values;
      var new_arrays = [];
      if(0 < aes_name.indexOf(".variable")){ 
	selected_arrays.forEach(function(old_array){
	  var some_data = chunk;
	  old_array.forEach(function(value){
            if(some_data.hasOwnProperty(value)) {
              some_data = some_data[value];
            } else {
              some_data = {};
            }
	  })
	  values = d3.keys(some_data);
	  values.forEach(function(s_name){
	    var selected = Selectors[s_name].selected;
	    var new_array = old_array.concat(s_name).concat(selected);
	    new_arrays.push(new_array);
	  })
	})
      }else{//not .variable aes:
	if(aes_name == "PANEL"){
	  selected = PANEL;
	}else{
          var s_name = g_info.aes[aes_name];
          selected = Selectors[s_name].selected;
	}
	if(isArray(selected)){ 
	  values = selected; //multiple selection.
	}else{
	  values = [selected]; //single selection.
	}
	values.forEach(function(value){
	  selected_arrays.forEach(function(old_array){
	    var new_array = old_array.concat(value);
	    new_arrays.push(new_array);
	  })
	})
      }
      selected_arrays = new_arrays;
    });
    // data can be either an array[] if it will be directly involved
    // in a data-bind, or an object{} if it will be involved in a
    // data-bind by group (e.g. geom_line).
    var data;
    if(g_info.data_is_object){
      data = {};
    }else{
      data = [];
    }
    selected_arrays.forEach(function(value_array){
      var some_data = chunk;
      value_array.forEach(function(value){
        if (some_data.hasOwnProperty(value)) {
          some_data = some_data[value];
        } else {
	  if(g_info.data_is_object){
	    some_data = {};
	  }else{
            some_data = [];
	  }
        }
      });
      if(g_info.data_is_object){
	if(isArray(some_data) && some_data.length){
	  data["0"] = some_data;
	}else{
	  for(k in some_data){
            data[k] = some_data[k];
          }
	}
      }else{//some_data is an array.
        data = data.concat(some_data);
      }
    });
    var aes = g_info.aes;
    var toXY = function (xy, a) {
      return function (d) {
        return scales[xy](d[a]);
      };
    };
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
    };
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
    };

    var linetype = "solid";
    if (g_info.params.linetype) {
      linetype = g_info.params.linetype;
    }

    var get_dasharray = function (d) {
      var lt = linetype;
      if (aes.hasOwnProperty("linetype") && d.hasOwnProperty("linetype")) {
        lt = d["linetype"];
      }
      return linetypesize2dasharray(lt, get_size(d));
    };
    var colour = "black";
    var fill = "black";
    var get_colour = function (d) {
      if (d.hasOwnProperty("colour")) {
        return d["colour"]
      }
      return colour;
    };
    var get_fill = function (d) {
      if (d.hasOwnProperty("fill")) {
        return d["fill"];
      }
      return fill;
    };
    if (g_info.params.colour) {
      colour = g_info.params.colour;
    }
    if (g_info.params.fill) {
      fill = g_info.params.fill;
    }else if(g_info.params.colour){
      fill = g_info.params.colour;
    }

    // For aes(hjust) the compiler should make an "anchor" column.
    var text_anchor = "middle";
    if(g_info.params.hasOwnProperty("anchor")){
      text_anchor = g_info.params["anchor"];
    }
    var get_text_anchor;
    if(g_info.aes.hasOwnProperty("hjust")) {
      get_text_anchor = function(d){
	return d["anchor"];
      }
    }else{
      get_text_anchor = function(d){
	return text_anchor;
      }
    }

    var eActions, eAppend, linkActions;
    var key_fun = null;
    var id_fun = function(d){
      return d.id;
    };
    if(g_info.aes.hasOwnProperty("key")){
      key_fun = function(d){
        return d.key;
      };
    }
    if(g_info.data_is_object) {

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

      // In order to get d3 lines to play nice, bind fake "data" (group
      // id's) -- the kv variable. Then each separate object is plotted
      // using path (case of only 1 thing and no groups).

      // we need to use a path for each group.
      var keyed_data = {}, one_group, group_id, k;
      for(group_id in data){
	one_group = data[group_id];
	one_row = one_group[0];
	if(one_row.hasOwnProperty("key")){
	  k = one_row.key;
	}else{
	  k = group_id;
	}
	keyed_data[k] = one_group;
      }
      var kv_array = d3.entries(d3.keys(keyed_data));
      var kv = kv_array.map(function (d) {
        //d[aes.group] = d.value;

        // Need to store the clickSelects value that will
        // be passed to the selector when we click on this
        // item.
        d.clickSelects = keyed_data[d.value][0].clickSelects;
        return d;
      });

      // line, path, and polygon use d3.svg.line(),
      // ribbon uses d3.svg.area()
      // we have to define lineThing accordingly.
      if (g_info.geom == "ribbon") {
        var lineThing = d3.svg.area()
          .x(toXY("x", "x"))
          .y(toXY("y", "ymax"))
          .y0(toXY("y", "ymin"));
      } else {
        var lineThing = d3.svg.line()
          .x(toXY("x", "x"))
          .y(toXY("y", "y"));
      }
      // select the correct group before returning anything.
      key_fun = function(group_info){
	return group_info.value;
      };
      id_fun = function(group_info){
        var one_group = keyed_data[group_info.value];
        var one_row = one_group[0];
	      // take key from first value in the group.
	      return one_row.id;
      };
      elements = elements.data(kv, key_fun);
      linkActions = function(a_elements){
	a_elements
	  .attr("xlink:href", function(group_info){
            var one_group = keyed_data[group_info.value];
            var one_row = one_group[0];
	    return one_row.href;
	  })
          .attr("target", "_blank")
          .attr("class", "geom")
	;
      };
      eActions = function (e) {
        e.attr("d", function (d) {
          var one_group = keyed_data[d.value];
          // filter NaN since they make the whole line disappear!
	  var no_na = one_group.filter(function(d){
            if(g_info.geom == "ribbon"){
              return !isNaN(d.x) && !isNaN(d.ymin) && !isNaN(d.ymax);
            }else{
              return !isNaN(d.x) && !isNaN(d.y);
            }
          });
          return lineThing(no_na);
        })
          .style("fill", function (group_info) {
            if (g_info.geom == "line" || g_info.geom == "path") {
              return "none";
            }
            var one_group = keyed_data[group_info.value];
            var one_row = one_group[0];
            // take color for first value in the group
            return get_fill(one_row);
          })
          .style("stroke-width", function (group_info) {
            var one_group = keyed_data[group_info.value];
            var one_row = one_group[0];
  	        // take size for first value in the group
            return get_size(one_row);
          })
          .style("stroke", function (group_info) {
            var one_group = keyed_data[group_info.value];
            var one_row = one_group[0];
  	        // take color for first value in the group
            return get_colour(one_row);
          })
          .style("stroke-dasharray", function (group_info) {
            var one_group = keyed_data[group_info.value];
            var one_row = one_group[0];
  	        // take linetype for first value in the group
            return get_dasharray(one_row);
          })
          .style("stroke-width", function (group_info) {
            var one_group = keyed_data[group_info.value];
            var one_row = one_group[0];
  	        // take line size for first value in the group
            return get_size(one_row);
          });
      };
      eAppend = "path";
    }else{
      linkActions = function(a_elements){
	a_elements.attr("xlink:href", function(d){ return d.href; })
          .attr("target", "_blank")
          .attr("class", "geom");
      };
    }
    if (g_info.geom == "segment") {
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
      };
      eAppend = "line";
    }
    if (g_info.geom == "linerange") {
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
      };
      eAppend = "line";
    }
    if (g_info.geom == "vline") {
      elements = elements.data(data, key_fun);
      eActions = function (e) {
        e.attr("x1", toXY("x", "xintercept"))
          .attr("x2", toXY("x", "xintercept"))
          .attr("y1", scales.y.range()[0])
          .attr("y2", scales.y.range()[1])
          .style("stroke-dasharray", get_dasharray)
          .style("stroke-width", get_size)
          .style("stroke", get_colour);
      };
      eAppend = "line";
    }
    if (g_info.geom == "hline") {
      // pretty much a copy of geom_vline with obvious modifications
      elements = elements.data(data, key_fun);
      eActions = function (e) {
        e.attr("y1", toXY("y", "yintercept"))
          .attr("y2", toXY("y", "yintercept"))
          .attr("x1", scales.x.range()[0])
          .attr("x2", scales.x.range()[1])
          .style("stroke-dasharray", get_dasharray)
          .style("stroke-width", get_size)
          .style("stroke", get_colour);
      };
      eAppend = "line";
    }
    if (g_info.geom == "text") {
      elements = elements.data(data, key_fun);
      // TODO: how to support vjust? firefox doensn't support
      // baseline-shift... use paths?
      // http://commons.oreilly.com/wiki/index.php/SVG_Essentials/Text
      eActions = function (e) {
        e.attr("x", toXY("x", "x"))
          .attr("y", toXY("y", "y"))
          .style("fill", get_colour)
          .attr("font-size", get_size)
          .style("text-anchor", get_text_anchor)
          .text(function (d) {
            return d.label;
          });
      };
      eAppend = "text";
    }
    if (g_info.geom == "point") {
      elements = elements.data(data, key_fun);
      eActions = function (e) {
        e.attr("cx", toXY("x", "x"))
          .attr("cy", toXY("y", "y"))
          .attr("r", get_size)
          .style("fill", get_fill)
          .style("stroke", get_colour);
      };
      eAppend = "circle";
    }
    if (g_info.geom == "jitter") {
      elements = elements.data(data, key_fun);
      eActions = function (e) {
        e.attr("cx", toXY("x", "x"))
          .attr("cy", toXY("y", "y"))
          .attr("r", get_size)
          .style("fill", get_fill)
          .style("stroke", get_colour);
      };
      eAppend = "circle";
    }
    if (g_info.geom == "tallrect") {
      elements = elements.data(data, key_fun);
      eActions = function (e) {
        e.attr("x", toXY("x", "xmin"))
          .attr("width", function (d) {
            return scales.x(d["xmax"]) - scales.x(d["xmin"]);
          })
          .attr("y", scales.y.range()[1])
          .attr("height", scales.y.range()[0] - scales.y.range()[1])
          .style("fill", get_fill)
          .style("stroke-dasharray", get_dasharray)
          .style("stroke-width", get_size)
          .style("stroke", get_colour);
      };
      eAppend = "rect";
    }
    if (g_info.geom == "widerect") {
      elements = elements.data(data, key_fun);
      eActions = function (e) {
        e.attr("y", toXY("y", "ymax"))
          .attr("height", function (d) {
            return scales.y(d["ymin"]) - scales.y(d["ymax"]);
          })
          .attr("x", scales.x.range()[0])
          .attr("width", scales.x.range()[1] - scales.x.range()[0])
          .style("fill", get_fill)
          .style("stroke-dasharray", get_dasharray)
          .style("stroke-width", get_size)
          .style("stroke", get_colour);
      };
      eAppend = "rect";
    }
    if (g_info.geom == "rect") {
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
      };
      eAppend = "rect";
    }
    if (g_info.geom == "boxplot") {

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
      };
    }
    elements.exit().remove();
    var enter = elements.enter();
    if(g_info.aes.hasOwnProperty("href")){
      enter = enter.append("svg:a")
        .append("svg:"+eAppend);
    }else{
      enter = enter.append(eAppend)
	      .attr("class", "geom");
    }
    var has_clickSelects = g_info.aes.hasOwnProperty("clickSelects");
    var has_clickSelects_variable =
      g_info.aes.hasOwnProperty("clickSelects.variable");
    if (has_clickSelects || has_clickSelects_variable) {
      var selected_funs = {
	"opacity":{
	  "mouseout":function (d) {
	    var alpha_on = get_alpha(d);
	    var alpha_off = get_alpha(d) - 0.5;
	    if(has_clickSelects){
              return ifSelectedElse(d.clickSelects, g_info.aes.clickSelects,
				    alpha_on, alpha_off);
	    }else if(has_clickSelects_variable){
	      return ifSelectedElse(d["clickSelects.value"],
				    d["clickSelects.variable"],
				    alpha_on, alpha_off);
	    }
	  },
	  "mouseover":function (d) {
            return get_alpha(d);
	  }
	},
	"stroke":{
	  "mouseout":function(d){
	    var stroke_on = "black";
	    var stroke_off = "transparent";
	    if(has_clickSelects){
	      return ifSelectedElse(d.clickSelects, g_info.aes.clickSelects,
				    stroke_on, stroke_off);
	    }else{
	      return ifSelectedElse(d["clickSelects.value"],
				    d["clickSelects.variable"],
				    stroke_on, stroke_off);
	    }
	  },
	  "mouseover":function(d){
	    return "black";
	  }
	}
      }; //selected_funs.
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
      };
      var out_fun = function(e){
        e.style(g_info.select_style, style_funs["mouseout"]);
      };
      elements.call(out_fun)
        .on("mouseover", function (d) {
          d3.select(this).call(over_fun);
        })
        .on("mouseout", function (d) {
          d3.select(this).call(out_fun);
        })
      ;
      if(has_clickSelects){
	elements.on("click", function (d) {
	  // The main idea of how clickSelects works: when we click
	  // something, we call update_selector with the clicked
	  // value.
            var s_name = g_info.aes.clickSelects;
            update_selector(s_name, d.clickSelects);
	});
      }else{
	elements.on("click", function(d){
	  var s_name = d["clickSelects.variable"];
	  var s_value = d["clickSelects.value"];
	  update_selector(s_name, s_value);
	});
      }
    }else{//has neither clickSelects nor clickSelects.variable.
      elements.style("opacity", get_alpha);
    }
    var has_tooltip = g_info.aes.hasOwnProperty("tooltip");
    if(has_clickSelects || has_tooltip || has_clickSelects_variable){
      var text_fun, get_one;
      if(g_info.data_is_object){
	get_one = function(d_or_kv){
          var one_group = keyed_data[d_or_kv.value];
	  return one_group[0];
        };
      }else{
	get_one = function(d_or_kv){ 
	  return d_or_kv;
	};
      }
      if(has_tooltip){
        text_fun = function(d){
	  return d.tooltip;
	};
      }else if(has_clickSelects){
	text_fun = function(d){
          var v_name = g_info.aes.clickSelects;
          return v_name + " " + d.clickSelects;
	};
      }else{ //clickSelects_variable
	text_fun = function(d){
	  return d["clickSelects.variable"] + " " + d["clickSelects.value"];
	};
      }
      elements.text("")
        .append("svg:title")
        .text(function(d_or_kv){
	  var d = get_one(d_or_kv);
	  return text_fun(d);
	})
      ;
    }
    // Set attributes of only the entering elements. This is needed to
    // prevent things from flying around from the upper left when they
    // enter the plot.
    eActions(enter);  // DO NOT DELETE!
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
      // d3.select(linked_geoms).data(data, key_fun); // WHY did we need this?
      eActions(linked_geoms);
      linkActions(elements);
    }else{
      // elements are e.g. <circle>
      eActions(elements); // Set the attributes of all elements (enter/exit/stay)
    }
  };

  var update_selector = function (v_name, value) {
    value = value + "";
    var s_info = Selectors[v_name];
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
    // if there are levels, then there is a selectize widget which
    // should be updated.
    if(isArray(s_info.levels)){
      // the jquery ids
      if(s_info.type == "single") {
	var selected_ids = v_name.concat("___", value);
      } else {
	var selected_ids = [];
	for(i in s_info.selected) {
          selected_ids[i] = v_name.concat("___", s_info.selected[i]);
	}
      }
      // from
      // https://github.com/brianreavis/selectize.js/blob/master/docs/api.md:
      // setValue(value, silent) If "silent" is truthy, no change
      // event will be fired on the original input.
      selectized_array[v_name].setValue(selected_ids, true);
    }
    update_legend_opacity(v_name);
    s_info.update.forEach(function(g_name){
      update_geom(g_name, v_name);
    });
  };

  var ifSelectedElse = function (s_value, s_name, selected, not_selected) {
    var is_selected;
    var s_info = Selectors[s_name];
    if(s_info.type == "single"){
      is_selected = s_value == s_info.selected;
    }else{
      is_selected = s_info.selected.indexOf(s_value) != -1;
    }
    if(is_selected){
      return selected;
    } else {
      return not_selected;
    }
  };
  
  function update_next_animation(){
    var values = d3.values(Animation.done_geoms);
    if(d3.sum(values) == values.length){
      // If the values in done_geoms are all 1, then we have loaded
      // all of the animation-related chunks, and we can start
      // playing the animation.
      var v_name = Animation.variable;
      var cur = Selectors[v_name].selected;
      var next = Animation.next[cur];
      update_selector(v_name, next);
    }
  }

  // The main idea of how legends work:

  // 1. In getLegend in animint.R I export the legend entries as a
  // list of rows that can be used in a data() bind in D3.

  // 2. Here in add_legend I create a <table> for every legend, and
  // then I bind the legend entries to <tr>, <td>, and <svg> elements.
  var add_legend = function(p_name, p_info){
    // case of multiple legends, d3 reads legend structure in as an array
    var tdRight = element.select("td."+p_name+"_legend");
    var legendkeys = d3.keys(p_info.legend);
    for(var i=0; i<legendkeys.length; i++){
      var legend_key = legendkeys[i];
      var l_info = p_info.legend[legend_key];
      // the table that contains one row for each legend element.
      var legend_table = tdRight.append("table")
	.attr("class", "legend")
      ;
      var legend_class = legend_class_name(l_info["class"]);
      var legend_id = p_info.plot_id + "_" + legend_class;
      // the legend table with breaks/value/label .
      // TODO: variable and value should be set in the compiler! What
      // if label is different from the data value?
      for(var entry_i=0; entry_i < l_info.entries.length; entry_i++){
	var entry = l_info.entries[entry_i];
	entry.variable = l_info.selector;
	entry.value = entry.label;
	entry.id = safe_name(legend_id + "_" + entry["label"]);
      }
      var legend_rows = legend_table.selectAll("tr")
        .data(l_info.entries)
        .enter()
        .append("tr")
      // in a good data viz there should not be more than one legend
      // that shows the same thing, so there should be no duplicate
      // id.
        .attr("id", function(d) { return d["id"]; })
	.attr("class", legend_class)
      ;
      if(l_info.selector != null){
	legend_rows
	  .on("click", function(d) { 
            update_selector(d.variable, d.value);
	  })
	  .attr("title", function(d) {
            return "Toggle " + d.value;
	  })
	  .attr("style", "cursor:pointer")
	;
      }
      var first_tr = legend_table.insert("tr", "tr");
      var first_th = first_tr.append("th")
	.attr("align", "left")
	.attr("colspan", 2)
        .text(l_info.title)
        .attr("class", legend_class)
      ;
      var legend_svgs = legend_rows.append("td")
        .append("svg")
  	    .attr("id", function(d){return d["id"]+"_svg";})
  	    .attr("height", 14)
  	    .attr("width", 20);
      var pointscale = d3.scale.linear().domain([0,7]).range([1,4]);
      // scale points so they are visible in the legend. (does not
      // affect plot scaling)
      var linescale = d3.scale.linear().domain([0,6]).range([1,4]);
      // scale lines so they are visible in the legend. (does not
      // affect plot scaling)
      if(l_info.geoms.indexOf("polygon")>-1){
        // aesthetics that would draw a rect
        legend_svgs.append("rect")
          .attr("x", 2)
	        .attr("y", 2)
	        .attr("width", 10)
	        .attr("height", 10)
          .style("stroke-width", function(d){return d["polygonsize"]||1;})
          .style("stroke-dasharray", function(d){
            return linetypesize2dasharray(d["polygonlinetype"], d["size"]||2);
          })
          .style("stroke", function(d){return d["polygoncolour"] || "#000000";})
          .style("fill", function(d){return d["polygonfill"] || "#FFFFFF";})
          .style("opacity", function(d){return d["polygonalpha"]||1;});
      }
      if(l_info.geoms.indexOf("text")>-1){
        // aesthetics that would draw a rect
        legend_svgs.append("text")
	        .attr("x", 10)
	        .attr("y", 14)
          .style("fill", function(d){return d["textcolour"]||1;})
	        .style("text-anchor", "middle")
	        .attr("font-size", function(d){return d["textsize"]||1;})
	        .text("a");
      }
      if(l_info.geoms.indexOf("path")>-1){
        // aesthetics that would draw a line
        legend_svgs.append("line")
          .attr("x1", 1).attr("x2", 19).attr("y1", 7).attr("y2", 7)
          .style("stroke-width", function(d){
            return linescale(d["pathsize"])||2;
          })
          .style("stroke-dasharray", function(d){
            return linetypesize2dasharray(d["pathlinetype"], d["pathsize"] || 2);
          })
          .style("stroke", function(d){return d["pathcolour"] || "#000000";})
          .style("opacity", function(d){return d["pathalpha"]||1;});
      }
      if(l_info.geoms.indexOf("point")>-1){
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
	.attr("align", "left") // TODO: right for numbers?
	.attr("class", "legend_entry_label")
	.attr("id", function(d){ return d["id"]+"_label"; })
	.text(function(d){ return d["label"];});
    }
  }

  // Download the main description of the interactive plot.
  d3.json(json_file, function (error, response) {
    if(response.hasOwnProperty("title")){
      // This selects the title of the web page, outside of wherever
      // the animint is defined, usually a <div> -- so it is OK to use
      // global d3.select here.
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
    
    ////////////////////////////////////////////
    // Widgets at bottom of page
    ////////////////////////////////////////////
    element.append("br");
      
    // loading table.
    var show_hide_table = element.append("button")
      .text("Show download status table");
    show_hide_table
      .on("click", function(){
        if(this.textContent == "Show download status table"){
          loading.style("display", "");
          show_hide_table.text("Hide download status table");
        }else{
          loading.style("display", "none");
          show_hide_table.text("Show download status table");
        }
      });
    var loading = element.append("table")
      .style("display", "none");
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
    // add a button to view the animation widgets
    var show_hide_animation_controls = element.append("button")
      .text(show_message)
      .attr("id", viz_id + "_show_hide_animation_controls")
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
    // table of the animint widgets
    var time_table = element.append("table")
      .style("display", "none");
    var first_tr = time_table.append("tr");
    var first_th = first_tr.append("th");
    // if there's a time variable, add a button to pause the animint
    if(response.time){
      Animation.next = {};
      Animation.ms = response.time.ms;
      Animation.variable = response.time.variable;
      Animation.sequence = response.time.sequence;
      Widgets["play_pause"] = first_th.append("button")
	.text("Play")
        .attr("id", "play_pause")
	.on("click", function(){
          if(this.textContent == "Play"){
            Animation.play();
          }else{
            Animation.pause(false);
          }
        })
      ;
    }
    first_tr.append("th").text("milliseconds");
    if(response.time){
      var second_tr = time_table.append("tr");
      second_tr.append("td").text("updates");
      second_tr.append("td").append("input")
	.attr("id", "updates_ms")
	.attr("type", "text")
	.attr("value", Animation.ms)
	.on("change", function(){
          Animation.pause(false);
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
      .append("tr");
    duration_rows
      .append("td")
      .text(function(s_name){return s_name;});
    var duration_tds = duration_rows.append("td");
    var duration_inputs = duration_tds
      .append("input")
      .attr("id", function(s_name){
        return viz_id + "_duration_ms_" + s_name;
      })
      .attr("type", "text")
      .on("change", function(s_name){
        Selectors[s_name].duration = this.value;
      })
      .attr("value", function(s_name){
        return Selectors[s_name].duration;
      });
    // selector widgets
    var toggle_message = "Show selection menus";
    var show_or_hide_fun = function(){
      if(this.textContent == toggle_message){
        selector_table.style("display", "");
        show_hide_selector_widgets.text("Hide selection menus");
      }else{
        selector_table.style("display", "none");
        show_hide_selector_widgets.text(toggle_message);
      }
    }
    var show_hide_selector_widgets = element.append("button")
      .text(toggle_message)
      .attr("class", "show_hide_selector_widgets")
      .on("click", show_or_hide_fun)
    ;
    // adding a table for selector widgets
    var selector_table = element.append("table")
      .style("display", "none")
      .attr("class", "table_selector_widgets")
    ;
    var selector_first_tr = selector_table.append("tr");
    selector_first_tr
      .append("th")
      .text("Variable")
    ;
    selector_first_tr
      .append("th")
      .text("Selected value(s)")
    ;
      
     // looping through and adding a row for each selector
    for(s_name in Selectors) {
      var s_info = Selectors[s_name];
      // for .variable .value selectors, levels is undefined and we do
      // not want to make a selectize widget.

      // TODO: why does it take so long to initialize the selectize
      // widget when there are many (>1000) values?
      if(isArray(s_info.levels)){
	// If there were no geoms that specified clickSelects for this
	// selector, then there is no way to select it other than the
	// selectize widgets (and possibly legends). So in this case
	// we show the selectize widgets by default.
	var selector_widgets_hidden = 
	  show_hide_selector_widgets.text() == toggle_message;
	var has_no_clickSelects = 
	  !Selectors[s_name].hasOwnProperty("clickSelects")
	var has_no_legend = 
	  !Selectors[s_name].hasOwnProperty("legend")
	if(selector_widgets_hidden && has_no_clickSelects && has_no_legend){
	  var node = show_hide_selector_widgets.node();
	  show_or_hide_fun.apply(node);
	}
	// removing "." from name so it can be used in ids
	var s_name_id = legend_class_name(s_name);

	// adding a row for each selector
	var selector_widget_row = selector_table
          .append("tr")
          .attr("class", function() { return s_name_id + "_selector_widget"; })
	;
	selector_widget_row.append("td").text(s_name);
	// adding the selector
	var selector_widget_select = selector_widget_row
          .append("td")
          .append("select")
          .attr("class", function() { return s_name_id + "_input"; })
          .attr("placeholder", function() { return "Toggle " + s_name; });
	// adding an option for each level of the variable
	selector_widget_select.selectAll("option")
          .data(s_info.levels)
          .enter()
          .append("option")
          .attr("value", function(d) { return d; })
          .text(function(d) { return d; });
	// making sure that the first option is blank
	selector_widget_select
          .insert("option")
          .attr("value", "")
          .text(function() { return "Toggle " + s_name; });
	
	// calling selectize
	var selectize_selector = to_select + ' .' + s_name_id + "_input";
	if(s_info.type == "single") {
          // setting up array of selector and options
          var selector_values = [];
          for(i in s_info.levels) {
            selector_values[i] = {
              id: s_name.concat("___", s_info.levels[i]), 
              text: s_info.levels[i]
            };
          }
          // the id of the first selector
          var selected_id = s_name.concat("___", s_info.selected);

          // if single selection, only allow one item
          var $temp = $(selectize_selector)
            .selectize({
              create: false, 
              valueField: 'id',
              labelField: 'text',
              searchField: ['text'],
              options: selector_values, 
              items: [selected_id],
              maxItems: 1, 
              allowEmptyOption: true,
              onChange: function(value) {
		// extracting the name and the level to update
		var selector_name = value.split("___")[0];
		var selected_level = value.split("___")[1];
		// updating the selector
		update_selector(selector_name, selected_level);
              }
            })
          ;
	} else { // multiple selection:
          // setting up array of selector and options
          var selector_values = [];
          if(typeof s_info.levels == "object") {
            for(i in s_info.levels) {
              selector_values[i] = {
		id: s_name.concat("___", s_info.levels[i]), 
		text: s_info.levels[i]
              };
            }
          } else {
            selector_values[0] = {
              id: s_name.concat("___", s_info.levels), 
              text: s_info.levels
            };
          }
          // setting up an array to contain the initally selected elements
          var initial_selections = [];
          for(i in s_info.selected) {
            initial_selections[i] = s_name.concat("___", s_info.selected[i]);
          }
          
          // construct the selectize
          var $temp = $(selectize_selector)
            .selectize({
              create: false, 
              valueField: 'id',
              labelField: 'text',
              searchField: ['text'],
              options: selector_values, 
              items: initial_selections,
              maxItems: s_info.levels.length, 
              allowEmptyOption: true,
              onChange: function(value) { 
		// if nothing is selected, remove what is currently selected
		if(value == null) {
                  // extracting the selector ids from the options
                  var the_ids = Object.keys($(this)[0].options);
                  // the name of the appropriate selector
                  var selector_name = the_ids[0].split("___")[0];
                  // the previously selected elements
                  var old_selections = Selectors[selector_name].selected;
                  // updating the selector for each of the old selections
                  old_selections.forEach(function(element) {
                    update_selector(selector_name, element);
                  });
		} else { // value is not null:
                  // grabbing the name of the selector from the selected value
                  var selector_name = value[0].split("___")[0];
                  // identifying the levels that should be selected
                  var specified_levels = [];
                  for(i in value) {
                    specified_levels[i] = value[i].split("___")[1];
                  }
                  // the previously selected entries
                  old_selections = Selectors[selector_name].selected;
                  
                  // the levels that need to have selections turned on
                  specified_levels
                    .filter(function(n) {
                      return old_selections.indexOf(n) == -1;
                    })
                    .forEach(function(element) {
                      update_selector(selector_name, element);
                    })
                  ;
                  // the levels that need to be turned off
                  // - same approach
                  old_selections
                    .filter(function(n) {
                      return specified_levels.indexOf(n) == -1;
                    })
                    .forEach(function(element) {
                      update_selector(selector_name, element);
                    })
                  ;
		}//value==null
              }//onChange
            })//selectize
          ;
	}//single or multiple selection.
	selectized_array[s_name] = $temp[0].selectize;
      }//levels, is.variable.value
    } // close for loop through selector widgets
    // If this is an animation, then start downloading all the rest of
    // the data, and start the animation.
    if (response.time) {
      var i, prev, cur;
      for (var i = 0; i < Animation.sequence.length; i++) {
        if (i == 0) {
          prev = Animation.sequence[Animation.sequence.length-1];
        } else {
          prev = Animation.sequence[i - 1];
        }
        cur = Animation.sequence[i];
        Animation.next[prev] = cur;
      }
      Animation.timer = null;
      Animation.play = function(){
	if(Animation.timer == null){ // only play if not already playing.
    	  // as shown on http://bl.ocks.org/mbostock/3808234
    	  Animation.timer = setInterval(update_next_animation, Animation.ms);
    	  Widgets["play_pause"].text("Pause");
	}
      };
      Animation.play_after_visible = false;
      Animation.pause = function(play_after_visible){
        Animation.play_after_visible = play_after_visible;
        clearInterval(Animation.timer);
	Animation.timer = null;
        Widgets["play_pause"].text("Play");
      };
      var s_info = Selectors[Animation.variable];
      Animation.done_geoms = {};
      s_info.update.forEach(function(g_name){
        var g_info = Geoms[g_name];
        if(g_info.chunk_order.length == 1 &&
	   g_info.chunk_order[0] == Animation.variable){
	  g_info.seq_i = Animation.sequence.indexOf(s_info.selected);
	  g_info.seq_count = 0;
	  Animation.done_geoms[g_name] = 0;
	  download_next(g_name);
	}
      });
      Animation.play();
      all_geom_names = d3.keys(response.geoms);

      // This code starts/stops the animation timer when the page is
      // hidden, inspired by
      // http://stackoverflow.com/questions/1060008
      function onchange (evt) {
        if(document.visibilityState == "visible"){
          if(Animation.play_after_visible){
            Animation.play();
          }
        }else{
          if(Widgets["play_pause"].text() == "Pause"){
            Animation.pause(true);
          }
        }
      };
      document.addEventListener("visibilitychange", onchange);
    }
  });
};
