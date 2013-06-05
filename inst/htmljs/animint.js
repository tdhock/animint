// Define functions to render linked interactive plots using d3.

// Another script should define e.g.
// <script>
//   var plot = new animint("#plot","plot.json");
// </script>


// Constructor for animint Object.
var animint = function(to_select, json_file){
    var element = d3.select(to_select);
    this.element = element;
    var Selectors = {};
    this.Selectors = Selectors;
    var Plots = {};
    this.Plots = Plots;
    var Geoms = {};
    this.Geoms = Geoms;
    var SVGs = {};
    this.SVGs = SVGs;
    var Animation = {};
    this.Animation = Animation;
    var getcol = function(v_name){
      return function(d){return d[v_name];};
    }
    var add_geom = function(g_name, g_info){
    	d3.csv(g_info.data, function(error, response){
    	    // First convert to correct types.
    	  response.forEach(function(d){
      		for(var v_name in g_info.types){
      		    var r_type = g_info.types[v_name];
      		    if(r_type == "integer"){
      			d[v_name] = parseInt(d[v_name]);
      		    }else if(r_type == "numeric"){
      			d[v_name] = parseFloat(d[v_name]);
      		    }else if(r_type == "factor"){
      			//keep it as a character.
      		    }else{
      			throw "unsupported R type "+r_type;
      		    }
    		    }
    	  });
    	  var nest = d3.nest();
    	  g_info.subord.forEach(function(aes_name){
      		var v_name = g_info.subvars[aes_name];
      		//alert(aes_name+" "+g_info.classed+" "+v_name);
      		nest.key(getcol(v_name));
    	  });
    	  g_info.data = nest.map(response);
    	  Geoms[g_name] = g_info;
    	  update_geom(g_name);
    	});
    }
    var add_plot = function(p_name, p_info){
    	var svg = element.append("svg")
    	    .attr("id",p_name)
    	    .attr("height",p_info.options.height)
    	    .attr("width",p_info.options.width);
      var padding = 30;
      var h = svg.attr("height");
      var w = svg.attr("width");
    	svg.x = d3.scale.linear()
    	    .domain(p_info.ranges.x)
    	    .range([padding,svg.attr("width")-padding]);
    	svg.y = d3.scale.linear()
    	    .domain(p_info.ranges.y)
    	    .range([svg.attr("height")-padding,padding]);
      var xaxis = d3.svg.axis().scale(svg.x).orient("bottom")
          .tickValues({return p_info.axis.x*svg.attr("width")-padding})
          .tickFormat(function(d) {return p_info.axis.xlab[p_info.axis.x.indexOf(d)]});
        svg.append("g")
          .attr("class", "axis")
          .attr("transform", "translate(0," + (h-padding) + ")")
          .call(xaxis)
          .tickValues({return p_info.axis.x*svg.attr("width")-padding})
          .tickFormat(function(d) {return p_info.axis.xlab[p_info.axis.x.indexOf(d)]});
      var yaxis = d3.svg.axis().scale(svg.y).orient("left");
        svg.append("g")
          .attr("class", "axis")
          .attr("transform", "translate("+padding+",0)")
          .call(yaxis);
    	svg.plot = p_info;
    	p_info.geoms.forEach(function(g_name){
    	    SVGs[g_name] = svg;
	    });

	    Plots[p_name] = p_info;
    }
    var add_selector = function(s_name, s_info){
    	Selectors[s_name] = s_info;
    }
    var update_geom = function(g_name){
  	var svg = SVGs[g_name];
  	var g_info = Geoms[g_name];
  	var data = g_info.data;
  	g_info.subord.forEach(function(aes_name){
  	    if(aes_name != "group"){
  		var v_name = g_info.subvars[aes_name];
  		var value = Selectors[v_name].selected;
  		if(data.hasOwnProperty(value)){
  		    data = data[value];
  		}else{
  		    data = [];
  		}
  	    }
  	});
	var aes = g_info.aes;
	var toXY = function(xy, a){
	    return function(d){
		return svg[xy](d[ aes[a] ]);
	    }
	}
	var elements = svg.selectAll("."+g_info.classed);

	// TODO: set all of these on a per-item basis. Currently, only
	// linetype and fill have some support for manual scales. This
	// needs to be standardized!
	var base_opacity = 1;
	if(g_info.params.alpha){
	    base_opacity = g_info.params.alpha;
	}
	var size = 2;
	if(g_info.params.size){
	    size = g_info.params.size;
	}
	var get_size = function(d){
	    if(aes.hasOwnProperty("size") && d.hasOwnProperty(aes.size)){
		return d[ aes.size ];
	    }
	    return size;
	}
	var linetypesize2dasharray = function(linetype, size){
	    return {
		"dashed":size*4+","+size*4,
		"solid":null,
		"dotted":size+","+size*2,
	    }[linetype];
	}
	var get_dasharray = function(d){
	    var value, lt;
	    try{
		value = d[aes.linetype];
		lt = svg.plot.scales.linetype[value];
	    }catch(err){
		lt = g_info.params.linetype;
	    }
	    return linetypesize2dasharray(lt, get_size(d));
	}
	var get_fill = function(d){
    var value, fl;
	    try{
        value = d[aes.fill];
		    fl = svg.plot.scales.fill[ value ];
	    }catch(err){
		    fl =  g_info.params.fill;
	    }
    return fl;
	}
  var get_colour = function(d){
	    try{
		return svg.plot.scales.colour[ d[aes.colour] ];
	    }catch(err){
		return g_info.params.colour;
	    }
	}
	var colour = "black";
	if(g_info.params.colour){
	    colour = g_info.params.colour;
	}
	var text_anchor = "middle";
	if(g_info.params.hjust == 0){
	    text_anchor = "start";
	}
	if(g_info.params.hjust == 1){
	    text_anchor = "end";
	}

	var eActions, eAppend;
	if(g_info.geom == "line"){
	    // case of only 1 line and no groups.
	    if(!aes.hasOwnProperty("group")){
		kv = [{"key":0,"value":0}];
		data = {0:data};
	    }else{
		// we need to use a path for each group.
		var kv = d3.entries(d3.keys(data));
		kv = kv.map(function(d){
		    d[aes.group] = d.value;
		    return d;
		});
	    }
	    var lineThing = d3.svg.line()
		.x(toXY("x","x"))
		.y(toXY("y","y"))
	    ;
	    elements = elements.data(kv);
	    eActions = function(e){
		e.attr("d",function(d){
		    var one_group = data[d.value];
		    return lineThing(one_group);
		})
		    .style("fill","none")
		    .style("stroke-width",size)
		    .style("stroke-dasharray",get_dasharray)
		    .style("stroke",colour)
		;
	    }
	    eAppend = "path";
	}else if(g_info.geom == "text"){
	    elements = elements.data(data);
	    // TODO: how to support vjust? firefox doensn't support
	    // baseline-shift... use paths?
	    // http://commons.oreilly.com/wiki/index.php/SVG_Essentials/Text
	    eActions = function(e){
		e.attr("x",toXY("x","x"))
		    .attr("y",toXY("y","y"))
		    .text(function(d){ return d[aes.label]; })
		    .style("text-anchor",text_anchor)
		;
	    }
	    eAppend = "text";
	}else if(g_info.geom == "point"){
	    elements = elements.data(data);
	    eActions = function(e){
		   e.attr("cx",toXY("x","x"))
		    .attr("cy",toXY("y","y"))
		    .attr("r",size)
        .style("fill",get_fill)
        .style("stroke",get_colour)
		;
	    }
	    eAppend = "circle";
	}else if(g_info.geom == "tallrect"){
      elements = elements.data(data);
	    eActions = function(e){
		e.attr("x",toXY("x","xmin"))
		    .attr("width",function(d){
			return svg.x(d[ aes.xmax ])-svg.x(d[ aes.xmin ]);
		    })
		    .attr("y", svg.y.range()[1])
		    .attr("height", svg.y.range()[0])
		    .style("fill",get_fill)
		;
	    }
	    eAppend = "rect";
	}else if(g_info.geom == "rect"){
      elements = elements.data(data);
	    eActions = function(e){
		   e.attr("x",toXY("x","xmin"))
		    .attr("width",function(d) {return svg.x(d[aes.xmax])-svg.x(d[aes.xmin]);})
		    .attr("y",toXY("y","ymax"))
		    .attr("height",function(d) {return svg.y(d[aes.ymin])-svg.y(d[aes.ymax]);})
		    .style("stroke-dasharray",get_dasharray)
		    .style("stroke-width",size)
		    .style("stroke",colour)
		;
	    }
	    eAppend = "rect";
	}else if(g_info.geom == "vline"){
	    elements = elements.data(data);
	    eActions = function(e){
		e.attr("x1",toXY("x","xintercept"))
		    .attr("x2",toXY("x","xintercept"))
		    .attr("y1",svg.y.range()[0])
		    .attr("y2",svg.y.range()[1])
		    .style("stroke-dasharray",get_dasharray)
		    .style("stroke-width",size)
		    .style("stroke",colour)
		;
	    }
	    eAppend = "line";
  }else if(g_info.geom == "segment"){
      elements = elements.data(data);
	    eActions = function(e){
		e.attr("x1",toXY("x","x"))
		    .attr("x2",toXY("x","xend"))
		    .attr("y1",toXY("y", "y"))
		    .attr("y2",toXY("y", "yend"))
		    .style("stroke-dasharray",get_dasharray)
		    .style("stroke-width",size)
		    .style("stroke",colour)
		;
	    }
	    eAppend = "line";
  }else if(g_info.geom == "hline"){  
    //pretty much a copy of geom_vline with obvious modifications
      elements = elements.data(data);
	    eActions = function(e){
		e.attr("y1",toXY("y","yintercept"))
		    .attr("y2",toXY("y","yintercept"))
		    .attr("x1",svg.x.range()[0])
		    .attr("x2",svg.x.range()[1])
		    .style("stroke-dasharray",get_dasharray)
		    .style("stroke-width",size)
		    .style("stroke",colour)
		;
	    }
	    eAppend = "line";
	}else if(g_info.geom == "segment"){
	    elements = elements.data(data);
	    eAppend = "line";
	    eActions = function(e){
		e.attr("x1",toXY("x","x"))
		    .attr("y1",toXY("y","y"))
		    .attr("y2",toXY("y","yend"))
		    .attr("x2",toXY("x","xend"))
		    .style("stroke-width",size)
		    .style("stroke-dasharray",get_dasharray)
		    .style("stroke",colour)
		;
	    }
	}else{
	    return "unsupported geom "+g_info.geom;
	}
	elements.exit().remove();
	var enter = elements.enter().insert(eAppend, "."+g_info.nextgeom);
	enter.classed(g_info.classed, 1);
	if(g_info.aes.hasOwnProperty("clickSelects")){
	    var notOver = function(d){
		return selectedOpacity(d, g_info.aes.clickSelects, 
				       base_opacity, base_opacity-1/2);
	    }
	    //elements.style("opacity",notOver);
	    elements.style("opacity",notOver)
		.on("mouseover",function(d){
		    d3.select(this).style("opacity",function(d){
			return selectedOpacity(d, g_info.aes.clickSelects,
					       base_opacity, base_opacity);
		    });
		})
		.on("mouseout",function(d){
		    d3.select(this).style("opacity",notOver);
		})
		.on("click",function(d){
		    var v_name = g_info.aes.clickSelects;
		    update_selector(v_name, d[v_name]);
		})
		.text("")
		.append("svg:title")
		.text(function(d){
		    var v_name = g_info.aes.clickSelects;
		    return v_name+" "+d[v_name];
		});
	}else{
	    enter.style("opacity",base_opacity);
	}
	eActions(enter);
	if(g_info.duration){
	    elements = elements.transition().duration(g_info.duration);
	}
	eActions(elements);
    }
    var update_selector = function(v_name, value){
	Selectors[v_name].selected = value;
	Selectors[v_name].subset.forEach(update_geom);
	//Selectors[v_name].hilite.forEach(update_geom);
    }
    var selectedOpacity = function(d, v_name, selected, others){
	if(d[v_name] == Selectors[v_name].selected){
	    return selected;
	}else{
	    return others;
	}
    }
    var animateIfInactive = function(){
	var v_name = Animation.variable;
	var cur = Selectors[v_name].selected;
	var next = Animation.next[cur];
	update_selector(v_name, next);
	d3.timer(animateIfInactive, Animation.ms);
    }
    
    // Download the main description of the interactive plot.
    d3.json(json_file,function(error, response){
	// Add plots.
	for(var p_name in response.plots){
	    add_plot(p_name, response.plots[p_name]);
	}
	// Add selectors.
	for(var s_name in response.selectors){
	    add_selector(s_name, response.selectors[s_name]);
	}
	// Add geoms and construct nest operators.
	for(var g_name in response.geoms){
	    add_geom(g_name, response.geoms[g_name]);
	}
	// Start timer if necessary.
	if(response.time){
 	    Animation.next = {};
	    Animation.ms = response.time.ms;
	    Animation.variable = response.time.variable;
	    var i, prev, cur, seq = response.time.sequence;
	    for(i=0; i < seq.length; i++){
		if(i==0){
		    prev = seq.length;
		}else{
		    prev = seq[i-1];
		}
		cur = seq[i];
		Animation.next[prev] = cur;
	    }
	    d3.timer(animateIfInactive, Animation.ms);
	}
    });
}
