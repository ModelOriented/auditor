var xTitle = options.xTitle, n = options.n,
    yTitle = options.yTitle, chartTitle = options.chartTitle;

var plotHeight, plotWidth,
    margin = {top: 78, right: 30, bottom: 71, left: 60, inner: 70},
    h = height - margin.top - margin.bottom,
    plotTop = margin.top, plotLeft = margin.left;

if (options.scalePlot === true) {
  var m = Math.ceil(n/2);
  if (n == 2) { m = 2; }
  plotHeight = (h-(m-1)*margin.inner)/m;
  plotWidth = 3*plotHeight/2;
} else {
  plotHeight = 280;
  plotWidth = 420;
}

var modelNames = Object.keys(data[0]);

var xMinMax = data[1], yMinMax = data[2];

var colors = getColors(3, "point"),
    pointColor = colors[0],
    greyColor = colors[2];

halfNormal(data);

svg.selectAll("text")
  .style('font-family', 'Fira Sans, sans-serif');

// plot function
function halfNormal(data){
  var plotData = data[0];
  for (var i=0; i<n; i++){
    var modelName = modelNames[i];
    singlePlot(modelName, plotData[modelName], i+1);
  }
}

function singlePlot(modelName, plotData, i) {

  var xmin = xMinMax[modelName][0], xmax = xMinMax[modelName][1],
      ymin = yMinMax[modelName][0], ymax = yMinMax[modelName][1];

  var x = d3.scaleLinear()
        .range([plotLeft + 5, plotLeft + plotWidth - 5])
        .domain([xmin, xmax]);

  var y = d3.scaleLinear()
        .range([plotTop + plotHeight, plotTop])
        .domain([ymin, ymax]);

  // add plot things only once
  if (i==1) {
    svg.append("text")
        .attr("class", "bigTitle")
        .attr("x", plotLeft)
        .attr("y", plotTop - 40)
        .text(chartTitle);
  }

  svg.append("text")
      .attr("class","smallTitle")
      .attr("x", plotLeft)
      .attr("y", plotTop - 15)
      .text(modelName);

  var tickValues = getTickValues(x.domain());

  // axis and grid
  var xAxis = d3.axisBottom(x)
            .tickValues(tickValues)
            .tickSizeInner(0)
            .tickPadding(15);

  xAxis = svg.append("g")
            .attr("class", "axisLabel")
            .attr("transform", "translate(0,"+ (plotTop + plotHeight) + ")")
            .call(xAxis);

  var yGrid = svg.append("g")
           .attr("class", "grid")
           .attr("transform", "translate(" + plotLeft + ",0)")
           .call(d3.axisLeft(y)
                  .ticks(10)
                  .tickSize(-plotWidth)
                  .tickFormat("")
          ).call(g => g.select(".domain").remove());

  if (i%2 === 1 || true) { // SHOULD Y AXIS BE ALWAYS DISPLAYED?
    var yAxis = d3.axisLeft(y)
            .ticks(5)
            .tickSize(0);

    yAxis = svg.append("g")
            .attr("class", "axisLabel")
            .attr("transform","translate(" + (plotLeft-8) + ",0)")
            .call(yAxis)
            .call(g => g.select(".domain").remove());
  }

  // scatter
  svg.selectAll()
      .data(plotData)
      .enter()
      .append("circle")
      .attr("class", "dot " + modelName)
      .attr("id", modelName)
      .attr("cx", d => x(d.x))
      .attr("cy", d => y(d.residuals))
      .attr("r", 1.5)
      .style("fill", pointColor);

  // add upper, lower, median lines
  var lineU = d3.line()
      .x(function(d) { return x(d.x); })
      .y(function(d) { return y(d.upper); })
      .curve(d3.curveCardinal.tension(1));

  svg.append("path")
        .data([plotData])
        .attr("d", lineU)
        .style("fill", "none")
        .style("stroke", "160e3b")
        .style("opacity", 1)
        .style("stroke-width", 2);

  var lineL = d3.line()
      .x(function(d) { return x(d.x); })
      .y(function(d) { return y(d.lower); })
      .curve(d3.curveCardinal.tension(1));

  svg.append("path")
        .data([plotData])
        .attr("d", lineL)
        .style("fill", "none")
        .style("stroke", "160e3b")
        .style("opacity", 1)
        .style("stroke-width", 2);


  var lineM = d3.line()
               .x(function(d) { return x(d.x); })
               .y(function(d) { return y(d.median); });

  svg.append("path")
        .data([plotData])
        .attr("d", lineM)
        .style("stroke", "ceced9")
        .style("fill", "none")
        .style("stroke-width", 2)
        .style("stroke-dasharray", ("3, 2"));

  if (i==n) {
  	svg.append("text")
        .attr("class", "axisTitle")
        .attr("transform", "rotate(-90)")
        .attr("y", margin.left - 45)
        .attr("x", -(margin.top + plotTop + plotHeight)/2)
        .attr("text-anchor", "middle")
        .text(yTitle);

                                                // SHOULD Y AXIS BE ALWAYS DISPLAYED?
    let temp = n==1 ? margin.left + plotWidth/2 : margin.left + plotWidth + (25+15)/2;

    svg.append("text")
        .attr("class", "axisTitle")
        .attr("y", (plotTop + plotHeight + margin.bottom - 15))
        .attr("x", temp)
        .attr("text-anchor", "middle")
        .text(xTitle);
  }

  if (i%2 === 1){
    plotLeft += (25 + 15 + plotWidth); // SHOULD Y AXIS BE ALWAYS DISPLAYED?
  }
  if (i%2 === 0){
    plotLeft -= (25 + plotWidth);
    plotTop += (margin.inner + plotHeight);
  }
}
