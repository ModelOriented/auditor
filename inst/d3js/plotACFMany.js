var minVariable = options.xmin, maxVariable = options.xmax,
    minValue = options.ymin, maxValue = options.ymax,
    n = options.n,
    xTitle = options.xTitle, chartTitle = options.chartTitle;

var plotHeight, plotWidth,
    margin = {top: 78, right: 30, bottom: 50, left: 60, inner: 42},
    h = height - margin.top - margin.bottom,
    plotTop = margin.top;

if (options.scalePlot === true) {
  plotHeight = (h-(n-1)*margin.inner)/n;
  plotWidth = 3*plotHeight/2;
} else {
  plotHeight = 280;
  plotWidth = 420;
}

var modelNames = Object.keys(data[0]);
var dotLineData = data[1];

var colors = getColors(n, "line");

ACF(data);

svg.selectAll("text")
  .style('font-family', 'Fira Sans, sans-serif');

// plot function
function ACF(data){
  var lineData = data[0];
  for (var i=0; i<n; i++){
    var modelName = modelNames[i];
    singlePlot(modelName, lineData, i+1);
  }
}

function singlePlot(modelName, lineData, i) {

  var x = d3.scaleLinear()
        .range([margin.left, margin.left + plotWidth])
        .domain([minVariable, maxVariable]);

  var y = d3.scaleLinear()
        .range([plotTop + plotHeight, plotTop])
        .domain([minValue, maxValue]);

  // add plot things only once
  if (i == 1) {
    svg.append("text")
        .attr("class", "bigTitle")
        .attr("x", margin.left)
        .attr("y", plotTop - 40)
        .text(chartTitle);
  }

  svg.append("text")
      .attr("class","smallTitle")
      .attr("x", margin.left)
      .attr("y", plotTop - 15)
      .text(modelName);

  // axis and grid
  if (i == n) {
    var xAxis = d3.axisBottom(x)
                .ticks(5)
                .tickSize(0);

    xAxis = svg.append("g")
            .attr("class", "axisLabel")
            .attr("transform", "translate(0," + (plotTop + plotHeight) + ")")
            .call(xAxis)
            .call(g => g.select(".domain").remove());
  }

  var yGrid = svg.append("g")
           .attr("class", "grid")
           .attr("transform", "translate(" + margin.left + ",0)")
           .call(d3.axisLeft(y)
                  .ticks(10)
                  .tickSize(-plotWidth)
                  .tickFormat("")
          ).call(g => g.select(".domain").remove());

  var yAxis = d3.axisLeft(y)
          .ticks(5)
          .tickSize(0);

  yAxis = svg.append("g")
          .attr("class", "axisLabel")
          .attr("transform","translate(" + (margin.left-8) + ",0)")
          .call(yAxis)
          .call(g => g.select(".domain").remove());


  var lines = svg.selectAll()
                  .data(lineData[modelName])
                  .enter();

  lines.append("line")
        .attr("x1", d => x(d.lag))
        .attr("y1", d => y(d.ymin))
        .attr("x2", d => x(d.lag))
        .attr("y2", d => y(d.acf))
        .style("stroke", colors[i-1])
        .style("stroke-width", "2px")
        .style("stroke-opacity", "1")
        .style("stroke-linecap", "butt");

  // make dotted line from intercept to prediction
  var dotLine1 = [{"x": x(minVariable), "y": y(dotLineData[0])},
                  {"x": x(maxVariable), "y": y(dotLineData[0])}],
      dotLine2 = [{"x": x(minVariable), "y": y(dotLineData[1])},
                  {"x": x(maxVariable), "y": y(dotLineData[1])}];

  var lineFunction = d3.line()
                       .x(function(d) { return d.x; })
                       .y(function(d) { return d.y; });

  svg.append("path")
        .data([dotLine1])
        .attr("class", "dotLine")
        .attr("d", lineFunction)
        .style("stroke-dasharray", ("1, 2"));

  svg.append("path")
        .data([dotLine2])
        .attr("class", "dotLine")
        .attr("d", lineFunction)
        .style("stroke-dasharray", ("1, 2"));

  if (i == n) {
      svg.append("text")
          .attr("class", "axisTitle")
          .attr("y", (plotTop + plotHeight + margin.bottom - 15))
          .attr("x", (margin.left + plotWidth/2))
          .attr("text-anchor", "middle")
          .text(xTitle);
 	}

  // update plotTop
  plotTop += (margin.inner + plotHeight);
}
