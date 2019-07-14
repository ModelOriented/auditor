var minValue = options.xmin, maxValue = options.xmax,
    n = options.n,
    xTitle = options.xTitle,
    chartTitle = options.chartTitle;

var ymin = 0, ymax = 1, size = 2;

var plotHeight, plotWidth,
    margin = {top: 98, right: 30, bottom: 71, left: 60, inner: 42},
    h = height - margin.top - margin.bottom;

if (options.scalePlot === true) {
  plotHeight = h;
  plotWidth = 3*plotHeight/2;
} else {
  plotHeight = 280;
  plotWidth = 420;
}

var colors = getColors(n, "line");

// scale
var x = d3.scaleLinear()
          .range([margin.left, margin.left + plotWidth])
          .domain([minValue, maxValue]);

var y = d3.scaleLinear()
      .range([margin.top + plotHeight, margin.top])
      .domain([ymin, ymax]);

// title
svg.append("text")
    .attr("class", "bigTitle")
    .attr("x", margin.left)
    .attr("y", margin.top - 60)
    .text(chartTitle);

svg.append("text")
    .attr("class", "axisTitle")
    .attr("transform",
          "translate(" + (margin.left + plotWidth/2) + " ," + (margin.top + plotHeight + 50) + ")")
    .attr("text-anchor", "middle")
    .text(xTitle);

// axis and grid
var xGrid = svg.append("g")
               .attr("class", "grid")
               .attr("transform", "translate(0,"+ (margin.top + plotHeight) + ")")
               .call(d3.axisBottom(x)
                      .ticks(10)
                      .tickSize(-plotHeight)
                      .tickFormat("")
              ).call(g => g.select(".domain").remove());

var tickValues = getTickValues(x.domain());

var xAxis = d3.axisBottom(x)
          .tickValues(tickValues)
          .tickSizeInner(0)
          .tickPadding(15);

xAxis = svg.append("g")
          .attr("class", "axisLabel")
          .attr("transform", "translate(0,"+ (margin.top + plotHeight + 8) + ")")
          .call(xAxis);

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
          .tickSizeInner(0)
          .tickPadding(15)
          .tickFormat(d3.format(".0%"));

yAxis = svg.append("g")
        .attr("class", "axisLabel")
        .attr("transform","translate(" + (margin.left) + ",0)")
        .call(yAxis)
        .call(g => g.select(".domain").remove());

// line
var line = d3.line()
      .x(function(d) { return x(d.x); })
      .y(function(d) { return y(d.y); })
      .curve(d3.curveCardinal.tension(1));

// make tooltip
var tool_tip = d3.tip()
            .attr("class", "tooltip")
            .offset([-8, 0])
            .html(function(d) {
                return staticTooltipHtml(d);
              });

svg.call(tool_tip);

// function to find nearest point on the line
var bisectXhat = d3.bisector(d => d.x).right;

// tooltip appear with info nearest to mouseover
function appear(data){
  var x0 = x.invert(d3.mouse(d3.event.currentTarget)[0]),
      i = bisectXhat(data, x0),
      d0 = data[i - 1],
      d1 = data[i],
      d = x0 - d0.x > d1.x - x0 ? d1 : d0;

  tool_tip.show(d);
}

// add model lines
var modelData = data[0];
var modelNames = Object.keys(modelData);

modelNames.forEach(function(key, i) {
      svg.append("path")
        .data([modelData[key]])
        .attr("class", "line" + key.replace(/\s/g, ''))
        .attr("d", line)
        .style("fill", "none")
        .style("stroke", colors[i])
        .style("opacity", 1)
        .style("stroke-width", size)
        .on('mouseover', function(d){

          d3.select(this)
            .style("stroke-width", size*1.5);

          // make line appear on top
          this.parentNode.appendChild(this);

          // show changed tooltip
          appear(d);
        })
        .on('mouseout', function(d){

          d3.select(this)
            .style("stroke-width", size);

          // hide changed tooltip
          tool_tip.hide(d);
        });
  });

// add legend
var tempW = -20+14;

var legend = svg.selectAll(".legend")
      .data(modelNames)
      .enter()
      .append("g")
      .attr("class", "legend")
      .attr("transform", function(d, i) {
        let temp = getTextWidth(d, 11, "Fira Sans, sans-serif");
        tempW = tempW + temp + 20;
        return "translate(" + (margin.left+plotWidth - tempW) +
            "," + (margin.top - 20) + ")";
      });

legend.append("text")
      .attr("dy", ".6em")
      .attr("class", "legendLabel")
      .text(function(d) { return d;})
      .attr("x", 14);

legend.append("rect")
        .attr("width", 8)
        .attr("height", 8)
        .attr("class", "legendBox");

legend.append("circle")
        .attr("class", "legendDot")
        .attr("cx", 4)
        .attr("cy", 4)
        .attr("r", 2.5)
        .style("fill", function(d, i) {return colors[i];});

// change font
svg.selectAll("text")
  .style('font-family', 'Fira Sans, sans-serif');


function staticTooltipHtml(d){
  // function formats tooltip text
  var temp = "";
  for (var [k, v] of Object.entries(d)) {
    switch(k) {
      case "x":
        k = "Error tolerance";
        temp += "<center>" +  k + ": " + v + "</br>";
        break;
      case "y":
        k = "Percentage";
        temp += "<center>" +  k + ": " + v + "</br>";
        break;
      case "label":
        temp += "<center>" +  k + ": " + v + "</br>";
        break;
      default:
        break;
    }
  }
  return temp;
}
