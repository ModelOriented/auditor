var minVariable = options.xmin, maxVariable = options.xmax,
    minResidual = options.ymin, maxResidual = options.ymax,
    xTitle = options.xTitle, n = options.n,
    yTitle = options.yTitle, chartTitle = options.chartTitle;

var plotHeight, plotWidth,
    margin = {top: 78, right: 30, bottom: 71, left: 60, inner: 42},
    h = height - margin.top - margin.bottom;

if (options.scalePlot === true) {
  plotHeight = h;
  plotWidth = 3*plotHeight/2;
} else {
  plotHeight = 280;
  plotWidth = 420;
}

var modelNames = Object.keys(data[0]);

var colors = getColors(3, "point"),
    pointColor = colors[0], dPointColor = colors[0],
    greyColor = colors[2], dOpacity = 1;

/* --------------------- */

// title
svg.append("text")
  .attr("class", "bigTitle")
  .attr("x", margin.left)
  .attr("y", margin.top - 40)
  .text(chartTitle);

if (n!=1) {

  // add legend before rest of the plot to adjust margin.top
  var tempW = -20+14;
  var tempH = 25;
  var ti = 0;

  var legend = svg.selectAll(".legend")
        .data(modelNames)
        .enter()
        .append("g")
        .attr("class", "legend")
        .attr("transform", function(d, i) {
          let temp = getTextWidth(d, 11, "Fira Sans, sans-serif");
          tempW = tempW + temp + 20;

          let ret;

          if (margin.left + plotWidth - tempW > 0) {
            ret = "translate(" + (margin.left+plotWidth - tempW) +
              "," + (margin.top - tempH) + ")";
          } else {
            tempW = -20+14 + temp + 20;
            tempH = tempH - 20;
            ti++;
            ret = "translate(" + (margin.left+plotWidth - tempW) +
              "," + (margin.top - tempH) + ")";
          }

          return ret;
        });

  legend.append("text")
        .attr("dy", ".6em")
        .attr("class", "legendLabel")
        .text(d => d)
        .attr("x", 14);

  var activeLink = modelNames[1].replace(/\s/g, '');

  legend.append("rect")
          .attr("width", 8)
          .attr("height", 8)
          .attr("class", "legendBox");

  // legend functions
  legend.append("circle")
          .attr("class", "legendDot")
          .attr("cx", 4)
          .attr("cy", 4)
          .attr("r", 2.5)
          .style("stroke-width", 15)
          .style("stroke", "red")
          .style("stroke-opacity", 0)
          .style("fill", greyColor)
          .style("opacity", 0.5)
          .attr("id", function (d, i) {
                    return d.replace(/\s/g, '');
                  })
          .on("mouseover",function(){
                // change cursor
                if (activeLink === this.id){
                  d3.select(this).style("cursor", "auto");
                } else {
                  d3.select(this).style("cursor", "pointer");
                }
          })
          .on("click", function(){

                svg.selectAll("circle.legendDot")
                      .style("fill", greyColor)
                      .style("opacity", 0.5);

                svg.selectAll(".point" + activeLink)
                      .style("fill", greyColor)
                      .style("opacity", 0.5);

                activeLink = this.id;

                d3.select(this)
                    .style("fill", pointColor)
                    .style("opacity", 1);

                svg.selectAll(".point" + activeLink)
                      .style("fill", pointColor)
                      .style("opacity", 1);

                // effort to bring plot to the top
                svg.selectAll("#" + activeLink).raise();
            });

    dPointColor = greyColor;
    dOpacity = 0.5;

    margin.top = margin.top + 20*ti;
}

var x = d3.scaleLinear()
    .range([margin.left + 10, margin.left + plotWidth - 10])
    .domain([minVariable, maxVariable]);

var y = d3.scaleLinear()
    .range([margin.top + plotHeight, margin.top])
    .domain([minResidual, maxResidual]);

svg.append("text")
    .attr("class", "axisTitle")
    .attr("transform", "rotate(-90)")
    .attr("y", margin.left - 45)
    .attr("x", -(margin.top + plotHeight/2))
    .attr("text-anchor", "middle")
    .text(yTitle);

svg.append("text")
    .attr("class", "axisTitle")
    .attr("transform",
          "translate(" + (margin.left + plotWidth/2) + " ," + (margin.top + plotHeight + 50) + ")")
    .attr("text-anchor", "middle")
    .text(xTitle);

var tickValues = getTickValues(x.domain());

// axis and grid
var xAxis = d3.axisBottom(x)
          .tickValues(tickValues)
          .tickSizeInner(0)
          .tickPadding(15);

xAxis = svg.append("g")
          .attr("class", "axisLabel")
          .attr("transform", "translate(0,"+ (margin.top + plotHeight) + ")")
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
        .tickSize(0);

yAxis = svg.append("g")
        .attr("class", "axisLabel")
        .attr("transform","translate(" + (margin.left-8) + ",0)")
        .call(yAxis)
        .call(g => g.select(".domain").remove());

plot(data);

if (n!=1) {
    svg.select("g.legend").select("circle.legendDot").dispatch("click");
}

svg.selectAll("text")
  .style('font-family', 'Fira Sans, sans-serif');

// plot function
function plot(data){
  var pointData = data[0];
  for (var i=0; i<n; i++){
    var modelName = modelNames[i];
    singlePlot(modelName, pointData[modelName], i+1);
  }
}

function singlePlot(modelName, pData, i) {

    if (n == 1) {
      svg.append("text")
            .attr("x", margin.left)
            .attr("y", margin.top - 15)
            .attr("class","smallTitle")
            .text(modelName);
    }

    let tModelName = modelName.replace(/\s/g, '');

    svg.selectAll()
        .data(pData)
        .enter()
        .append("circle")
        .attr("class", "point" + tModelName)
        .attr("id", tModelName)
        .attr("cx", d => x(d.x))
        .attr("cy", d => y(d.y))
        .attr("r", 1.5)
        .style("fill", dPointColor)
        .style("opacity", dOpacity);

    svg.selectAll()
        .data(pData.filter(d => d.big === true))
        .enter()
        .append("text")
        .attr("class", "point" + tModelName)
        .attr("id", tModelName)
        .text(d => d.x)
        .attr("x", d => x(d.x))
        .attr("y", d => y(d.y)-2)
        .attr("text-anchor", "middle")
        .style("font-weight", 400)
        .style("font-size", "11px")
        .style("text-align", "left");
}
