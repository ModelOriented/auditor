var points = options.points, smooth = options.smooth;
var minVariable = options.xmin, maxVariable = options.xmax,
    minResidual = options.ymin, maxResidual = options.ymax;
var variableName = options.variable, n = options.n;
var yTitle = options.yTitle, chartTitle = options.chartTitle;

var plotHeight, plotWidth;
var margin = {top: 98, right: 30, bottom: 71, left: 120, inner: 42};
var labelsMargin = 94;
var w = width - margin.left - margin.right;
var h = height - margin.top - margin.bottom;
var labelsMargin = margin.left - 8;

if (options.scalePlot === true) {
  plotHeight = h;
  plotWidth = 3*plotHeight/2;
} else {
  plotHeight = 280;
  plotWidth = 420;
}

var k;
if (smooth === true) { k = 1; } else { k = 0; }
var modelNames = Object.keys(data[k]);

var colors = getColors(3, "point");
var pointColor = colors[k], dPointColor = colors[k];
var smoothColor = colors[0], dSmoothColor = colors[0];
var greyColor = colors[2], dOpacity = 1;

residual(data);

if (n!=1) {
    svg.select("g.legend").select("circle.legendDot").dispatch("click");
}

svg.selectAll("text")
  .style('font-family', 'Fira Sans, sans-serif');

// plot function
function residual(data){
  var pointData = data[0], smoothData = data[1];
  for (var i=0; i<n; i++){
    var modelName = modelNames[i];
    singlePlot(modelName, pointData[modelName], smoothData[modelName], i+1);
  }
}

function singlePlot(modelName, pData, sData, i) {

    var x = d3.scaleLinear()
          .range([margin.left + 10, margin.left + plotWidth - 10])
          .domain([minVariable, maxVariable]);

    var y = d3.scaleLinear()
          .range([margin.top + plotHeight, margin.top])
          .domain([minResidual, maxResidual]);

    // function to draw smooth lines
    var line = d3.line()
          .x(function(d) { return x(d.val); })
          .y(function(d) { return y(d.smooth); })
          .curve(d3.curveMonotoneX);

    // add plot things only once
    if (i==1) {
      svg.append("text")
          .attr("class", "bigTitle")
          .attr("x", margin.left)
          .attr("y", margin.top - 60)
          .text(chartTitle);

      svg.append("text")
          .attr("class", "axisTitle")
          .attr("transform", "rotate(-90)")
          .attr("y", margin.left - 50)
          .attr("x", -(margin.top + plotHeight/2))
          .attr("text-anchor", "middle")
          .text(yTitle);

      svg.append("text")
          .attr("class", "axisTitle")
          .attr("transform",
                "translate(" + (margin.left + plotWidth/2) + " ," + (margin.top + plotHeight + 50) + ")")
          .attr("text-anchor", "middle")
          .text(variableName);

      // find 5 nice ticks with max and min - do better than d3
      var domain = x.domain();
      var tickValues = d3.ticks(domain[0], domain[1],5);

      switch (tickValues.length){
        case 3:
          tickValues.unshift(domain[0]);
          tickValues.push(domain[1]);
          break;

        case 4:
          if(Math.abs(domain[0] - tickValues[0]) < Math.abs(domain[1] - tickValues[3])){
            tickValues.shift();
            tickValues.unshift(domain[0]);
            tickValues.push(domain[1]);
          } else {
            tickValues.pop();
            tickValues.push(domain[1]);
            tickValues.unshift(domain[0]);
          }
          break;

        case 5:
          tickValues.pop();
          tickValues.shift();
          tickValues.push(domain[1]);
          tickValues.unshift(domain[0]);
          break;

        case 6:
          if(Math.abs(domain[0] - tickValues[0]) < Math.abs(domain[1] - tickValues[3])){
            tickValues.pop();
            tickValues.shift();
            tickValues.shift();
            tickValues.push(domain[1]);
            tickValues.unshift(domain[0]);
          } else {
            tickValues.pop();
            tickValues.pop();
            tickValues.shift();
            tickValues.push(domain[1]);
            tickValues.unshift(domain[0]);
          }
          break;

        case 7:
          tickValues.pop();
          tickValues.pop();
          tickValues.shift();
          tickValues.shift();
          tickValues.push(domain[1]);
          tickValues.unshift(domain[0]);
      }

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
              .attr("transform","translate(" + labelsMargin + ",0)")
              .call(yAxis)
              .call(g => g.select(".domain").remove());

      if (n!=1) {

        var legend = svg.selectAll(".legend")
              .data(modelNames)
              .enter()
              .append("g")
              .attr("class", "legend");

        // add legend
        var textWidth = [];

        legend.append("text")
                .attr("dy", ".6em")
                .attr("class", "legendLabel")
                .text(function(d) { return d;})
                .attr("x", 14)
                .each(function(d,i) {
                    var thisWidth = this.getComputedTextLength();
                    textWidth.push(thisWidth);
                });

        // make nice legend text and boxes
        var maxLength = d3.max(textWidth)+30;
        legend.attr("transform", function(d, i) {
            return "translate(" + (margin.left+plotWidth - maxLength*(n-i)) +
            "," + (margin.top - 20) + ")";
        });

        var activeLink = modelName.replace(/\s/g, '');

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
                      svg.selectAll(".smooth" + activeLink)
                            .style("stroke", greyColor)
                            .style("opacity", 0.5);

                      activeLink = this.id;

                      d3.select(this)
                          .style("fill", pointColor)
                          .style("opacity", 1);

                      svg.selectAll(".point" + activeLink)
                            .style("fill", pointColor)
                            .style("opacity", 1);
                      svg.selectAll(".smooth" + activeLink)
                            .style("stroke", smoothColor)
                            .style("opacity", 1);

                      // effort to bring plot to the top
                      svg.selectAll("#" + activeLink).each(function() {
                           this.parentNode.appendChild(this);
                      });
                  });

          dPointColor = greyColor;
          dSmoothColor = greyColor;
          dOpacity = 0.5;

        } else { // end of n!=1
          svg.append("text")
            .attr("x", margin.left)
            .attr("y", margin.top - 15)
            .attr("class","smallTitle")
            .text(modelName);

        }
    } //end of i==1

    let tModelName = modelName.replace(/\s/g, '');

    // scatter
    if (points === true) {

      svg.selectAll()
        .data(pData)
        .enter()
        .append("circle")
        .attr("class", "point" + tModelName)
        .attr("id", tModelName)
        .attr("cx", d => x(d.val))
        .attr("cy", d => y(d.res))
        .attr("r", 1)
        .style("fill", dPointColor)
        .style("opacity", dOpacity);
    }

    // smooth line
    if (smooth === true) {
      svg.append("path")
        .data([sData])
        .attr("class", "smooth" + tModelName)
        .attr("id", tModelName)
        .attr("d", line)
        .style("fill", "none")
        .style("stroke", dSmoothColor)
        .style("opacity", dOpacity)
        .style("stroke-width", 2);
    }
}
