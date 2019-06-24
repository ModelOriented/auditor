var minValue = options.ymin, maxValue = options.ymax,
    modelNames = options.modelNames, n = options.n,
    xTitle = options.xTitle,
    yTitle = options.yTitle,
    chartTitle = options.chartTitle;

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

var colors = getColors(n, "point"),
    colors2 = ["#4378bf", "#ae2c87"];

// osie, grid, podpisy, tytuły
// dodaj linie dla ideal i random
// dodaj linie dla modeli
// dodaj tooltip dla każdego punktu, tak żeby pokazywał się threshold
