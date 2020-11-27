/* global d3 */

var barcodeChart = function() {
    'use strict';

    var width = 600,
        height = 30,
        margin = {top: 5, right: 5, bottom: 5, left: 5};

    var value = function(d) { return d.date; };

    var timeInterval = d3.time.day;

    function chart(selection) {
        selection.each(function(data) {

            var div = d3.select(this),
                svg = div.selectAll('svg').data([data]);

            svg.enter().append('svg')
                .call(chart.svgInit);

            var g = svg.select('g.chart-content'),
                lines = g.selectAll('line');

            var lastDate = d3.max(data, value);

            if (!lines.empty()) {
                lastDate = d3.max(lines.data(), value);
            }

            var xScale = d3.time.scale()
                .domain([timeInterval.offset(lastDate, -1), lastDate])
                .range([0, width - margin.left - margin.right]);

            var bars = g.selectAll('line').data(data, value);

            bars.enter().append('line')
                .attr('x1', function(d) { return xScale(value(d)); })
                .attr('x2', function(d) { return xScale(value(d)); })
                .attr('y1', 0)
                .attr('y2', height - margin.top - margin.bottom)
                .attr('stroke', '#000')
                .attr('stroke-opacity', 0.5);

            lastDate = d3.max(data, value);
            xScale.domain([timeInterval.offset(lastDate, -1), lastDate]);

            bars.transition()
                .duration(300)
                .attr('x1', function(d) { return xScale(value(d)); })
                .attr('x2', function(d) { return xScale(value(d)); });

            bars.exit().transition()
                .duration(300)
                .attr('stroke-opacity', 0)
                .remove();
        });
    }

    chart.svgInit = function(svg) {

        svg
            .attr('width', width)
            .attr('height', height);

        var g = svg.append('g')
            .attr('class', 'chart-content')
            .attr('transform', 'translate(' + [margin.top, margin.left] + ')');

        g.append('rect')
            .attr('width', width - margin.left - margin.right)
            .attr('height', height - margin.top - margin.bottom)
            .attr('fill', 'white');
    };

    chart.width = function(value) {
        if (!arguments.length) { return width; }
        width = value;
        return chart;
    };

    chart.height = function(value) {
        if (!arguments.length) { return height; }
        height = value;
        return chart;
    };

    chart.margin = function(value) {
        if (!arguments.length) { return margin; }
        margin = value;
        return chart;
    };

    chart.value = function(accessorFunction) {
        if (!arguments.length) { return value; }
        value = accessorFunction;
        return chart;
    };

    chart.timeInterval = function(value) {
        if (!arguments.length) { return timeInterval; }
        timeInterval = value;
        return chart;
    };

    return chart;
};