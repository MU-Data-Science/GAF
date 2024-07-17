/* Copyright © 2019  Roel Janssen <roel@gnu.org>
 *
 * This program is free software: you can redistribute it and/or
 * modify it under the terms of the GNU Affero General Public License
 * as published by the Free Software Foundation, either version 3 of
 * the License, or (at your option) any later version.
 */

function plot_data (plotObject, data_str)
{
    var data = JSON.parse(data_str);
    var keys = Object.keys(data[0]);
    var x_key = keys[0];
    var y_key = keys[1];
    var colors =
        [
            "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b",
            "#aec7e8", "#ffbb78", "#98df8a", "#ff9896", "#c5b0d5", "#c49c94",
            "#e377c2", "#7f7f7f", "#bcbd22", "#17becf", "#f7b6d2", "#c7c7c7",
            "#dbdb8d", "#393b79", "#9edae5", "#637939", "#8c6d31", "#843c39",
            "#7b4173", "#5254a3", "#8ca252", "#bd9e39", "#ad494a", "#a55194",
            "#6b6ecf", "#b5cf6b", "#e7ba52", "#d6616b", "#ce6dbd", "#9c9ede",
            "#cedb9c", "#e7cb94", "#e7969c", "#de9ed6"
        ];

    //var colors = d3.scaleLinear().domain([1,40]).range(["white", "blue"])
    var keys_enum = Array.map(data, function (o) { return o[x_key] });

    var margin = {top: 20, right: 20, bottom: 90, left: 40},
        width =  $(plotObject).width() - margin.left - margin.right,
        height = $(plotObject).height() - margin.top - margin.bottom;

    // set the ranges
    var x = d3.scaleBand()
        .range([0, width])
        .padding(0.1);
    var y = d3.scaleLinear()
        .range([height, 0]);

    const svg = d3.select(plotObject)
        .attr("width", width + margin.left + margin.right)
        .attr("height", height + margin.top + margin.bottom)
        .append("g")
        .attr("transform",
              "translate(" + margin.left + "," + margin.top + ")");

    // Scale the range of the data in the domains
    x.domain(data.map(function(d) { return d[x_key]; }));
    y.domain([0, d3.max(data, function(d) { return d[y_key]; })]);

    // append the rectangles for the bar chart
    svg.selectAll(".bar")
        .data(data)
        .enter().append("rect")
        .attr("class", "bar")
        .attr("fill", function(d) {
            return colors[keys_enum.indexOf(d[x_key])]
        })
        .attr("x", function(d) { return x(d[x_key]); })
        .attr("width", x.bandwidth())
        .attr("y", function(d) { return y(d[y_key]); })
        .attr("height", function(d) { return height - y(d[y_key]); });

    // add the x Axis
    svg.append("g")
        .attr("transform", "translate(0," + height + ")")
        .call(d3.axisBottom(x))
        .selectAll("text")
        .attr("y", 0)
        .attr("x", 9)
        .attr("dy", ".35em")
        .attr("transform", "rotate(90)")
        .style("text-anchor", "start");

    // add the y Axis
    svg.append("g")
        .call(d3.axisLeft(y));
}
