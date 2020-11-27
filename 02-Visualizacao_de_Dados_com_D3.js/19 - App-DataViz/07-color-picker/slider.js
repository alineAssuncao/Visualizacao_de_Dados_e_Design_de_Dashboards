// Slider function

function sliderControl() {

    var width = 600;

    var domain = [0, 100];

    var onSlide = function(selection) { };

    // Função do Chart
    function chart(selection) {
        selection.each(function(data) {

            // Select de um grupo de container.
            var group = d3.select(this);

            // Adicionando uma linha.
            group.selectAll('line')
                .data([data])
                .enter()
                .append('line')
                .call(chart.initLine);

            // Adicionando um círculo
            var handle = group.selectAll('circle')
                .data([data])
                .enter()
                .append('circle')
                .call(chart.initHandle);

            // Definindo o domínio
            var posScale = d3.scale.linear()
                .domain(domain)
                .range([0, width]);

            function moveHandle(d) {
                // Computando a posição futura do elemento
                var cx = +d3.select(this).attr('cx') + d3.event.dx;

                // Atualizando a posição com um range válido.
                if ((0 < cx) && (cx < width)) {
                    // Computando o novo valor e ajustando os dados
                    d3.select(this)
                        .data([posScale.invert(cx)])
                        .attr('cx', cx)
                        .call(onSlide);
                }
            }

            var drag = d3.behavior.drag()
                .on('drag', moveHandle);

            // Ajustando a posição do elemento.
            handle
                .attr('cx', function(d) { return posScale(d); })
                .call(drag);
        });
    }

    chart.initLine = function(selection) {
        selection
            .attr('x1', 2)
            .attr('x2', width - 4)
            .attr('stroke', '#777')
            .attr('stroke-width', 4)
            .attr('stroke-linecap', 'round');
    };

    chart.initHandle = function(selection) {
        selection
            .attr('cx', function(d) { return width / 2; })
            .attr('r', 6)
            .attr('fill', '#aaa')
            .attr('stroke', '#222')
            .attr('stroke-width', 2);
    };

    
    chart.width = function(value) {
        if (!arguments.length) { return width; }
        width = value;
        return chart;
    };

    chart.domain = function(value) {
        if (!arguments.length) { return domain; }
        domain = value;
        return chart;
    };

    // Slide callback function
    chart.onSlide = function(onSlideFunction) {
        if (!arguments.length) { return onSlide; }
        onSlide = onSlideFunction;
        return chart;
    };

    return chart;
}