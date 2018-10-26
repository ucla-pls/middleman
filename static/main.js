function renderOverview(str) {
    $.getJSON("api/work/per-hour", function ( data ) {
        var ctx = document.getElementById(str);
        console.log(data);

        var jobSummary = data[0];
        var workDone = data[1];

        var total = (jobSummary.waiting + jobSummary.timeout + jobSummary.active );

        var labels = [];
        var points = [];
        var remaining = [];

        var now = new Date(Date.now());

        now.setMinutes(0, 0);

        for (var i = 0; i >= -24; i--) {
            var h = new Date(now.getTime() + (i * 1000 * 3600));
            labels.push(h);
            var x = workDone[i];
            if (x === undefined) {
                x = 0;
            }
            points.push(x);
            remaining.push(total);
            total += x;
        }

        console.log(points)
        console.log(remaining)

        var myChart = new Chart(ctx, {
            type: 'bar',
            data: {
                labels: labels,
                datasets: [ {
                    label: 'remaining',
                    data: remaining,
                    borderColor: "rgba(128,0, 0, 1)",
                    pointBackgroundColor: "rgba(128,0, 0, 1)",
                    backgroundColor: "rgba(128,0,0,0.05)",
                    yAxisID : "y-axis-2",
                    type: "line",
                    cubicInterpolationMode: 'monotone'
                },
                {
                    label: 'work',
                    data: points,
                    backgroundColor: "rgba(0,128, 0, 0.5)",
                    // borderColor: "black"
                    yAxisID : "y-axis-1"
                }]
            },
            options: {
                scales: {
                    xAxes: [{
                        type: 'time',
                        distribution: 'series',
                        time: {
                            minUnit:"hour",
                            displayFormats : { hour : 'HH:00'},
                            stepSize:1
                        },
                        gridLines: {
                            offsetGridLines: true
                        },
                        offset: true,
                    }],
                    yAxes: [{
                        id: "y-axis-1",
                        position: "left",
                        ticks: {
                            beginAtZero: true
                        },
                        gridLines: { drawOnChartArea: false }
                    }, {
                        id: "y-axis-2",
                        position: "right",
                        ticks: {
                            beginAtZero: true
                        },
                        gridLines: { drawOnChartArea: false }
                    }]
                }
            }
        });
    });
}
