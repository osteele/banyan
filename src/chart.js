import Highcharts from 'highcharts'
import addHeatmap from 'highcharts/modules/heatmap';
import addTreemap from 'highcharts/modules/treemap';

addTreemap(Highcharts);
addHeatmap(Highcharts);

export default function chart(title, data) {
    data = data.filter(({ value }) => value > 0);
    data.forEach((item, i) => item.colorValue = i + 1);
    Highcharts.chart('treeMap', {
        title: { text: title },
        colorAxis: {
            minColor: '#FFFFFF',
            maxColor: Highcharts.getOptions().colors[0]
        },
        series: [{
            type: 'treemap',
            layoutAlgorithm: 'squarified',
            animation: false,
            data
        }]

    });
}
