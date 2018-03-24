import Highcharts from 'highcharts';
import addHeatmap from 'highcharts/modules/heatmap';
import addTreemap from 'highcharts/modules/treemap';

addTreemap(Highcharts);
addHeatmap(Highcharts);

export default function chart(title, data_, onClick) {
  const data = data_.filter(({ value }) => value > 0);
  // eslint-disable-next-line no-param-reassign
  data.forEach((item, i) => { item.colorValue = i + 1; });
  Highcharts.chart('treeMap', {
    title: { text: title },
    colorAxis: {
      minColor: '#FFFFFF',
      maxColor: Highcharts.getOptions().colors[0],
    },
    series: [{
      type: 'treemap',
      layoutAlgorithm: 'squarified',
      animation: false,
      cursor: 'pointer',
      events: { click: ({ point }) => onClick(point) },
      data,
    }],
  });
}
