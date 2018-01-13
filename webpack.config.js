const path = require('path');
const webpack = require('webpack');

module.exports = {
  entry: './src/index.js',
  output: {
    path: __dirname + '/dist',
    filename: 'index.js'
  },
  resolve: {
    extensions: ['.js', '.elm'],
    modules: [path.join(__dirname, "src"), 'node_modules']
  },
  plugins: [
    new webpack.EnvironmentPlugin(['DROPBOX_APP_KEY'])
  ],
  module: {
    noParse: /\.elm$/,
    rules: [
      {
        test: /\.html$/,
        exclude: /node_modules/,
        loader: 'file-loader?name=[name].[ext]'
        // use: {
        //   loader: 'file-loader',
        //   options: {
        //     name: '[name].[ext]'
        //   }
        // }
      },
      {
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        use: {
          loader: 'elm-webpack-loader',
        }
      }
    ]
  },
  devServer: {
    inline: true,
    stats: 'errors-only'
  }
};
