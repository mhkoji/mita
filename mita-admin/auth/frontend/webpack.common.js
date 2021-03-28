const path = require('path');
const webpack = require('webpack');
const MiniCssExtractPlugin = require('mini-css-extract-plugin');

module.exports = {
  plugins: [
    new MiniCssExtractPlugin({
      filename: '[name].bundle.css'
    }),
    new webpack.ProvidePlugin({
      $: 'jquery',
      jQuery: 'jquery'
    })
  ],

  entry: {
    login: './src/components/entries/login.jsx',
    admin: './src/components/entries/admin.jsx'
  },

  output: {
    path: path.resolve(__dirname, '../static/gen'),
    filename: '[name].bundle.js'
  },

  module: {
    rules: [
      {
        test: /\.jsx$/,
        include: [
          path.resolve(__dirname, 'src/')
        ],
        loader: 'babel-loader'
      }, {
        test: /\.css$/,
        use: [MiniCssExtractPlugin.loader, 'css-loader']
      }
    ]
  },

  resolve: {
    extensions: ['.js', '.jsx']
  }
};
