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
    view: './src/components/entries/view.jsx',
    tags: './src/components/entries/tags.jsx',
    dir: './src/components/entries/dir.jsx',
    page: './src/components/entries/page.jsx',
    pages: './src/components/entries/pages.jsx',
    album: './src/components/entries/album.jsx',
    albums: './src/components/entries/albums.jsx'
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
