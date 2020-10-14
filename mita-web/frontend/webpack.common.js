const path = require('path');
const MiniCssExtractPlugin = require('mini-css-extract-plugin');

module.exports = {
  plugins: [
    new MiniCssExtractPlugin({
      filename: '[name].bundle.css'
    })
  ],

  entry: {
    view: './src/components/entries/view.jsx',
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
        loaders: [MiniCssExtractPlugin.loader, 'css-loader']
      }
    ]
  },

  resolve: {
    extensions: ['.js', '.jsx']
  }
};
