const path = require('path');
const { merge } = require('webpack-merge');
const TerserPlugin = require('terser-webpack-plugin');
const OptimizeCSSAssetsPlugin = require('optimize-css-assets-webpack-plugin');
const common = require('./webpack.common.js');

module.exports = merge(common, {
  mode: 'production',
  optimization: {
    minimizer: [
      new TerserPlugin({
        extractComments: false
      }),
      new OptimizeCSSAssetsPlugin({})]
  },

  entry: {
    view: './src/mita/components/entries/view.jsx',
    home: './src/mita/components/entries/home.jsx',
    tags: './src/mita/components/entries/tags.jsx',
    dir: './src/mita/components/entries/dir.jsx',
    album: './src/mita/components/entries/album.jsx',
    albums: './src/mita/components/entries/albums.jsx'
  },

  output: {
    path: path.resolve(__dirname, '../backend/mita/static/gen'),
    filename: '[name].bundle.js'
  },
    
  performance: {
    // Suppress warnings in size limits
    maxEntrypointSize: 2048000,
    maxAssetSize: 2048000
  }
});
