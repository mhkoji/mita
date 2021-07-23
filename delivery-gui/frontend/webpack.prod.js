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
    index: './src/components/entries/index.jsx'
  },

  output: {
    path: path.resolve(__dirname, '../gui/static/gen'),
    filename: '[name].bundle.js'
  },
    
  performance: {
    // Suppress warnings in size limits
    maxEntrypointSize: 2048000,
    maxAssetSize: 2048000
  }
});
