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
  performance: {
    // Suppress warnings in size limits
    maxEntrypointSize: 2048000,
    maxAssetSize: 1024000
  }
});
