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
    admin: './src/auth/components/entries/admin.jsx',
    login: './src/auth/components/entries/login.jsx',
  },

  output: {
    path: path.resolve(__dirname, '../auth/static/gen'),
    filename: '[name].bundle.js'
  },

  performance: {
    // Suppress warnings in size limits
    maxEntrypointSize: 2048000,
    maxAssetSize: 1024000
  }
});
