const { merge } = require("webpack-merge");
const TerserPlugin = require("terser-webpack-plugin");
const common = require("./webpack.common.js");

module.exports = merge(common, {
  mode: "production",
  optimization: {
    minimizer: [
      "...",
      new TerserPlugin({
        extractComments: false,
      }),
    ],
  },

  performance: {
    // Suppress warnings in size limits
    maxEntrypointSize: 5120000,
    maxAssetSize: 10240000,
  },
});
