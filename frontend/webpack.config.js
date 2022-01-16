const path = require("path");
const webpack = require("webpack");
const TerserPlugin = require("terser-webpack-plugin");
const OptimizeCSSAssetsPlugin = require("optimize-css-assets-webpack-plugin");
const MiniCssExtractPlugin = require("mini-css-extract-plugin");

module.exports = {
  //  mode: "production",
  mode: "development",

  plugins: [
    new MiniCssExtractPlugin({
      filename: "[name].bundle.css",
    }),
    new webpack.ProvidePlugin({
      $: "jquery",
      jQuery: "jquery",
    }),
  ],

  module: {
    rules: [
      {
        test: /\.jsx$/,
        include: [path.resolve(__dirname, "src/")],
        loader: "babel-loader",
      },
      {
        test: /\.css$/,
        use: [MiniCssExtractPlugin.loader, "css-loader"],
      },
    ],
  },

  resolve: {
    extensions: [".js", ".jsx"],
  },

  optimization: {
    minimizer: [
      new TerserPlugin({
        extractComments: false,
      }),
      new OptimizeCSSAssetsPlugin({}),
    ],
  },

  entry: {
    folder: "./src/components/entries/folder.jsx",
    view: "./src/components/entries/view.jsx",
  },

  output: {
    path: path.resolve(__dirname, "../backend/static/gen"),
    filename: "[name].bundle.js",
  },

  performance: {
    // Suppress warnings in size limits
    maxEntrypointSize: 2048000,
    maxAssetSize: 2048000,
  },
};
