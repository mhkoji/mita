const path = require("path");
const MiniCssExtractPlugin = require("mini-css-extract-plugin");
const CssMinimizerPlugin = require("css-minimizer-webpack-plugin");
const webpack = require("webpack");

module.exports = {
  plugins: [
    new MiniCssExtractPlugin(),
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
    minimizer: [new CssMinimizerPlugin()],
  },

  entry: {
    folder: "./src/components/entries/folder.jsx",
    view: "./src/components/entries/view.jsx",
    tags: "./src/components/entries/tags.jsx",
  },

  output: {
    path: path.resolve(__dirname, "../backend/static/gen"),
  },
};
