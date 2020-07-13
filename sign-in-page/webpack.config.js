const HtmlWebpackPlugin = require('html-webpack-plugin')
const path = require('path')

module.exports = env => ({
  entry: './index.js',
  module: {
    rules: [{
        exclude: /node_modules/,
        loader: 'babel-loader',
        query: {
          presets: ['env']
        },
        test: /\.js$/
      },
      {
        exclude: /node_modules/,
        loader: 'eslint-loader',
        test: /\.js$/
      }
    ]
  },
  output: {
    filename: '[name].[chunkhash].js',
    path: path.resolve(__dirname, 'dist')
  },
  plugins: [
    new HtmlWebpackPlugin({
      minify: {
        collapseWhitespace: true
      },
      title: 'Logga in'
    })
  ],
  resolve: {
    alias: {
      vue: env.NODE_ENV === 'production' ? 'vue/dist/vue.min.js' : 'vue/dist/vue.js'
    }
  }
})
