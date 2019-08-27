// Note this only includes basic configuration for development mode.
// For a more comprehensive configuration check:
// https://github.com/fable-compiler/webpack-config-template

var path = require("path");
var publicDir = path.join(__dirname, "./public");

module.exports = {
    mode: "development",
    entry: path.join(__dirname, "./src/App.fsproj"),
    output: {
        path: publicDir,
        filename: "bundle.js",
    },
    devServer: {
        contentBase: publicDir,
        port: 8080,
    },
    module: {
        rules: [{
            test: /\.fs(x|proj)?$/,
            use: "fable-loader"
        }]
    }
}