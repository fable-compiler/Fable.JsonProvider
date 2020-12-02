var path = require("path");
var publicDir = path.join(__dirname, "./public");

module.exports = {
    mode: "development",
    entry: path.join(__dirname, "./src/App.fs.js"),
    output: {
        path: publicDir,
        filename: "bundle.js",
    },
    devServer: {
        contentBase: publicDir,
        port: 8080,
    },
}