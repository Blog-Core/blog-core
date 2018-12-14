const path = require('path');
const assert = require('assert');

// Returns configuration for the given bundle.

module.exports = {
    entry: path.join(__dirname, 'analytics', 'readers.js'),
    output: {
        path: path.resolve(__dirname, 'prolog', 'bc', 'public', 'js'),
        filename: `readers.min.js`
    },
    devtool: 'source-map',
    module: {
        rules: [{
            test: /\.js$/,
            use: {
                loader: 'babel-loader',
                options: {
                    presets: [['@babel/preset-env', {
                        'targets': { 'ie': '11' }
                    }]]
                }
            }
        }]
    }
};
