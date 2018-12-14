const path = require('path');
const assert = require('assert');

const publicJsDir = path.join(__dirname, 'prolog', 'bc', 'public', 'js');

// Returns configuration for the given bundle.

module.exports = {
    entry: path.join(__dirname, 'admin', 'admin.js'),
    output: {
        path: publicJsDir,
        filename: `admin.min.js`
    },
    devtool: 'source-map',
    module: {
        rules: [
            {
                test: /\.js$/,
                use: {
                    loader: 'babel-loader',
                    options: {
                        presets: [['@babel/preset-env', {
                            'targets': { 'chrome': '70' }
                        }]]
                    }
                }
            },
            {
                test: /\.html$/,
                use: 'raw-loader'
            }
        ]
    }
};
