var spin = require('./spin');

// From https://gist.github.com/matthewp/3099268

var count = 0;

module.exports = function(options) {

    return new Promise(function(resolve, reject) {

        var req = new XMLHttpRequest();

        req.open(options.method || 'GET', options.url, true);

        Object.keys(options.headers || {}).forEach(function (key) {

            req.setRequestHeader(key, options.headers[key]);
        });

        req.onreadystatechange = function(e) {

            if (req.readyState !== 4) {

                return;
            }

            count -= 1;

            if (count === 0) {

                spin.hide();
            }

            if (req.status !== 200) {

                reject(new Error('Server responded with a status of ' + req.status));

            } else {

                resolve(req.responseText);
            }
        };

        if (count === 0) {

            spin.show();
        }

        count += 1;

        req.send(options.data);
    });
};
