var Q = require('./lib/q');

// From https://gist.github.com/matthewp/3099268

module.exports = function(options) {

    var deferred = Q.defer();

    var req = new XMLHttpRequest();

    req.open(options.method || 'GET', options.url, true);

    Object.keys(options.headers || {}).forEach(function (key) {

        req.setRequestHeader(key, options.headers[key]);
    });

    req.onreadystatechange = function(e) {

        if (req.readyState !== 4) {

            return;
        }

        if (req.status !== 200) {

            deferred.reject(new Error('Server responded with a status of ' + req.status));

        } else {

            deferred.resolve(req.responseText);
        }
    };

    req.send(options.data);

    return deferred.promise;
};
