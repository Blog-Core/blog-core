var Q = require('./lib/q');
var xhr = require('./xhr');

exports.login = function(username, password) {

    var options = {

        method: 'POST',

        url: '/api/auth',

        data: JSON.stringify({

            username: username,
            password: password
        }),

        headers: {

            'Content-Type': 'application/json'
        }
    };

    return xhr(options).then(function(response) {

        return JSON.parse(response);
    });
};

exports.posts = function(type) {

    var options = {

        url: '/api/entries/' + type,

        headers: {

            'X-Key': apiKey()
        }
    };

    return xhr(options).then(function(response) {

        return JSON.parse(response).data;
    });
};

exports.post = function(id) {

    var options = {

        url: '/api/entry/' + id,

        headers: { 'X-Key': apiKey() }
    };

    return xhr(options).then(function(response) {

        return JSON.parse(response).data;
    });
};

exports.updatePost = function(id, data) {

    var options = {

        method: 'PUT',

        url: '/api/entry/' + id,

        data: JSON.stringify(data),

        headers: { 'X-Key': apiKey(), 'Content-Type': 'application/json' }
    };

    return xhr(options).then(function(response) {

        return JSON.parse(response);
    });
};

exports.savePost = function(data) {

    var options = {

        method: 'POST',

        url: '/api/entry',

        data: JSON.stringify(data),

        headers: { 'X-Key': apiKey(), 'Content-Type': 'application/json' }
    };

    return xhr(options).then(function(response) {

        return JSON.parse(response);
    });
};

exports.directory = function(directory) {

    var options = {

        url: '/api/directory/' + encodeURIComponent(window.btoa(directory)),

        headers: { 'X-Key': apiKey() }
    };

    return xhr(options).then(function(response) {

        return JSON.parse(response).data;
    });
};

exports.createDirectory = function(directory, subdirectory) {

    var options = {

        method: 'POST',

        url: '/api/directory/' + encodeURIComponent(window.btoa(directory)) + '/' +
            encodeURIComponent(subdirectory),

        headers: { 'X-Key': apiKey() }
    };

    return xhr(options).then(function(response) {

        return JSON.parse(response);
    });
};

exports.removeDirectory = function(directory) {

    var options = {

        method: 'DELETE',

        url: '/api/directory/' + encodeURIComponent(window.btoa(directory)),

        headers: { 'X-Key': apiKey() }
    };

    return xhr(options).then(function(response) {

        return JSON.parse(response);
    });
};

// Retrieves file metainfo.

exports.file = function(file) {

    var options = {

        url: '/api/file/' + encodeURIComponent(window.btoa(file)),

        headers: { 'X-Key': apiKey() }
    };

    return xhr(options).then(function(response) {

        return JSON.parse(response);
    });
};

// Removes the given file.

exports.removeFile = function(file) {

    var options = {

        method: 'DELETE',

        url: '/api/file/' + encodeURIComponent(window.btoa(file)),

        headers: { 'X-Key': apiKey() }
    };

    return xhr(options).then(function(response) {

        return JSON.parse(response);
    });
};

var apiKey = exports.apiKey = function() {

    var key = sessionStorage.getItem('api-key');

    if (!key) {

        throw new Error('API key is not set');
    }

    return key;
}
