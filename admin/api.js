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

        url: typeof type === 'undefined' ?
            '/api/posts' : '/api/posts/' + type,

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

        url: '/api/post/' + id,

        headers: { 'X-Key': apiKey() }
    };

    return xhr(options).then(function(response) {

        return JSON.parse(response).data;
    });
};

exports.updatePost = function(id, data) {

    var options = {

        method: 'PUT',

        url: '/api/post/' + id,

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

        url: '/api/post',

        data: JSON.stringify(data),

        headers: { 'X-Key': apiKey(), 'Content-Type': 'application/json' }
    };

    return xhr(options).then(function(response) {

        return JSON.parse(response);
    });
};

function apiKey() {

    var key = sessionStorage.getItem('api-key');

    if (!key) {

        throw new Error('API key is not set');
    }

    return key;
}
