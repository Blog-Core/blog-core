var xhr = require('./xhr');

// FIXME make all API calls use it.

function jsendAuth(options) {

    options.headers = options.headers || {};

    options.headers['X-Key'] = apiKey();

    return jsend(options);
}

function jsend(options) {

    return xhr(options).then(function(response) {

        var json = JSON.parse(response);

        if (json.status === 'success') {

            return json.data;

        } else {

            throw new Error(json.message);
        }
    });
}

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

        url: '/api/entry/' + encodeURIComponent(id),

        headers: { 'X-Key': apiKey() }
    };

    return xhr(options).then(function(response) {

        return JSON.parse(response).data;
    });
};

exports.entryInfo = function(id) {

    var options = {

        url: '/api/entry/' + encodeURIComponent(id) + '/info',

        headers: { 'X-Key': apiKey() }
    };

    return xhr(options).then(function(response) {

        return JSON.parse(response).data;
    });
};

// Retrieves the given post comments.

exports.comments = function(id) {

    var options = {

        url: '/api/post/' + encodeURIComponent(id) + '/comments',

        headers: { 'X-Key': apiKey() }
    };

    return xhr(options).then(function(response) {

        return JSON.parse(response).data;
    });
};

// Removes the given comment.

exports.removeComment = function(id) {

    return jsendAuth({

        method: 'DELETE',

        url: '/api/comment/' + encodeURIComponent(id)
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

// Removes the given post.

exports.removePost = function(id) {

    var options = {

        method: 'DELETE',

        url: '/api/entry/' + encodeURIComponent(id),

        headers: { 'X-Key': apiKey() }
    };

    return xhr(options).then(function(response) {

        return JSON.parse(response);
    });
};

exports.directory = function(directory) {

    var options = {

        url: '/api/directory/' + encodeURIComponent(directory),

        headers: { 'X-Key': apiKey() }
    };

    return xhr(options).then(function(response) {

        return JSON.parse(response).data;
    });
};

// Creates the subdirectory.
// Assumes that directory is hex-encoded
// and subdirectory is not.

exports.createDirectory = function(directory, subdirectory) {

    return jsendAuth({

        method: 'POST',

        url: '/api/directory/' + encodeURIComponent(directory) + '/' +
            encodeURIComponent(subdirectory)
    });
};

// Removes the directory.
// Assumes that file path is hex-encoded.

exports.removeDirectory = function(directory) {

    return jsendAuth({

        method: 'DELETE',

        url: '/api/directory/' + encodeURIComponent(directory)
    });
};

// Retrieves file metainfo.
// Assumes that file path is hex-encoded.

exports.file = function(file) {

    return jsendAuth({

        url: '/api/file/' + encodeURIComponent(file)
    });
};

// Removes the given file.
// Assumes that file path is hex-encoded.

exports.removeFile = function(file) {

    return jsendAuth({

        method: 'DELETE',

        url: '/api/file/' + encodeURIComponent(file)
    });
};

// Retrieves all users.

exports.users = function() {

    var options = {

        url: '/api/users',

        headers: { 'X-Key': apiKey() }
    };

    return xhr(options).then(function(response) {

        return JSON.parse(response).data;
    });
};

// Retrieves the given user.

exports.user = function(id) {

    var options = {

        url: '/api/user/' + encodeURIComponent(id),

        headers: { 'X-Key': apiKey() }
    };

    return xhr(options).then(function(response) {

        return JSON.parse(response).data;
    });
};

// Retrieves the current user info.

exports.userInfo = function() {

    return jsendAuth({

        url: '/api/user/info'
    });
};

// Updates the given user.

exports.updateUser = function(id, data) {

    var options = {

        method: 'PUT',

        data: JSON.stringify(data),

        url: '/api/user/' + encodeURIComponent(id),

        headers: { 'X-Key': apiKey(), 'Content-Type': 'application/json' }
    };

    return xhr(options).then(function(response) {

        return JSON.parse(response);
    });
};

// Saves the new user.

exports.saveUser = function(data) {

    var options = {

        method: 'POST',

        data: JSON.stringify(data),

        url: '/api/user',

        headers: { 'X-Key': apiKey(), 'Content-Type': 'application/json' }
    };

    return xhr(options).then(function(response) {

        return JSON.parse(response);
    });
};

// Removes the given user.

exports.removeUser = function(id) {

    var options = {

        method: 'DELETE',

        url: '/api/user/' + encodeURIComponent(id),

        headers: { 'X-Key': apiKey() }
    };

    return xhr(options).then(function(response) {

        return JSON.parse(response);
    });
};

// Finds entry types.

exports.types = function() {

    return jsendAuth({

        url: '/api/types',
    });
};

// Finds user roles.

exports.roles = function() {

    return jsendAuth({

        url: '/api/roles',
    });
};

// Checks whether the API key has been set.

exports.hasKey = function() {

    return !!sessionStorage.getItem('api-key');
};

var apiKey = exports.apiKey = function() {

    var key = sessionStorage.getItem('api-key');

    if (!key) {

        throw new Error('API key is not set');
    }

    return key;
};
