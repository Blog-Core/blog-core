var xhr = require('./xhr');

function jsend_auth(options) {

    options.headers = options.headers || {};

    options.headers['X-Key'] = apiKey();

    return jsend(options);
}

// XHR with jsend response.

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

// Login with credentials.

exports.login = function(username, password) {

    return jsend({

        method: 'POST',

        url: '/api/auth',

        data: JSON.stringify({ username: username, password: password }),

        headers: { 'Content-Type': 'application/json' }
    });
};

// List of entries.

exports.posts = function(type) {

    return jsend_auth({ url: '/api/entries/' + encodeURIComponent(type) });
};

// The given entry.

exports.post = function(id) {

    return jsend_auth({ url: '/api/entry/' + encodeURIComponent(id) });
};

// The entry info.

exports.entryInfo = function(id) {

    return jsend_auth({ url: '/api/entry/' + encodeURIComponent(id) + '/info' });
};

// Retrieves the given entry comments.

exports.comments = function(id) {

    return jsend_auth({ url: '/api/post/' + encodeURIComponent(id) + '/comments' });
};

// Removes the given comment.

exports.removeComment = function(entryId, id) {

    return jsend_auth({

        method: 'DELETE',

        url: '/api/comment/' + encodeURIComponent(entryId) +
            '/' + encodeURIComponent(id)
    });
};

// Updates the given post.

exports.updatePost = function(id, data) {

    return jsend_auth({

        method: 'PUT',

        url: '/api/entry/' + encodeURIComponent(id),

        data: JSON.stringify(data),

        headers: { 'Content-Type': 'application/json' }
    });
};

// Saves the given post.

exports.savePost = function(data) {

    return jsend_auth({

        method: 'POST',

        url: '/api/entry',

        data: JSON.stringify(data),

        headers: { 'Content-Type': 'application/json' }
    });
};

// Removes the given post.

exports.removePost = function(id) {

    return jsend_auth({

        method: 'DELETE',

        url: '/api/entry/' + encodeURIComponent(id)
    });
};

// Finds entry files.

exports.files = function(entryId) {

    return jsend_auth({ url: '/api/files/' + encodeURIComponent(entryId) });
};

// Removes the given entry file.

exports.removeFile = function(entryId, file) {

    return jsend_auth({

        method: 'DELETE',

        url: '/api/file/' + encodeURIComponent(entryId) +
            '/' + encodeURIComponent(file)
    });
};

// Retrieves all users.

exports.users = function() {

    return jsend_auth({ url: '/api/users' });
};

// Retrieves the given user.

exports.user = function(id) {

    return jsend_auth({ url: '/api/user/' + encodeURIComponent(id) });
};

// Retrieves the current user info.

exports.userInfo = function() {

    return jsend_auth({ url: '/api/user/info' });
};

// Updates the given user.

exports.updateUser = function(id, data) {

    return jsend_auth({

        method: 'PUT',

        data: JSON.stringify(data),

        url: '/api/user/' + encodeURIComponent(id),

        headers: { 'Content-Type': 'application/json' }
    });
};

// Saves the new user.

exports.saveUser = function(data) {

    return jsend_auth({

        method: 'POST',

        data: JSON.stringify(data),

        url: '/api/user',

        headers: { 'Content-Type': 'application/json' }
    });
};

// Removes the given user.

exports.removeUser = function(id) {

    return jsend_auth({

        method: 'DELETE',

        url: '/api/user/' + encodeURIComponent(id)
    });
};

// Finds entry types.

exports.types = function() {

    return jsend_auth({ url: '/api/types' });
};

// Finds entry type info.

exports.typeInfo = function(type) {

    return jsend_auth({ url: '/api/type/' + encodeURIComponent(type) });
};

// Finds user roles.

exports.roles = function() {

    return jsend_auth({ url: '/api/roles' });
};

// Checks whether the API key has been set.

exports.hasKey = function() {

    return !!(sessionStorage.getItem('api-key') ||
        localStorage.getItem('api-key'));
};

var apiKey = exports.apiKey = function() {

    var key = sessionStorage.getItem('api-key') ||
        localStorage.getItem('api-key');

    if (!key) {

        throw new Error('API key is not set');
    }

    return key;
};
