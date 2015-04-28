var spinner = require('./spinner');

var showCount = 0;

function spinnerShow() {

    if (showCount === 0) {

        spinner.show();
    }

    showCount += 1;
}

function spinnerHide() {

    showCount -= 1;

    if (showCount === 0) {

        spinner.hide();
    }
}

function fetchGet(url) {

    spinnerShow();

    return fetch(url, {

        headers: {

            'Accept': 'application/json',
            'X-Key': apiKey()
        }

    }).catch(function(err) {

        spinnerHide();

        throw err;

    }).then(handleResponse);
}

function fetchDelete(url) {

    spinnerShow();

    return fetch(url, {

        method: 'DELETE',

        headers: {

            'Accept': 'application/json',
            'X-Key': apiKey()
        }

    }).catch(function(err) {

        spinnerHide();

        throw err;

    }).then(handleResponse);
}

function fetchPost(url, data, noauth) {

    spinnerShow();

    var headers = {

        'Accept': 'application/json',
        'Content-Type': 'application/json'
    };

    if (!noauth) {

        headers['X-Key'] = apiKey();
    }

    return fetch(url, {

        method: 'POST',

        headers: headers,

        body: JSON.stringify(data)

    }).catch(function(err) {

        spinnerHide();

        throw err;

    }).then(handleResponse);
}

function fetchUpdate(url, data) {

    spinnerShow();

    return fetch(url, {

        method: 'PUT',

        headers: {

            'Accept': 'application/json',
            'Content-Type': 'application/json',
            'X-Key': apiKey()
        },

        body: JSON.stringify(data)

    }).catch(function(err) {

        spinnerHide();

        throw err;

    }).then(handleResponse);
}

function handleResponse(response) {

    spinnerHide();

    return response.json().then(function(json) {

        if (json.status === 'success') {

            return json.data;

        } else {

            throw new Error('API response is not success: ' + json.message);
        }
    });
}

// Login with credentials.

exports.login = function(username, password) {

    return fetchPost('/api/auth', {
        username: username, password: password }, true);
};

// List of entries.

exports.posts = function(type) {

    return fetchGet('/api/entries/' + encodeURIComponent(type));
};

// The given entry.

exports.post = function(id) {

    return fetchGet('/api/entry/' + encodeURIComponent(id));
};

// The entry info.

exports.entryInfo = function(id) {

    return fetchGet('/api/entry/' + encodeURIComponent(id) + '/info');
};

// Retrieves the given entry comments.

exports.comments = function(id) {

    return fetchGet('/api/post/' + encodeURIComponent(id) + '/comments');
};

// Removes the given comment.

exports.removeComment = function(entryId, id) {

    return fetchDelete('/api/comment/' + encodeURIComponent(entryId) +
            '/' + encodeURIComponent(id));
};

// Updates the given post.

exports.updatePost = function(id, data) {

    return fetchUpdate('/api/entry/' + encodeURIComponent(id), data);
};

// Saves the given post.

exports.savePost = function(data) {

    return fetchPost('/api/entry', data);
};

// Removes the given post.

exports.removePost = function(id) {

    return fetchDelete('/api/entry/' + encodeURIComponent(id));
};

// Finds entry files.

exports.files = function(entryId) {

    return fetchGet('/api/files/' + encodeURIComponent(entryId));
};

// Removes the given entry file.

exports.removeFile = function(entryId, file) {

    return fetchDelete('/api/file/' + encodeURIComponent(entryId) +
            '/' + encodeURIComponent(file));
};

// Retrieves all users.

exports.users = function() {

    return fetchGet('/api/users');
};

// Retrieves the given user.

exports.user = function(id) {

    return fetchGet('/api/user/' + encodeURIComponent(id));
};

// Retrieves the current user info.

exports.userInfo = function() {

    return fetchGet('/api/user/info');
};

// Updates the given user.

exports.updateUser = function(id, data) {

    return fetchUpdate('/api/user/' + encodeURIComponent(id), data);
};

// Saves the new user.

exports.saveUser = function(data) {

    return fetchPost('/api/user', data);
};

// Removes the given user.

exports.removeUser = function(id) {

    return fetchDelete('/api/user/' + encodeURIComponent(id));
};

// Finds entry types.

exports.types = function() {

    return fetchGet('/api/types');
};

// Finds entry type info.

exports.typeInfo = function(type) {

    return fetchGet('/api/type/' + encodeURIComponent(type));
};

// Finds user roles.

exports.roles = function() {

    return fetchGet('/api/roles');
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
