var hex = require('./hex');

// Helper to work with hex-encoded file paths.

// Decodes the given path into non-hex form.

exports.decode = function(path) {

    return hex.string(path);
};

// Returns hex-encoded parent directory.

exports.parent = function(path) {

    var decoded = hex.string(path);

    if (decoded === '/') {

        return path;
    }

    // Paths must start with slash.

    if (decoded[0] !== '/') {

        throw new Error('Paths must start with a slash.');
    }

    // Drop end slash.

    if (decoded[decoded.lenght - 1] === '/') {

        decoded = decoded.substring(0, decoded.lenght - 1);
    }

    var tokens = decoded.split(/\//);

    // Extract and encode the parent.

    return hex.hex('/' + tokens.slice(1, tokens.length - 1).join('/'));
};

// Joins hex-encoded directory name with
// non-encoded file name.

exports.join = function(directory, name) {

    var decoded = hex.string(directory);

    // Does not add double slash when directory is /.

    return hex.hex((decoded === '/' ? '' : decoded) + '/' + name);
};
