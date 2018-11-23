var api = require('../api');
var path = require('../path');
var assign = require('../assign');
var message = require('../message');

// View Model for a directory entry.
exports.create = function(directory, data) {
    var entry = {};
    // Copies data attributes.
    assign(entry, data);
    entry.encoded = path.join(directory, entry.name);
    return entry;
};

// Compares entries so that directories
// come first.
exports.compare = function(entry1, entry2) {
    // Compares directory entries by name.
    function compareName() {
        return entry1.name < entry2.name ? -1 : 1;
    }
    if (entry1.directory) {
        if (entry2.directory) {
            return compareName();
        } else {
            return -1;
        }
    } else {
        if (entry2.directory) {
            return 1;
        } else {
            return compareName();
        }
    }
};
