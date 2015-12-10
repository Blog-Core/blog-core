var api = require('../api');
var assign = require('../assign');
var message = require('../message');
var expandable = require('../vm/expandable');

// Creates file view model for
// the files list view.

exports.create = function(slug, data) {

    var file = {};

    // Copies data attributes.

    assign(file, data);

    expandable.mixin(file);

    file.url = ko.pureComputed(function() {

        return '/' + encodeURIComponent(slug()) + '/' +
            encodeURIComponent(file.name);
    });

    file.image = !!file.name.match(/\.(gif|jpg|jpeg|tiff|png)$/i);

    return file;
};
