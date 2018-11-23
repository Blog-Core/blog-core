var api = require('../api');
var assign = require('../assign');
var message = require('../message');
var expandable = require('./expandable');

// Creates trash item view model.

exports.create = function(data) {

    var item = {};

    // Make it expandable.
    expandable.mixin(item);

    // Copies data attributes.
    assign(item, data);

    // Removes from trash.
    item.remove = function() {
        if (confirm('Remove the entry from trash?')) {
            api.removeFromTrash(item.$id).then(function() {
                message.info('Item "' + item.title + '" has been removed.');
                route.refresh();
            }).catch(message.error);
        }
    };

    // Restores the item.
    item.restore = function() {
        api.restore(item.$id).then(function() {
            message.info('Item "' + item.title + '" has been restored.');
            route.refresh();
        }).catch(message.error);
    };

    return item;
};
