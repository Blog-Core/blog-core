var api = require('../api');
var assign = require('../assign');
var message = require('../message');
var expandable = require('./expandable');

// View Model for a comment.

exports.create = function(data) {

    var comment = {};

    // Make it expandable.

    expandable.mixin(comment);

    // Copies data attributes.

    assign(comment, data);

    comment.remove = function() {

        if (confirm('Remove the comment?')) {

            api.removeComment(comment.$id).then(function() {

                route.refresh();

            }).catch(message.error);
        }
    };

    return comment;
};
