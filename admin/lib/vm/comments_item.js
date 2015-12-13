var api = require('../api');
var assign = require('../assign');
var message = require('../message');

// View Model for a comment.

exports.create = function(data, userInfo, entryInfo, typeInfo) {

    var comment = {};

    // Copies data attributes.

    assign(comment, data);

    comment.removable = false;

    if (userInfo.type === 'admin') {

        comment.removable = true;
    }

    if (typeInfo.grants.indexOf('update_any') >= 0) {

        comment.removable = true;
    }

    if (typeInfo.grants.indexOf('update_own') >= 0) {

        if (entryInfo.author === userInfo.$id) {

            comment.removable = true;
        }
    }

    comment.remove = function() {

        if (confirm('Remove the comment?')) {

            api.removeComment(entryInfo.$id, comment.$id).then(function() {

                message.info('The comment has been removed.');

                route.refresh();

            }).catch(message.error);
        }
    };

    return comment;
};
