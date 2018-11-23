var api = require('../api');
var assign = require('../assign');
var message = require('../message');
var expandable = require('./expandable');

// Creates posts view model for
// the users list view.

exports.create = function(data, typeInfo, userInfo) {

    var post = {};

    // Make it expandable.
    expandable.mixin(post);

    // Copies data attributes.
    assign(post, data);

    post.removable = false;

    // Find if the entry can be removed.
    if (userInfo.type === 'admin') {
        post.removable = true;
    }
    if (typeInfo.grants.indexOf('remove_any') >= 0) {
        post.removable = true;
    }
    if (typeInfo.grants.indexOf('remove_own') >= 0) {
        if (userInfo.$id === post.author) {
            post.removable = true;
        }
    }

    post.editable = false;
    if (userInfo.type === 'admin') {
        post.editable = true;
    }

    if (typeInfo.grants.indexOf('update_any') >= 0) {
        post.editable = true;
    }

    if (typeInfo.grants.indexOf('update_own') >= 0) {
        if (userInfo.$id === post.author) {
            post.editable = true;
        }
    }

    post.editLink = '#entry/' + typeInfo.name + '/' + post.$id;
    post.commentsLink = '#comments/' + typeInfo.name + '/' + post.$id;

    // Removes the post.
    // Asks confirmation.
    post.remove = function() {
        if (confirm('Remove the post "' + post.title + '"?')) {
            api.removePost(post.$id).then(function() {
                message.info('The post "' + post.title + '" has been removed.');
                route.refresh();
            }).catch(message.error);
        }
    };

    return post;
};
