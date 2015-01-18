var api = require('../api');
var assign = require('../assign');
var message = require('../message');
var expandable = require('./expandable');

// Creates posts view model for
// the users list view.

exports.create = function(data) {

    var post = {

        editable: false
    };

    // Make it expandable.

    expandable.mixin(post);

    // Copies data attributes.

    assign(post, data);

    // Removes the post.
    // Asks confirmation.

    post.remove = function() {

        if (confirm('Remove the post?')) {

            api.removePost(post.$id).then(function(response) {

                if (response.status === 'success') {

                    message.info('The post was removed.');

                    route.refresh();

                } else {

                    message.error(response.message);
                }

            }).catch(message.error);
        }
    };

    return post;
};
