var api = require('../api');
var assign = require('../assign');
var message = require('../message');
var expandable = require('./expandable');

// Creates user view model for
// the users list view.

exports.create = function(data) {

    var user = {

        editable: false
    };

    // Make it expandable.

    expandable.mixin(user);

    // Copies data attributes.

    assign(user, data);

    // Removes the user.
    // Asks confirmation.

    user.remove = function() {

        if (confirm('Remove the user?')) {

            api.removeUser(user.$id).then(function(response) {

                if (response.status === 'success') {

                    message.info('User "' + user.username + '" has been removed.');

                    route.refresh();

                } else {

                    message.error(response.message);
                }

            }).catch(message.error);
        }
    };

    return user;
};
