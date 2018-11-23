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
            api.userInfo().then(function(userInfo) {
                return api.removeUser(user.$id).then(function() {
                    message.info('User "' + user.username + '" has been removed.');
                    if (userInfo.$id === user.$id) {
                        // User removed itself.
                        sessionStorage.removeItem('api-key');
                        localStorage.removeItem('api-key');
                        window.location = '/admin';
                    } else {
                        route.refresh();
                    }
                });
            }).catch(message.error);
        }
    };

    return user;
};
