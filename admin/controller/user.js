var ko = require('../lib/knockout');
var api = require('../api');
var view = require('../view');
var message = require('../message');
var userVm = require('../vm/user_vm');
var route = require('../lib/router');

exports.list = function() {

    var mytype = sessionStorage.getItem('user-type');
    var myid = sessionStorage.getItem('user-id');

    return api.users().then(function(users) {

        users.forEach(function(user) {

            user.expanded = ko.observable(false);

            user.expand = function() {

                if (user.expanded()) {

                    user.expanded(false);

                } else {

                    user.expanded(true);
                }
            };

            user.remove = function() {

                if (confirm('Remove the user?')) {

                    api.removeUser(user.$id).then(function(response) {

                        if (response.status === 'success') {

                            message.info('The user was removed.');

                            route.refresh();

                        } else {

                            message.error(response.message);
                        }

                    }, message.error).done();
                }
            };

            user.editable = mytype === 'admin' || user.$id === myid;
        });

        var model = { users: users, mytype: mytype };

        return view.show('users', model);

    }, message.error);
};

exports.edit = function(id) {

    return api.user(id).then(function(user) {

        return view.show('user', userVm.create(user));

    }, message.error);
};

exports.create = function() {

    return view.show('user', userVm.create());
};
