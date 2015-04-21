var fs = require('fs');
var api = require('../api');
var view = require('../view');
var users_item = require('../vm/users_item');

var template = fs.readFileSync(__dirname + '/users.html', { encoding: 'utf8' });

// Page for displaying the users list.

exports.create = function() {

    var model = {

        users: ko.observableArray([]),

        permission: ko.observable(false)
    };

    return api.userInfo().then(function(info) {

        if (info.type !== 'admin') {

            view.show(template, model);

        } else {

            return api.users().then(function(users) {

                model.users(users.map(function(data) {

                    var user = users_item.create(data);

                    user.editable = info.type === 'admin' || user.$id === info.$id;

                    return user;

                }));

                model.permission(true);

                view.show(template, model);
            });
        }
    });
};
