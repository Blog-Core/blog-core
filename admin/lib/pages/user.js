var api = require('../api');
var view = require('../view');
var user = require('../vm/user');
var resolveObject = require('../resolve_object');
var template = require('./user.html');

// Page for editing an user.
exports.create = function(id) {

    var model = {
        user: ko.observable()
    };

    var requests = { roles: api.roles() };
    if (id) {
        requests.user = api.user(id);
    }

    return resolveObject(requests).then(function(data) {
        model.user(user.create(data.roles, data.user));
        view.show(template, model);
        if (id) {
            var fullname = document.querySelector('#user-fullname');
            fullname.focus();
            if (typeof fullname.setSelectionRange === 'function') {
                fullname.setSelectionRange(0, fullname.value.length);
            }
        }
    });
};
