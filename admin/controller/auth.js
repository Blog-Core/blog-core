var view = require('../view');
var api = require('../api');
var ko = require('../lib/knockout');

exports.form = function() {

    var model = {

        username: ko.observable(),
        password: ko.observable(),

        login: function() {

            api.login(model.username(), model.password()).then(function(res) {

                if (res.status === 'success') {

                    sessionStorage.setItem('api-key', res.data);
                    window.location.hash = '#posts';
                }
            });
        }
    };

    return view.show('login', model);
};
