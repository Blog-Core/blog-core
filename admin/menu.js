var model = {

    visible: ko.observable(false),
    active: ko.observable()
};

exports.active = function(name) {

    if (name) {

        model.active(name);
        model.visible(true);

    } else {

        model.active();
        model.visible(false);
    }
};

var menu = document.getElementById('menu');

ko.applyBindings(model, menu);
