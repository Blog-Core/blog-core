var xhr = require('./xhr');
var ko = require('./lib/knockout');

var content = document.getElementById('content');

exports.show = function(name, model) {

    console.log('Loading template ' + name);

    // Load template, turn into DOM, bind with KnockoutJS.

    return xhr({ url: '/admin/tpl/' + name + '.html' }).then(function(response) {

        if (content.children.length === 1) {

            // Something was previously shown,
            // remove it.

            ko.removeNode(content.children[0]);
        }

        var wrap = document.createElement('div');

        wrap.innerHTML = response;

        content.appendChild(wrap);

        ko.applyBindings(model, wrap);
    });
};

window.formatDate = function(ts) {

    return new Date(1000 * ts).toISOString().substring(0, 10);
};
