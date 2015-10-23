var fs = require('fs');
var api = require('../api');
var view = require('../view');
var message = require('../message');
var trash_item = require('../vm/trash_item');

var template = fs.readFileSync(__dirname + '/trash.html', { encoding: 'utf8' });

// Page for displaying the trash list.

exports.create = function() {

    var model = {

        items: ko.observableArray([])
    };

    model.purge = function() {

        if (confirm('Purge trash?')) {

            api.purge().then(function() {

                message.info('Trash is purged.');

                route.refresh();

            }).catch(message.error);
        }
    };

    return api.trash().then(function(items) {

        model.items(items.map(function(data) {

            return trash_item.create(data);
        }));

        view.show(template, model);
    });
};
