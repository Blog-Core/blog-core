var fs = require('fs');
var api = require('../api');
var path = require('../path');
var message = require('../message');

// Component for displaying a directory.

function page(params) {

    var file = params.file;

    var model = {

        file: path.decode(file),

        parent: path.parent(file),

        size: ko.observable(),

        date: ko.observable(0)
    };

    model.remove = function() {

        if (confirm('Remove file ' + path.decode(file) + '?')) {

            api.removeFile(file).then(function() {

                message.info('File "' + path.decode(file) + '" removed.');

                route.go('directory/' + model.parent);

            }).catch(message.error);
        }
    };

    api.file(file).then(function(data) {

        model.size(data.size);

        model.date(data.modified);

    }).catch(message.error);

    return model;
}

ko.components.register('file', {

    viewModel: { createViewModel: page },

    template: fs.readFileSync(__dirname + '/file.html', { encoding: 'utf8' })
});
