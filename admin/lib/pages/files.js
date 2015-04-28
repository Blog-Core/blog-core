var fs = require('fs');
var api = require('../api');
var message = require('../message');
var files_item = require('../vm/files_item');

// Component for displaying file listing in entry views.

function page(params) {

    var directory = params.directory;

    var model = {

        entry_id: params.id,

        entry_slug: params.slug,

        files: ko.observableArray([]),

        progress: ko.observable(0),

        slug_changed: params.slug_changed
    };

    // Submits the upload form and
    // performs the file upload process.

    model.upload = function() {

        var file = document.getElementById('entry-file').files[0];

        if (!file) {

            return;
        }

        api.upload(model.entry_id(), file).then(function(response) {

            message.info('File "' + file.name + '" has been uploaded.');

            model.files.push(files_item.create(model.entry_slug, { name: file.name }));

            // This resets the file input.

            var wrap = document.getElementById('entry-file-wrap');

            wrap.innerHTML = '';
            wrap.innerHTML = '<input type="file" id="entry-file" class="form-control" placeholder="Your file">';

        }).catch(message.error);
    };

    // Removes the file.
    // Asks confirmation.

    model.remove = function(file) {

        if (confirm('Remove the file "' + file.name + '"?')) {

            api.removeFile(model.entry_id(), file.name).then(function() {

                message.info('File "' + file.name + '" has been removed.');

                model.files.remove(file);

            }).catch(message.error);
        }
    };

    if (model.entry_id()) {

        api.files(model.entry_id()).then(function(files) {

            files.sort(function(left, right) {

                return left.name === right.name ? 0 : (left.name < right.name ? -1 : 1);
            });

            model.files(files.map(function(file) {

                return files_item.create(model.entry_slug, file);

            }));

        }).catch(message.error);
    }

    return model;
}

ko.components.register('files', {

    viewModel: { createViewModel: page },

    template: fs.readFileSync(__dirname + '/files.html', { encoding: 'utf8' })
});
