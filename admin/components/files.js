var fs = require('fs');
var api = require('../api');
var message = require('../message');
var files_item = require('../vm/files_item');

// Component for displaying file listing in entry views.

function page(params) {

    var directory = params.directory;

    var model = {

        entry_id: params.id,

        files: ko.observableArray([]),

        progress: ko.observable(0)
    };

    // Submits the upload form and
    // performs the file upload process.

    model.upload = function() {

        var file = document.getElementById('entry-file').files[0];

        if (!file) {

            return;
        }

        model.file = file;

        var xhr = new XMLHttpRequest();

        xhr.upload.addEventListener('progress', model.progress, false);

        xhr.addEventListener('load', model.complete, false);
        xhr.addEventListener('error', model.failed, false);
        xhr.addEventListener('abort', model.aborted, false);

        xhr.open('POST', '/api/upload/' + encodeURIComponent(model.entry_id()));

        xhr.setRequestHeader('X-Key', api.apiKey());
        xhr.setRequestHeader('X-File-Name', file.name);
        xhr.setRequestHeader('Content-Type', 'application/octet-stream');

        xhr.send(file);
    };

    // Reports the upload progress.
    // FIXME only works on Firefox?

    model.progress = function(e) {

        if (e.lengthComputable) {

            model.progress(Math.round(e.loaded * 100 / e.total));
        }
    };

    // Executed then the upload process
    // is complete.

    model.complete = function(e) {

        var res = JSON.parse(e.target.responseText);

        if (res.status === 'success') {

            message.info('File "' + model.file.name +
                '" has been uploaded.');

            model.files.push(files_item.create(model.entry_id(), { name: model.file.name }));

            // This resets the file input.

            var wrap = document.getElementById('entry-file-wrap');

            wrap.innerHTML = '';
            wrap.innerHTML = '<input type="file" id="entry-file" class="form-control" placeholder="Your file">';

        } else {

            // FIXME show error in form.

            message.error(res.message);
        }
    };

    model.upload.failed = function() {

        // FIXME needs error checking.
    };

    // Executed when the upload process
    // was aborted.

    model.aborted = function() {

        // FIXME incomplete
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

                return files_item.create(model.entry_id(), file);

            }));

        }).catch(message.error);
    }

    return model;
}

ko.components.register('files', {

    viewModel: { createViewModel: page },

    template: fs.readFileSync(__dirname + '/files.html', { encoding: 'utf8' })
});
