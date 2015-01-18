var fs = require('fs');
var api = require('../api');
var path = require('../path');
var message = require('../message');
var directory_item = require('../vm/directory_item');

// Component for displaying a directory.

function page(params) {

    var directory = params.directory;

    var model = {

        directory: path.decode(directory),

        entries: ko.observable([]),

        parent: path.parent(directory),

        subdirectory: {

            form: ko.observable(false),

            directory: ko.observable('').trimmed()
        },

        upload: {

            form: ko.observable(false),

            progress: ko.observable(0)
        }
    };

    // Opens subdirectory form.

    model.subdirectory.show = function() {

        model.subdirectory.form(true);
    };

    // Creates the subdirectory.

    model.subdirectory.submit = function() {

        var sub = model.subdirectory.directory();

        api.createDirectory(directory, sub).then(function() {

            message.info('Directory "' + path.decode(path.join(directory, sub)) +
                '" has been created.');

            model.subdirectory.form(false);

            route.go('directory/' + path.join(directory, sub));

        }).catch(message.error);
    };

    // Cancels and hides the subdirectory form.
    // Resets the form.

    model.subdirectory.cancel = function() {

        model.subdirectory.form(false);
        model.subdirectory.directory('');
    };

    // Shows file upload form.

    model.upload.show = function() {

        model.upload.form(true);
    };

    // Submits the upload form and
    // performs the file upload process.

    model.upload.submit = function() {

        var file = document.getElementById('directory-file').files[0];

        if (!file) {

            return;
        }

        model.upload.file = file;

        var reader = new FileReader();

        reader.onload = function() {

            var xhr = new XMLHttpRequest();

            xhr.upload.addEventListener('progress', model.upload.progress, false);

            xhr.addEventListener('load', model.upload.complete, false);
            xhr.addEventListener('error', model.upload.failed, false);
            xhr.addEventListener('abort', model.upload.aborted, false);

            xhr.open('POST', '/api/upload/' + encodeURIComponent(directory));

            xhr.setRequestHeader('X-Key', api.apiKey());
            xhr.setRequestHeader('X-File-Name', file.name);
            xhr.setRequestHeader('Content-Type', 'application/octet-stream');

            xhr.send(new Uint8Array(reader.result));
        };

        reader.readAsArrayBuffer(file);
    };

    // Cancels and hides the upload form.

    model.upload.cancel = function() {

        model.upload.form(false);
    };

    // Reports the upload progress.
    // FIXME only works on Firefox?

    model.upload.progress = function(e) {

        if (e.lengthComputable) {

            model.upload.progress(Math.round(e.loaded * 100 / e.total));
        }
    };

    // Executed then the upload process
    // is complete.

    model.upload.complete = function() {

        route.refresh();

        message.info('File "' + path.decode(path.join(directory, model.upload.file.name)) +
            '" has been uploaded.');
    };

    model.upload.failed = function() {

        // FIXME needs error checking.
    };

    // Executed when the upload process
    // was aborted.

    model.upload.aborted = function() {

        model.upload.form(false);
    };

    model.remove = function() {

        if (confirm('Remove directory "' + path.decode(directory) + '"?')) {

            api.removeDirectory(directory).then(function() {

                message.info('Directory "' + path.decode(directory) +
                    '" has been successfully removed.');

                route.go('directory/' + model.parent);

            }).catch(message.error);
        }
    };

    api.directory(directory).then(function(list) {

        var entries = list.map(function(data) {

            return directory_item.create(directory, data);

        });

        entries.sort(directory_item.compare);

        model.entries(entries);

    }).catch(message.error);

    return model;
}

ko.components.register('directory', {

    viewModel: { createViewModel: page },

    template: fs.readFileSync(__dirname + '/directory.html', { encoding: 'utf8' })
});
