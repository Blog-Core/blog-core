var view = require('../view');
var api = require('../api');
var ko = require('../lib/knockout');
var route = require('../lib/router');
var hex = require('../hex');
var message = require('../message');

// Comparator function to sort
// directories before files and both
// then by name.

function compareEntry(entry1, entry2) {

    // Compares directory entries by name.

    function compareName() {

        return entry1.name < entry2.name ? -1 : 1;
    }

    if (entry1.directory) {

        if (entry2.directory) {

            return compareName();

        } else {

            return -1;
        }

    } else {

        if (entry2.directory) {

            return 1;

        } else {

            return compareName();
        }
    }
}

exports.directory = function(directory) {

    return api.directory(directory).then(function(entries) {

        entries.sort(compareEntry);

        entries.forEach(function(entry) {

            entry.encoded = hex.hex((directory === '/' ? '' : directory) + '/' + entry.name);
        });

        var parent = '/';

        if (directory !== '/') {

            var tokens = directory.split(/\//);

            parent = hex.hex('/' + tokens.slice(1, tokens.length - 1).join('/'));
        }

        var model = {

            directory: directory,
            entries: entries,
            subdirectory: ko.observable(),
            subdirectory_form: ko.observable(false),
            upload_form: ko.observable(false),
            parent: parent,

            addSubdirectory: function() {

                api.createDirectory(directory, model.subdirectory()).then(function(response) {

                    if (response.status === 'success') {

                        route.refresh();
                    }
                });
            },

            showSubdirectoryForm: function() {

                model.subdirectory_form(true);
                model.upload_form(false);
            },

            cancelDirectoryForm: function() {

                model.subdirectory_form(false);
            },

            removeDirectory: function() {

                if (confirm('Remove current directory?')) {

                    api.removeDirectory(directory).then(function(response) {

                        if (response.status === 'success') {

                            message.info('Directory successfully removed.');

                            route.go('directory/' + parent);
                        }
                    });
                }
            },

            upload: function(form) {

                var file = document.getElementById('directory-file').files[0];

                if (file) {

                    var reader = new FileReader();

                    reader.onload = function() {

                        var xhr = new XMLHttpRequest();

                        xhr.upload.addEventListener('progress', model.uploadProgress, false);

                        xhr.addEventListener('load', model.uploadComplete, false);
                        xhr.addEventListener('error', model.uploadFailed, false);
                        xhr.addEventListener('abort', model.uploadCanceled, false);

                        xhr.open('POST', '/api/upload/' + encodeURIComponent(window.btoa(directory)));

                        xhr.setRequestHeader('X-Key', api.apiKey());
                        xhr.setRequestHeader('X-File-Name', file. name);
                        xhr.setRequestHeader('Content-Type', 'application/octet-stream');

                        xhr.send(new Uint8Array(reader.result));
                    }

                    reader.readAsArrayBuffer(file);
                }
            },

            uploadProgress: function(e) {

                if (e.lengthComputable) {

                    var percentComplete = Math.round(e.loaded * 100 / e.total);

                    console.log(percentComplete);
                }
            },

            uploadComplete: function() {

                route.refresh();
                message.info('File uploaded.');
            },

            uploadFailed: function() {


            },

            uploadCanceled: function() {

                model.upload_form(false);
            },

            showUploadForm: function() {

                model.upload_form(true);
                model.subdirectory_form(false);
            },

            cancelUploadForm: function() {

                model.upload_form(false);
            }
        };

        return view.show('directory', model);
    });
};

exports.file = function(file) {

    return api.file(file).then(function(response) {

        var tokens = file.split(/\//);

        var parent = hex.hex('/' + tokens.slice(1, tokens.length - 1).join('/'));

        var model = {

            file: file,
            parent: parent,
            size: response.data.size,
            date: response.data.modified,

            remove: function() {

                if (confirm('Remove file ' + file + '?')) {

                    api.removeFile(file).then(function(response) {

                        if (response.status === 'success') {

                            message.info('File removed.');

                            route.go('directory/' + parent);
                        }
                    });
                }
            }
        };

        return view.show('file', model);
    });
};
