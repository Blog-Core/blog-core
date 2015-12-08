var fs = require('fs');
var api = require('../api');
var post = require('../vm/post');
var view = require('../view');
var editor = require('../editor');
var preview = require('../preview');
var resolveObject = require('../resolve_object');

var template = fs.readFileSync(__dirname + '/post.html', { encoding: 'utf8' });

// The post edit page.

exports.create = function(type, id, recovered) {

    var model = {

        post: ko.observable(),

        info: ko.observable(false),

        files: ko.observable(false),

        modified: ko.observable(false),

        hasPreview: ko.observable(false),

        previewNotice: ko.observable(false)
    };

    // Shows/hides the info section.

    model.toggleInfo = function() {

        model.info(!model.info());
    };

    // Shows/hides the files section.

    model.toggleFiles = function() {

        model.files(!model.files());
    };

    // Shows preview when possible.

    model.preview = function(callback) {

        if (id) {

            // Preview is shown only for saved posts.

            model.post().submitUpdate().then(function() {

                var slug = model.post().slug();

                var url = model.post().preview.replace(/<slug>/g, slug);

                preview.show(url, function() {

                    editor.focus();
                });
            });

        } else {

            model.previewNotice(true);
        }
    };

    // Leaves from the editing page.

    model.leave = function() {

        if (model.modified()) {

            if (confirm('You have modifications. Leave without saving?')) {

                route.go('entries/' + model.post().type());
            }

        } else {

            route.go('entries/' + model.post().type());
        }
    };

    return postData(type, id).then(function(data) {

        // data.post will be undefined when id is not set.

        model.post(post.create(data.userInfo, type,
            data.types, data.users, data.files, data.post, recovered));

        // Save handler.
        // http://stackoverflow.com/questions/4446987/overriding-controls-save-functionality-in-browser

        function saveHandler(e) {

            if (e.keyCode === 83 && (navigator.platform.match('Mac') ? e.metaKey : e.ctrlKey)) {

                e.preventDefault();

                model.post().submit();
            }

            if (e.keyCode === 73 && (navigator.platform.match('Mac') ? e.metaKey : e.ctrlKey)) {

                e.preventDefault();

                model.preview();
            }
        }

        document.addEventListener('keydown', saveHandler, false);

        // Saves entry contents to
        // localStorage. Prevents data loss
        // after server/browser crash.

        model.autosave = function() {

            var obj = model.post().toJS();

            obj.$id = model.post().$id();

            localStorage.setItem('autosave', JSON.stringify(obj));
        };

        // Starts timer that automatically saves
        // post to localStorage.

        model.autosaveTimer = setInterval(function() {

            // Save only when modified.

            if (model.modified()) {

                model.autosave();
            }

        }, 15000);

        // Clears possible autosave.

        model.clearAutosave = function() {

            localStorage.removeItem('autosave');
        };

        // Remove save handler when
        // view changes.

        model.dispose = function() {

            document.removeEventListener('keydown', saveHandler, false);

            preview.dispose();

            editor.dispose();

            // Stop autosave timer.

            clearTimeout(model.autosaveTimer);
        };

        view.show(template, model);

        // Enable editor for the
        // current page.

        editor.enable();

        if (id) {

            // Set initial editor content.

            if (recovered) {

                editor.begin(recovered.content);

                model.modified(true);

            } else {

                editor.begin(data.post.content);
            }

            // Enable preview

            if (model.post().preview) {

                model.hasPreview(true);
            }

        } else {

            if (recovered) {

                editor.begin(recovered.content);

                model.modified(true);
            }

            model.info(true);

            // Set focus to title.

            var title = document.querySelector('#post-title');

            title.focus();

            if (typeof title.setSelectionRange === 'function') {

                title.setSelectionRange(0, title.value.length);
            }
        }

        // Set up modification detection.

        editor.change(function() {

            model.modified(true);
        });

        model.post().change(function() {

            model.modified(true);
        });

        // Associate into the post.

        model.post().editor = editor;

        // Associate current model as well.

        model.post().parent = model;

        // Automatically update tags input.

        model.post().tags.subscribe(function(value) {

            var datalist = document.getElementById('taglist');

            while (datalist.firstChild) {

                datalist.removeChild(datalist.firstChild);
            }

            var options = [], length;

            // Find matches.

            var lwMatch = value.match(/^(.+\,\s*)(\w+)$/);

            if (lwMatch) {

                var prefix = lwMatch[1];

                var last = lwMatch[2];

                length = last.length;

                data.tags.forEach(function(entry) {

                    if (entry.tag.substring(0, length) === last) {

                        options.push(prefix + entry.tag);
                    }
                });

            } else {

                var fMatch = value.match(/^\w+$/);

                if (fMatch) {

                    length = value.length;

                    data.tags.forEach(function(entry) {

                        if (entry.tag.substring(0, length) === value) {

                            options.push(entry.tag);
                        }
                    });
                }
            }

            // Repopulate the datalist.

            options.forEach(function(option) {

                var elem = document.createElement('option');

                elem.value = option;

                datalist.appendChild(elem);
            });
        });
    });
};

// Retrieves data relevant to
// the post. When id is not set
// then retrieves data needed for the
// new post.

function postData(type, id) {

    return api.userInfo().then(function(userInfo) {

        var requests = {

            types: api.types(),

            users: authors(userInfo),

            userInfo: Promise.resolve(userInfo),

            tags: api.tags(type)
        };

        if (id) {

            requests.post = api.post(id);

            // The entry files.

            requests.files = api.files(id);

        } else {

            requests.files = Promise.resolve([]);
        }

        return resolveObject(requests);
    });
}

// Retrieves selectable authors.
// userInfo - the current user info data.

function authors(userInfo) {

    if (userInfo.type === 'admin') {

        return api.users();

    } else {

        // Non-admin can only select
        // itself as the author.

        return Promise.resolve([{

            $id: userInfo.$id,

            fullname: userInfo.fullname

        }]);
    }
}
