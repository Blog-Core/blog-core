var fs = require('fs');
var api = require('../api');
var post = require('../vm/post');
var view = require('../view');
var preview = require('../preview');
var resolveObject = require('../resolve_object');

var template = fs.readFileSync(__dirname + '/post.html', { encoding: 'utf8' });

// The post edit page.

exports.create = function(type, id) {

    var model = {

        post: ko.observable(),

        info: ko.observable(false),

        files: ko.observable(false)
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

        var url = model.post().preview.replace(/<slug>/g, model.post().slug());

        preview.show(url, callback);
    };

    return postData(id).then(function(data) {

        // data.post will be undefined when id is not set.

        model.post(post.create(data.userInfo, type, data.types, data.users, data.files, data.post));

        // Save handler.
        // http://stackoverflow.com/questions/4446987/overriding-controls-save-functionality-in-browser

        function saveHandler(e) {

            if (e.keyCode === 83 && (navigator.platform.match('Mac') ? e.metaKey : e.ctrlKey)) {

                e.preventDefault();

                model.post().submit();
            }

            if (e.keyCode === 73 && (navigator.platform.match('Mac') ? e.metaKey : e.ctrlKey)) {

                e.preventDefault();

                model.post().submitUpdate().then(function() {

                    model.preview(function() {

                        editor.focus();
                    });
                });
            }
        }

        document.addEventListener('keydown', saveHandler, false);

        // Remove save handler when
        // view changes.

        model.dispose = function() {

            document.removeEventListener('keydown', saveHandler, false);

            preview.dispose();
        };

        view.show(template, model);

        // Setup Ace

        var editor = ace.edit('editor');

        editor.container.style.lineHeight = 1.5

        editor.setOptions({

            maxLines: Infinity,

            showLineNumbers: false,

            wrap: true,

            showPrintMargin: false,

            showFoldWidgets: false,

            showGutter: false,

            displayIndentGuides: false,

            fontSize: 14,

            fontFamily: 'monospace',

            useSoftTabs: true,

            tabSize: 2
        });

        // Theme

        editor.setTheme('ace/theme/github');

        // Mode

        editor.getSession().setMode('ace/mode/markdown');

        if (id) {

            editor.setValue(data.post.content);

            // Go to last line

            editor.focus();

            editor.gotoLine(1);

        } else {

            model.info(true);

            // Set focus to title.

            var title = document.querySelector('#post-title');

            title.focus();

            if (typeof title.setSelectionRange === 'function') {

                title.setSelectionRange(0, title.value.length);
            }
        }

        // Associate into the post.

        model.post().editor = editor;

        // Associate current model as well.

        model.post().parent = model;
    });
};

// Retrieves data relevant to
// the post. When id is not set
// then retrieves data needed for the
// new post.

function postData(id) {

    return api.userInfo().then(function(userInfo) {

        var requests = {

            types: api.types(),

            users: authors(userInfo),

            userInfo: Promise.resolve(userInfo)
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
