var message = require('../message');
var view = require('../view');
var api = require('../api');

exports.list = function(id) {

    return api.entryInfo(id).then(function(info) {

        return api.comments(id).then(function(comments) {

            comments.forEach(function(comment) {

                comment.expanded = ko.observable(false);

                comment.expand = function() {

                    if (comment.expanded()) {

                        comment.expanded(false);

                    } else {

                        comment.expanded(true);
                    }
                };

                comment.remove = function() {

                    if (confirm('Remove the comment?')) {

                        api.removeComment(comment.$id).then(function(response) {

                            if (response.status === 'success') {

                                route.refresh();

                            } else {

                                message.error(response.message);
                            }

                        }, message.error);
                    }
                };
            });

            var model = {

                title: info.title,
                comments: comments
            };

            return view.show('comments', model);
        });
    });
};
