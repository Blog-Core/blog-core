var fs = require('fs');
var api = require('../api');
var message = require('../message');
var comments_item = require('../vm/comments_item');

// Creates view model for the comments list.

function page(params) {

    var model = {

        title: ko.observable(),

        comments: ko.observable([])
    };

    var tasks = [ api.entryInfo(params.id), api.comments(params.id) ];

    Promise.all(tasks).then(function(data) {

        var info = data[0], comments = data[1];

        model.title(info.title);

        model.comments(comments.map(function(data) {

            return comments_item.create(data);

        }));

    }).catch(message.error);

    return model;
}

ko.components.register('comments', {

    viewModel: { createViewModel: page },

    template: fs.readFileSync(__dirname + '/comments.html', { encoding: 'utf8' })
});
