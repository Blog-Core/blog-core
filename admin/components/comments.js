var fs = require('fs');
var api = require('../api');
var message = require('../message');
var resolveObject = require('../resolve_object');
var comments_item = require('../vm/comments_item');

// Creates view model for the comments list.

function page(params) {

    var model = {

        title: ko.observable(),

        comments: ko.observable([])
    };

    var requests = {

        entryInfo: api.entryInfo(params.id),

        userInfo: api.userInfo(params.id),

        typeInfo: api.typeInfo(params.type),

        comments: api.comments(params.id)
    };

    resolveObject(requests).then(function(data) {

        model.title(data.entryInfo.title);

        model.comments(data.comments.map(function(comments) {

            return comments_item.create(comments, data.userInfo, data.entryInfo, data.typeInfo);

        }));

    }).catch(message.error);

    return model;
}

ko.components.register('comments', {

    viewModel: { createViewModel: page },

    template: fs.readFileSync(__dirname + '/comments.html', { encoding: 'utf8' })
});
