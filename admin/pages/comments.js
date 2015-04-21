var fs = require('fs');
var api = require('../api');
var view = require('../view');
var resolveObject = require('../resolve_object');
var comments_item = require('../vm/comments_item');

var template = fs.readFileSync(__dirname + '/comments.html', { encoding: 'utf8' });

// Page for the comments list.

exports.create = function(id) {

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

    return resolveObject(requests).then(function(data) {

        model.title(data.entryInfo.title);

        model.comments(data.comments.map(function(comments) {

            return comments_item.create(comments, data.userInfo, data.entryInfo, data.typeInfo);

        }));

        view.show(template, model);
    });
};
