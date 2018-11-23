var fs = require('fs');
var api = require('../api');
var view = require('../view');
var resolveObject = require('../resolve_object');
var comments_item = require('../vm/comments_item');

var template = fs.readFileSync(__dirname + '/comments.html', { encoding: 'utf8' });

// Page for the comments list.
exports.create = function(type, id) {

    var model = {
        title: ko.observable(),
        comments: ko.observable([])
    };

    var requests = {
        entryInfo: api.entryInfo(id),
        userInfo: api.userInfo(id),
        typeInfo: api.typeInfo(type),
        comments: api.comments(id)
    };

    function flatten(comments) {
        var flat = [];
        flattenRec(comments, flat, 0);
        return flat;
    }

    function flattenRec(comments, list, depth) {
        comments.forEach(function(comment) {
            comment.depth = depth;
            list.push(comment);
            flattenRec(comment.replies, list, depth + 1);
        });
    }
    
    return resolveObject(requests).then(function(data) {
        model.title(data.entryInfo.title);
        model.comments(flatten(data.comments).map(function(comment) {
            return comments_item.create(comment,
                data.userInfo, data.entryInfo, data.typeInfo);
        }));
        view.show(template, model);
    });
};
