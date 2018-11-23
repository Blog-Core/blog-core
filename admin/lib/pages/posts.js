var fs = require('fs');
var api = require('../api');
var view = require('../view');
var postsItem = require('../vm/posts_item');
var resolveObject = require('../resolve_object');

var template = fs.readFileSync(__dirname + '/posts.html', { encoding: 'utf8' });

// Page for displaying a post list.
exports.create = function(type) {

    var model = {
        type: type,
        all: ko.observable([]),
        count: ko.observable(10),
        step: ko.observable(10),
        comments: ko.observable(false),
        create: ko.observable(false),
        title: ko.observable(),
        loaded: ko.observable(false),
        tags: ko.observable([]),
        tag: ko.observable(),
        status: ko.observable('all')
    };

    // List of filtered posts.
    model.filtered = ko.pureComputed(function() {
        var tag = model.tag();
        var status = model.status();
        var filtered = [];
        model.all().forEach(function(entry) {
            var tagMatch = !tag || entry.tags.indexOf(tag) > -1;
            var statusMatch = status === 'all' || (
                status === 'published' && entry.published) || (
                status === 'unpublished' && !entry.published);
            if (tagMatch && statusMatch) {
                filtered.push(entry);
            }
        });
        return filtered;
    });

    // Shows whether there are more
    // pager pages.
    model.hasMore = ko.pureComputed(function() {
        return model.count() < model.filtered().length;
    });

    // Shows new pager page with posts.
    model.showMore = function() {
        model.count(model.count() + model.step());
        setTimeout(function() {
            // Scrolls to the bottom.
            window.scrollTo(0, document.body.scrollHeight);
        }, 50);
    };

    // Shows all posts.
    model.showAll = function() {
        model.count(model.filtered().length);
    };

    // Posts array considering the current
    // pager state.
    model.posts = ko.pureComputed(function() {
        var all = model.filtered();
        return all.slice(0, model.count());
    });

    // Finds data from API and updates
    // the view.
    var requests = {
        typeInfo: api.typeInfo(type),
        userInfo: api.userInfo(),
        posts: api.posts(type),
        tags: api.tags(type)
    };

    return resolveObject(requests).then(function(data) {
        model.title(data.typeInfo.menu_label);
        model.comments(data.typeInfo.comments);
        var create = false;
        // Finds if the user can create entries.
        if (data.userInfo.type === 'admin') {
            create = true;
        }
        if (data.typeInfo.grants.indexOf('create') >= 0) {
            create = true;
        }
        model.create(create);
        data.tags.forEach(function(tag) {
            tag.label = tag.tag + ' (' + tag.count + ')';
        });
        data.tags.sort(function(t1, t2) {
            return t1.tag === t2.tag ? 0 : (t1.tag < t2.tag ? -1 : 1);
        });
        model.tags(data.tags);
        data.posts.sort(function(post1, post2) {
            return post2.date_updated - post1.date_updated;
        });
        model.all(data.posts.map(function(postData) {
            return postsItem.create(postData, data.typeInfo, data.userInfo);

        }));
        view.show(template, model);
    });
};
