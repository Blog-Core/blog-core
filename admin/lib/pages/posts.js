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

        count: ko.observable(5),

        step: ko.observable(5),

        comments: ko.observable(false),

        create: ko.observable(false),

        title: ko.observable(),

        loaded: ko.observable(false)
    };

    // Shows whether there are more
    // pager pages.

    model.hasMore = ko.pureComputed(function() {

        return model.count() < model.all().length;
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

        model.count(model.all().length);
    };

    // Posts array considering the current
    // pager state.

    model.posts = ko.pureComputed(function() {

        var all = model.all();

        return all.slice(0, model.count());
    });

    // Finds data from API and updates
    // the view.

    var requests = {

        typeInfo: api.typeInfo(type),

        userInfo: api.userInfo(),

        posts: api.posts(type)

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

        data.posts.sort(function(post1, post2) {

            return post2.date_updated - post1.date_updated;

        });

        model.all(data.posts.map(function(postData) {

            return postsItem.create(postData, data.typeInfo, data.userInfo);

        }));

        view.show(template, model);

    });
};
