var fs = require('fs');
var api = require('../api');
var message = require('../message');
var posts_item = require('../vm/posts_item');

// Component for displaying a post list.

function page(params) {

    var type = params.type;

    var model = {

        type: type,

        all: ko.observable([]),

        count: ko.observable(5),

        step: ko.observable(5)
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

    var tasks = [ api.userInfo(), api.posts(type) ];

    Promise.all(tasks).then(function(data) {

        var info = data[0], posts = data[1];

        posts.sort(function(post1, post2) {

            return post2.date_updated - post1.date_updated;
        });

        model.all(posts.map(function(data) {

            var post = posts_item.create(data);

            post.editable = info.type === 'admin' || info.$id === post.author;

            return post;

        }));

    }).catch(message.error);

    return model;
}

ko.components.register('posts', {

    viewModel: { createViewModel: page },

    template: fs.readFileSync(__dirname + '/posts.html', { encoding: 'utf8' })
});
