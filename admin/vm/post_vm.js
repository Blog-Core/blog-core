var ko = require('../lib/knockout');
var route = require('../lib/router');
var api = require('../api');
var message = require('../message');

exports.create = function(data) {

    var post = {

        title: ko.observable('Untitled'),
        slug: ko.observable(''),
        description: ko.observable(''),
        content: ko.observable(''),
        type: ko.observable('post'),
        content_type: ko.observable('markdown'),
        published: ko.observable(false),
        commenting: ko.observable(true),
        date_published: ko.observable(Math.floor(Date.now() / 1000)),
        tags: ko.observable(''),

        dfmode: function() {

            var editor = document.getElementById('post-content');

            editor.style.border = '0';
            editor.style.top = '0';
            editor.style.left = '0';
            editor.style.position = 'fixed';
            editor.style.height = '100%';
        },

        save: function() {

            if (post.$id) {

                api.updatePost(post.$id, post.toJS()).then(function(response) {

                    if (response.status === 'success') {

                        message.info('Post updated.');

                        route.go('posts')
                    }

                }).done();

            } else {

                api.savePost(post.toJS()).then(function(response) {

                    if (response.status === 'success') {

                        message.info('Post saved.');

                        route.go('post/' + response.data);
                    }
                });
            }
        },

        toJS: function() {

            var tags = post.tags().trim();

            return {

                author: post.author,
                title: post.title(),
                slug: post.slug(),
                description: post.description(),
                content: post.content(),
                type: post.type(),
                tags: [],
                date_published: post.date_published(),
                date_updated: Math.floor(Date.now() / 1000),
                commenting: post.commenting(),
                published: post.published(),
                content_type: post.content_type(),
                tags: tags === '' ? [] : tags.split(/\, */)
            };
        }
    };

    if (data) {

        post.$id = data.$id;
        post.author = data.author;
        post.title(data.title);
        post.slug(data.slug);
        post.description(data.description || '');
        post.content(data.content);
        post.type(data.type);
        post.content_type(data.content_type);
        post.published(data.published);
        post.commenting(data.commenting);
        post.tags(data.tags.join(', '));
    }

    return post;
};
