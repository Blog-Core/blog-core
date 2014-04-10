var ko = require('../lib/knockout');
var route = require('../lib/router');
var api = require('../api');
var message = require('../message');
var validate = require('../validate');
var speakingurl = require('../lib/speakingurl');

exports.create = function(authors, data) {

    var myid = sessionStorage.getItem('user-id');

    var mytype = sessionStorage.getItem('user-type');

    var author = findAuthor(authors, myid);

    var post = {

        authors: authors,
        title: ko.observable('Untitled'),
        slug: ko.observable('untitled'),
        description: ko.observable(''),
        content: ko.observable(''),
        type: ko.observable('post'),
        content_type: ko.observable('markdown'),
        published: ko.observable(false),
        commenting: ko.observable(true),
        date_published: ko.observable(Math.floor(Date.now() / 1000)),
        tags: ko.observable(''),
        comments: ko.observable(0),
        author: ko.observable(author),
        author_selectable: mytype === 'admin',
        date: ko.observable(''),

        dfmode: function() {

            var editor = document.getElementById('post-content');

            editor.style.border = '0';
            editor.style.top = '0';
            editor.style.left = '0';
            editor.style.position = 'fixed';
            editor.style.height = '100%';
        },

        save: function(form) {

            validate.clear(form);

            if (post.title() === '') {

                validate.error('post-title', 'Title is not entered.');
            }

            var slug = post.slug();

            if (slug === '') {

                validate.error('post-slug', 'Slug is not entered.');

            } else {

                if (!slug.match(/^[a-z0-9\-_]+$/)) {

                    validate.error('post-slug', 'Use lowercase letters, numbers, hyphen and underscore.');
                }
            }

            if (post.content() === '') {

                validate.error('post-content', 'Content is not entered.');
            }

            var date = post.date();

            if (date === '') {

                if (post.published()) {

                    validate.error('post-date', 'Publish date is not entered.');
                }

            } else if (!date.match(/^\d{4}\-\d{2}\-\d{2}$/)) {

                validate.error('post-date', 'Date must be in the YYYY-MM-DD format.');
            }

            if (validate.hasError(form)) {

                return false;
            }

            if (post.$id) {

                api.updatePost(post.$id, post.toJS()).then(function(response) {

                    if (response.status === 'success') {

                        message.info('Post updated.');

                        route.go(post.type() + 's');

                    } else {

                        validate.formError(form, response.message);

                        window.scroll(0, 0);
                    }

                }, message.error).done();

            } else {

                api.savePost(post.toJS()).then(function(response) {

                    if (response.status === 'success') {

                        message.info('Post saved.');

                        route.go(post.type() + 's');

                    } else {

                        validate.formError(form, response.message);

                        window.scroll(0, 0);
                    }

                }, message.error).done();
            }
        },

        toJS: function() {

            var tags = post.tags().trim();

            var date = post.date();

            // date_published will be undefined when
            // no date has been entered.

            var date_published;

            if (date !== '') {

                var match = date.match(/^(\d{4})\-(\d{2})\-(\d{2})$/);

                if (!match) {

                    throw new Error('Date does not match pattern: ' + date);
                }

                var d = new Date();

                d.setUTCHours(0, 0, 0, 0);
                d.setUTCFullYear(parseInt(match[1], 10), parseInt(match[2], 10) - 1, parseInt(match[3], 10));

                date_published = Math.floor(d.getTime() / 1000);
            }

            return {

                author: post.author().$id,
                title: post.title(),
                slug: post.slug(),
                description: post.description(),
                content: post.content(),
                type: post.type(),
                tags: [],
                date_published: date_published,
                date_updated: Math.floor(Date.now() / 1000),
                commenting: post.commenting(),
                published: post.published(),
                content_type: post.content_type(),
                tags: tags === '' ? [] : tags.split(/\, */)
            };
        }
    };

    if (data) {

        var author = findAuthor(authors, data.author);

        if (typeof data.date_published !== 'undefined') {

            var d = new Date(data.date_published * 1000);

            post.date(d.toISOString().substring(0, 10));
        }

        post.$id = data.$id;
        post.author(author);
        post.title(data.title);
        post.slug(data.slug);
        post.description(data.description || '');
        post.content(data.content);
        post.type(data.type);
        post.content_type(data.content_type);
        post.published(data.published);
        post.commenting(data.commenting);
        post.tags(data.tags.join(', '));
        post.comments(data.comments);

    } else {

        // Only when for new post.
        // Add automatic slug generation.

        post.title.subscribe(function(value) {

            post.slug(speakingurl(value));
        });
    }

    post.published.subscribe(function(value) {

        // Set publish date when post is published.

        if (value && post.date() === '') {

            post.date(new Date().toISOString().substring(0, 10));
        }
    });

    return post;
};

function findAuthor(authors, id) {

    var author;

    for (var i = 0; i < authors.length; i++) {

        if (authors[i].$id === id) {

            author = authors[i];

            break;
        }
    }

    if (!author) {

        throw new Error('No author ' + data.author);
    }

    return author;
}
