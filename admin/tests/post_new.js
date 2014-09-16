casper.test.begin('New post', function suite(test) {

    casper.start('http://localhost:18008/admin');

    casper.waitFor(function() {

        return this.exists('button[type="submit"]');
    });

    casper.then(function() {

        test.assertTitle('Blog-Core Administration');
        test.assertSelectorHasText('h2', 'Login');
    });

    casper.then(function() {

        this.fillSelectors('form#login', {

            'input[name="username"]': 'admin@example.com',
            'input[name="password"]': 'admin'

        }, false);
    });

    casper.thenClick('button[type=submit]');

    casper.waitFor(function() {

        return this.fetchText('h2') === 'Posts';
    });

    casper.then(function() {

        test.assertTitle('Blog-Core Administration');
        test.assertSelectorHasText('h2', 'Posts');
    });

    casper.thenClick('#content a.btn:last-child');

    casper.waitFor(function() {

        return this.fetchText('h2') === 'Untitled';
    });

    casper.then(function() {

        test.assertTitle('Blog-Core Administration');
        test.assertSelectorHasText('h2', 'Untitled');

        // Test default values.

        test.assertFieldCSS('#post-type', 'post');
        test.assertFieldCSS('#post-language', 'en');
        test.assertFieldCSS('#post-content-type', 'markdown');
        test.assertFieldCSS('#post-update', new Date().toISOString().substring(0, 10));
        test.assertFieldCSS('#post-date', '');
    });

    // Change title.

    casper.then(function() {

        this.fillSelectors('form#post', {

            '#post-title': 'Hello World'

        }, false);
    });

    casper.then(function() {

        // Check that slug is generated correctly.

        test.assertFieldCSS('#post-slug', 'hello-world');
    });

    // Toggle published

    casper.then(function() {

        this.click('#post-published');
    });

    casper.then(function() {

        // Check that post date is set correctly.

        test.assertFieldCSS('#post-date', new Date().toISOString().substring(0, 10));
    });

    // Try to publish.

    casper.thenClick('div + button');

    casper.then(function() {

        // Error about empty content.

        test.assertSelectorHasText('.error-message', 'Content is not entered.');
    });

    // Set content.

    casper.then(function() {

        this.fillSelectors('form#post', {

            '#post-content': 'This is **Hello World** post.',
            '#post-description': 'Test post.',
            '#post-tags': 'hello, world'

        }, false);
    });

    // Try to publish again.

    casper.thenClick('div + button');

    // Wait for message that post is saved.

    casper.waitFor(function() {

        return this.fetchText('#messages .message') === 'Post saved.';
    });

    casper.then(function() {

        // Check message for test log.

        test.assertSelectorHasText('#messages .message', 'Post saved.');
    });

    // After save we must be back in the
    // posts page.

    casper.waitFor(function() {

        return this.fetchText('h2') === 'Posts';
    });

    casper.then(function() {

        test.assertTitle('Blog-Core Administration');
        test.assertSelectorHasText('h2', 'Posts');
    });

    // Edit the post.

    casper.thenClick('.post-controls a:first-child');

    casper.waitFor(function() {

        return this.fetchText('h2') === 'Hello World';
    });

    casper.then(function() {

        test.assertTitle('Blog-Core Administration');
        test.assertSelectorHasText('h2', 'Hello World');

        // Check form values.

        test.assertFieldCSS('#post-title', 'Hello World');
        test.assertFieldCSS('#post-slug', 'hello-world');
        test.assertFieldCSS('#post-description', 'Test post.');
        test.assertFieldCSS('#post-content', 'This is **Hello World** post.');
        test.assertFieldCSS('#post-type', 'post');
        test.assertFieldCSS('#post-content-type', 'markdown');
        test.assertFieldCSS('#post-tags', 'hello, world');
        test.assertFieldCSS('#post-update', new Date().toISOString().substring(0, 10));
        test.assertFieldCSS('#post-date', new Date().toISOString().substring(0, 10));
        test.assertFieldCSS('#post-language', 'en');
    });

    casper.then(function() {

        // Modify values.

        this.fillSelectors('form#post', {

            '#post-content': 'This is updated **Hello World** post.',
            '#post-description': 'Updated test post.'

        }, false);
    });

    // Submit changes.

    casper.thenClick('div + button');

    // Wait for message that post is updated.

    casper.waitFor(function() {

        return this.fetchText('#messages .message') === 'Post updated.';
    });

    casper.then(function() {

        // Check message for test log.

        test.assertSelectorHasText('#messages .message', 'Post updated.');
    });

    // After update we must be back in the
    // posts page.

    casper.waitFor(function() {

        return this.fetchText('h2') === 'Posts';
    });

    casper.then(function() {

        test.assertTitle('Blog-Core Administration');
        test.assertSelectorHasText('h2', 'Posts');
    });

    // Remove the post.

    casper.thenClick('.post-controls a:last-child');

    // Wait for message that post is removed.

    casper.waitFor(function() {

        return this.fetchText('#messages .message') === 'The post was removed.';
    });

    casper.then(function() {

        // Check message for test log.

        test.assertSelectorHasText('#messages .message', 'The post was removed.');
    });

    casper.run(function() {

        test.done();
    });
});
