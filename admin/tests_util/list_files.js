var login = require('./login');

// Helper to add steps to the file list (/).

module.exports = function(casper, test) {

    login(casper, test);

    casper.thenClick('a[href="#files"]');

    // Root directory is displayed by default.

    casper.waitForUrl(/#directory\/2f$/);

    // Wait until directory is loaded.

    casper.waitForSelector('#content > .list-group > .list-group-item:nth-child(1)');

    casper.then(function() {

        test.assertSelectorHasText('h2', 'Directory /');

        test.assertSelectorHasText('.bc-controls > .btn:nth-child(1)', 'Add');

        test.assertSelectorHasText('.bc-controls > .btn:nth-child(2)', 'Upload');

        test.assertSelectorHasText('#content > .list-group > .list-group-item:nth-child(1)', 'path');

        test.assertSelectorHasText('#content > .list-group > .list-group-item:nth-child(2)', 'test.txt');
    });
};
