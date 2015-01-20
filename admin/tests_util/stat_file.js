var list_files = require('./list_files');

// Helper to add steps to a file stat page.

module.exports = function(casper, test) {

    list_files(casper, test);

    casper.thenClick('#content > .list-group > .list-group-item:nth-child(2)');

    casper.waitForUrl(/#file\/2f746573742e747874$/);

    casper.then(function() {

        test.assertSelectorHasText('h2', 'File /test.txt');

        test.assertSelectorHasText('.bc-controls > .btn:nth-child(1)', 'Up');

        test.assertSelectorHasText('.bc-controls > .btn:nth-child(2)', 'Direct link (opens in new tab)');

        test.assertSelectorHasText('.bc-controls > .btn:nth-child(3)', 'Remove');
    });
};
