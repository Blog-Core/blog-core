var list_files = require('./list_files');

// Helper to add steps to the file list (/).

module.exports = function(casper, test) {

    list_files(casper, test);

    casper.thenClick('#content > .list-group > .list-group-item:nth-child(1)');

    casper.waitForUrl(/#directory\/2f70617468$/);

    casper.then(function() {

        test.assertSelectorHasText('h2', 'Directory /path');

        test.assertSelectorHasText('.bc-controls > .btn:nth-child(1)', 'Up');

        test.assertSelectorHasText('.bc-controls > .btn:nth-child(2)', 'Add');

        test.assertSelectorHasText('.bc-controls > .btn:nth-child(3)', 'Upload');

        test.assertSelectorHasText('.bc-controls > .btn:nth-child(4)', 'Remove');

        test.assertSelectorHasText('#content > .list-group > .list-group-item:nth-child(1)', 'to');
    });
};
