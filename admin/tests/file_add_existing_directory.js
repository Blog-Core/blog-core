var list_files = require('../tests_util/list_files');

casper.test.begin('Adding a directory with existing name', function suite(test) {

    list_files(casper, test);

    casper.thenClick('.bc-controls > .btn:nth-child(1)');

    casper.then(function() {

        test.assertSelectorHasText('#content > .panel > .panel-heading', 'New directory');

        test.assertSelectorHasText('#content > .panel > .panel-body > form > .btn:nth-child(2)', 'Add');

        test.assertSelectorHasText('#content > .panel > .panel-body > form > .btn:nth-child(3)', 'Cancel');
    });

    casper.then(function() {

        this.fillSelectors('#content > .panel > .panel-body > form', {

            '#directory-new': 'path'

        }, false);
    });

    casper.thenClick('#content > .panel > .panel-body > form > .btn:nth-child(2)');

    casper.waitForUrl(/#directory\/2f$/);

    casper.then(function() {

        test.assertSelectorHasText('h2', 'Directory /');

        test.assertSelectorHasText('#messages .message:last-child', 'Error: The directory exists.');
    });

    casper.run(function() {

        test.done();
    });
});
