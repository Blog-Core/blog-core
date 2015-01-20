var list_files = require('../tests_util/list_files');

casper.test.begin('Adding a directory in root', function suite(test) {

    list_files(casper, test);

    casper.thenClick('.bc-controls > .btn:nth-child(1)');

    casper.then(function() {

        test.assertSelectorHasText('#content > .panel > .panel-heading', 'New directory');

        test.assertSelectorHasText('#content > .panel > .panel-body > form > .btn:nth-child(2)', 'Add');

        test.assertSelectorHasText('#content > .panel > .panel-body > form > .btn:nth-child(3)', 'Cancel');
    });

    casper.then(function() {

        this.fillSelectors('#content > .panel > .panel-body > form', {

            '#directory-new': 'new'

        }, false);
    });

    casper.thenClick('#content > .panel > .panel-body > form > .btn:nth-child(2)');

    casper.waitForUrl(/#directory\/2f6e6577$/);

    casper.then(function() {

        test.assertSelectorHasText('h2', 'Directory /new');

        test.assertSelectorHasText('#messages .message:last-child', 'Directory "/new" has been created.');
    });

    casper.run(function() {

        test.done();
    });
});
