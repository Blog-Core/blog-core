var list_files = require('../tests_util/list_files');

casper.test.begin('Adding file with existing name', function suite(test) {

    list_files(casper, test);

    casper.thenClick('.bc-controls > .btn:nth-child(2)');

    casper.then(function() {

        test.assertSelectorHasText('#content > .panel > .panel-heading', 'Upload file');

        test.assertSelectorHasText('#content > .panel > .panel-body > form > .btn:nth-child(3)', 'Upload');

        test.assertSelectorHasText('#content > .panel > .panel-body > form > .btn:nth-child(4)', 'Cancel');
    });

    casper.then(function() {

        this.fillSelectors('#content > .panel > .panel-body > form', {

            '#directory-file': 'tests_util/test.txt'

        }, false);
    });

    casper.thenClick('#content > .panel > .panel-body > form > .btn:nth-child(3)');

    casper.waitForSelector('#messages .alert-danger');

    casper.then(function() {

        test.assertSelectorHasText('h2', 'Directory /');

        test.assertSelectorHasText('#messages .message:last-child', 'The file exists.');
    });

    casper.run(function() {

        test.done();
    });
});
