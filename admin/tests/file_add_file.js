var list_files = require('../tests_util/list_files');

casper.test.begin('Adding a file in root', function suite(test) {

    list_files(casper, test);

    casper.thenClick('.bc-controls > .btn:nth-child(2)');

    casper.then(function() {

        test.assertSelectorHasText('#content > .panel > .panel-heading', 'Upload file');

        test.assertSelectorHasText('#content > .panel > .panel-body > form > .btn:nth-child(3)', 'Upload');

        test.assertSelectorHasText('#content > .panel > .panel-body > form > .btn:nth-child(4)', 'Cancel');
    });

    casper.then(function() {

        this.fillSelectors('#content > .panel > .panel-body > form', {

            '#directory-file': 'tests_util/upload.txt'

        }, false);
    });

    casper.thenClick('#content > .panel > .panel-body > form > .btn:nth-child(3)');

    casper.waitForSelector('#content > .list-group > .list-group-item:nth-child(3)');

    casper.then(function() {

        test.assertSelectorHasText('h2', 'Directory /');

        test.assertSelectorHasText('#messages .message:last-child', 'File "/upload.txt" has been uploaded.');

        test.assertSelectorHasText('#content > .list-group > .list-group-item:nth-child(3)', 'upload.txt');
    });

    casper.thenOpen('http://localhost:18008/upload.txt', function() {

        test.assert(casper.page.plainText === 'This file is to be uploaded.');
    });

    casper.run(function() {

        test.done();
    });
});
