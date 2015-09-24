var editor;

// Enables editor for the current page.

exports.enable = function() {

    // Setup Ace

    editor = ace.edit('editor');

    editor.$blockScrolling = Infinity;

    editor.container.style.lineHeight = 1.5;

    editor.setOptions({

        maxLines: Infinity,

        showLineNumbers: false,

        wrap: true,

        showPrintMargin: false,

        showFoldWidgets: false,

        showGutter: false,

        displayIndentGuides: false,

        fontSize: 14,

        fontFamily: 'monospace',

        useSoftTabs: true,

        tabSize: 2
    });

    // Theme

    editor.setTheme('ace/theme/github');

    // Mode

    editor.getSession().setMode('ace/mode/markdown');
};

// Sets the editor content
// and initial cursor position.

exports.begin = function(content) {

    editor.setValue(content);

    // Go to first line

    editor.focus();

    editor.gotoLine(1);
};

// Sets callback to be called
// when editor contents is modified.

exports.change = function(callback) {

    editor.on('change', callback);
};

// Focuses on the editor.

exports.focus = function() {

    editor.focus();
};

// Returns the current editor
// content.

exports.content = function() {

    return editor.getValue();
};

// Cleans up the current editor.

exports.dispose = function() {

    if (editor) {

        editor.destroy();

        editor = null;
    }
};
