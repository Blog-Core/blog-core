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

// Sets the softWrap property for the editor.
exports.setSoftWrap = function(softWrap) {
    if (editor) {
        editor.getSession().setWrapLimitRange(softWrap, softWrap);
    }
};

// Sets the font size used by the editor.
exports.setFontSize = function(size) {
    if (editor) {
        editor.setOption('fontSize', size);
    }
};

// Sets the editor content
// and initial cursor position.
exports.begin = function(content) {
    editor.setValue(content);
    // Remove editor's undo log.
    editor.getSession().setUndoManager(new ace.UndoManager());
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

// Inserts into current ACE editor
// at the cursor.
exports.insert = function(text) {
    if (editor) {
        editor.insert(text);
    }
};

// Sets editor content.
exports.setValue = function(text) {
    if (editor) {
        editor.setValue(text);
    }
};

// Cleans up the current editor.
// FIXME remove change callback
exports.dispose = function() {
    if (editor) {
        editor.destroy();
        editor = null;
    }
};
