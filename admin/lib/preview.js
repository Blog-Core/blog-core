// Shows preview for the given URL.
var last, callback, editorScrollTop, viewScrollTop = -1;

// Callback is called when preview is closed.
exports.show = function(url, cb) {
    var iframe = holder();
    if (iframe) {
        if (last === url) {
            // Just refresh.
            iframe.contentWindow.location.reload(true);
        } else {
            iframe.src = url;
            viewScrollTop = -1;
        }
    } else {
        iframe = document.createElement('iframe');
        iframe.src = url;
        preview.appendChild(iframe);
        viewScrollTop = -1;
    }
    // Store scroll position.
    editorScrollTop = document.body.scrollTop;
    // Show preview.
    main.style.display = 'none';
    preview.style.display = 'block';
    controls.style.display = 'block';
    if (viewScrollTop > 0) {
        // Restore preview scroll position when set.
        document.body.scrollTop = viewScrollTop;
    }
    last = url;
    callback = cb;
};

exports.dispose = function() {
    // Clean preview element.
    preview.innerHTML = '';
    callback = null;
};

function holder() {
    return preview.querySelector('iframe');
}

// Main and preview elements.
var main = document.getElementById('main');
var preview = document.getElementById('preview');
var controls = document.getElementById('preview-controls');

// Control button to close the preview.
var close = controls.querySelector('[data-control="close"]');

close.addEventListener('click', function(e) {
    e.preventDefault();
    hide();
}, false);

// Preview hide handler.
function hideHandler(e) {
    if (e.keyCode === 27) {
        hide();
    }
}

function hide() {
    // Store preview scroll position.
    viewScrollTop = document.body.scrollTop;
    main.style.display = 'block';
    preview.style.display = 'none';
    controls.style.display = 'none';
    if (typeof callback === 'function') {
        callback();
    }
    // Restore editor scroll position.
    document.body.scrollTop = editorScrollTop;
}

document.addEventListener('keydown', hideHandler, false);

// Starts resize timer.
var timer;
function resize() {
    var iframe = holder();
    if (iframe) {
        var contentDocument = iframe.contentDocument;
        if (contentDocument.body) {
            var scrollHeight = contentDocument.body.scrollHeight;
            var iframeHeight = iframe.clientHeight;
            if (scrollHeight > iframeHeight) {
                iframe.style.height = (scrollHeight + 50) + 'px';
            }
        }
    }
    timer = setTimeout(resize, 100);
}
timer = resize();
