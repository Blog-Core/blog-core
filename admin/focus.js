// Sets focus to the given selector.

module.exports = function(selector) {

    var el = document.querySelector(selector);

    if (el) {

        el.focus();
    }
};
