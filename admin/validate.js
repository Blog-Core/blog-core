// Cleans the form from error messages.

exports.clear = function(form) {

    var errors = form.querySelectorAll('.error-message');
    var error, i;

    for (i = 0; i < errors.length; i++) {

        error = errors.item(i);

        error.parentNode.classList.remove('has-error');
        error.parentNode.removeChild(error);
    }

    errors = form.parentNode.querySelectorAll('.alert-danger');

    for (i = 0; i < errors.length; i++) {

        error = errors.item(i);
        error.parentNode.removeChild(error);
    }
};

// Checks whether the form
// has a validation error.

exports.hasError = function(form) {

    return form.querySelectorAll('.error-message').length > 0;
};

// Adds error to the container of the
// given input.

exports.error = function(id, message) {

    var elem = document.getElementById(id);

    var error = document.createElement('div');

    error.className = 'error-message';
    error.innerHTML = message;

    elem.parentNode.appendChild(error);
    elem.parentNode.classList.add('has-error');
};

// Sets error to the whole form.

exports.formError = function(form, message) {

    var error = document.createElement('div');

    error.className = 'alert alert-danger';
    error.innerHTML = message;

    form.parentNode.insertBefore(error, form);
};
