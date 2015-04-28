function setErrors(element, valueAccessor) {

    var value = ko.unwrap(valueAccessor());

    if (value.length > 0) {

        var span = element.querySelector('.ko-input-error');

        if (!span) {

            // No existing error display.

            if (!element.className.match(/has\-error/)) {

                element.className = element.className + ' has-error';
            }

            span = document.createElement('span');

            span.className = 'help-block ko-input-error';

            // Finds the holder element.

            var holder = element.querySelector('.checkbox');

            if (!holder) {

                holder = element.querySelector('.form-control');
            }

            holder.parentNode.appendChild(span);
        }

        // Update errors display.

        span.innerHTML = value.join(' ');

    } else {

        // Removes error class and messages.

        element.className = element.className.replace(/has\-error/, '');
        element.className = element.className.replace(/\s+/, ' ');

        var error = element.querySelector('.ko-input-error');

        if (error) {

            error.parentNode.removeChild(error);
        }
    }
}

// Defines the errors binding.

ko.bindingHandlers.errors = {

    init: function(element, valueAccessor) {

        setErrors(element, valueAccessor);
    },

    update: function(element, valueAccessor, allBindings) {

        setErrors(element, valueAccessor);
    }
};
