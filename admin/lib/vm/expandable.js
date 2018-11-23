// Mixin for expandable view models.
exports.mixin = function(obj) {
    obj.expanded = ko.observable(false);
    obj.expand = function() {
        if (obj.expanded()) {
            obj.expanded(false);
        } else {
            obj.expanded(true);
        }
    };
};
