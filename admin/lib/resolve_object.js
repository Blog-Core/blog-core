// Resolves object properties
// as promises.

module.exports = function(obj) {

    var tasks = [], map = [];

    Object.keys(obj).forEach(function(key) {

        tasks.push(obj[key]);

        map.push(key);
    });

    return Promise.all(tasks).then(function(data) {

        var resolved = {};

        data.forEach(function(value, i) {

            resolved[map[i]] = value;
        });

        return resolved;
    });
};
