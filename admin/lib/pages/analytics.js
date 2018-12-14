var api = require('../api');
var view = require('../view');
var resolveObject = require('../resolve_object');
var template = require('./analytics.html');
var message = require('../message');
var dataline = require('../dataline');

// Page for visitor analytics.
exports.create = function() {

    // Generate the sequence of months for the
    // interval selector.
    var startDate = new Date();
    startDate.setUTCFullYear(2018, 11, 1);
    var currentDate = new Date();
    currentDate.setUTCDate(1);
    var months = [];
    while (true) {
        months.push(startDate.toISOString().substring(0, 7));
        if (startDate.getTime() >= currentDate) {
            break;
        }
        startDate.setUTCMonth(startDate.getUTCMonth() + 1);
    }

    var model = {
        durations: ['0', '5', '10', '15', '20', '30', '60', '180', '360'],
        months: months,
        startMonth: ko.observable(currentDate.toISOString().substring(0, 7)),
        endMonth: ko.observable(startDate.toISOString().substring(0, 7)),
        duration: ko.observable('30'),
        results: {
            timeseries: {
                users: ko.observable([]),
                sessions: ko.observable([]),
                pageviews: ko.observable([])
            }
        }
    };

    model.load = function() {
        var start = model.startMonth();
        var end = model.endMonth();
        var duration = parseInt(model.duration(), 10);
        api.analyticsTs(start, end, duration).then(function(data) {
            model.results.timeseries.users(data.users.map(function(user) {
                return user.count;
            }));
            model.results.timeseries.sessions(data.sessions.map(function(session) {
                return session.count;
            }));
            model.results.timeseries.pageviews(data.pageviews.map(function(pageview) {
                return pageview.count;
            }));
        }).catch(function(err) {
            message.error(err);
        });
    };

    model.results.timeseries.users.subscribe(function() {
        setTimeout(function() {
            dataline.draw('analytics-chart-users');
        }, 100);
    });

    model.results.timeseries.sessions.subscribe(function() {
        setTimeout(function() {
            dataline.draw('analytics-chart-sessions');
        }, 100);
    });

    model.results.timeseries.pageviews.subscribe(function() {
        setTimeout(function() {
            dataline.draw('analytics-chart-pageviews');
        }, 100);
    });

    model.load();

    return Promise.resolve().then(function() {
        view.show(template, model);
    });
};
