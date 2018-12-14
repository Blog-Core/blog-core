exports.create = function(data) {
    return {
        user_id: data.user_id,
        timestamp: data.timestamp,
        duration: data.duration,
        datetime: new Date(data.timestamp * 1000).toISOString(),
        session_count: data.session_count
    };
};
