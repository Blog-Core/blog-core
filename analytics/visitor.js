(() => {
    const startTimestamp = Date.now();
    let pageviewId = null;

    // Reads the previously stored user id.
    const readUserId = () => {
        const match = document.cookie.match(/visitor_user\s*=\s*([a-z0-9\-]+)/);
        return match ? match[1] : null;
    };

    // User ids use permanent cookies.
    const writeUserId = (userId) => {
        const expires = new Date();
        expires.setFullYear(expires.getFullYear() + 1);
        document.cookie = 'visitor_user=' + userId + '; path=/; expires=' + expires.toUTCString();
    };

    const readSessionId = () => {
        const match = document.cookie.match(/visitor_session\s*=\s*([a-z0-9\-]+)/);
        return match ? match[1] : null;
    };

    const writeSessionId = (sessionId) => {
        document.cookie = 'visitor_session=' + sessionId + '; path=/';
    };

    // Helper to run HTTP POST request to the backend.
    const postJSON = (url, data, cb) => {
        const supported = typeof XMLHttpRequest !== 'undefined' &&
            typeof JSON !== 'undefined';
        if (supported) {
            const xmlhttp = new XMLHttpRequest();
            xmlhttp.open('POST', url);
            xmlhttp.setRequestHeader('Content-Type', 'application/json');
            xmlhttp.send(JSON.stringify(data));
            xmlhttp.onreadystatechange = () => {
                if (xmlhttp.readyState === 4 && xmlhttp.status === 200) {
                    cb(JSON.parse(xmlhttp.responseText));
                }
            };
        }
    };

    // Tries to use either the stored user id
    // or generates a new one on the server.
    const doWithUser = (cb) => {
        const userId = readUserId();
        if (userId) {
            // Use the stored user id.
            cb(userId);
        } else {
            // Record the new user.
            postJSON('/api/visitor/user', {}, (response) => {
                if (response.status === 'success') {
                    const userId = response.data;
                    // Remember for later.
                    writeUserId(userId);
                    // Use the generated user id.
                    cb(userId);
                }
            });
        }
    };

    // Tries to use either the stored session id
    // or generates a new one on the server.
    const doWithSession = (userId, cb) => {
        const sessionId = readSessionId();
        if (sessionId) {
            // Use the stored session id.
            cb(userId, sessionId);
        } else {
            // Record the new session.
            const data = {
                user_id: userId,
                agent: navigator.userAgent || null,
                platform: navigator.platform || null
            };
            postJSON('/api/visitor/session', data, (response) => {
                if (response.status === 'success') {
                    const sessionId = response.data;
                    // Remember for later.
                    writeSessionId(sessionId);
                    // Use the generated id.
                    cb(userId, sessionId);
                }
            });
        }
    };

    // Starts reporting the current page.
    const doWithPageview = (userId, sessionId, cb) => {
        const elapsed = Math.floor((Date.now() - startTimestamp) / 1000);
        if (pageviewId) {
            // Repeated pageview. Just send elapsed time.
            const data = {
                pageview_id: pageviewId,
                elapsed: elapsed
            }
            postJSON('/api/visitor/pageview_extend', data, (response) => {
                if (response.status === 'success') {
                    cb(userId, sessionId);
                }
            });
        } else {
            const data = {
                session_id: sessionId,
                location: window.location.toString(),
                referrer: document.referrer || null,
                elapsed: elapsed
            };
            // First pageview record.
            postJSON('/api/visitor/pageview', data, (response) => {
                if (response.status === 'success') {
                    // Update the current pageview id.
                    pageviewId = response.data;
                    cb(userId, sessionId);
                }
            });
        }
    };

    // Reports the current page periodically with
    // exponential interval.
    const reportPageviewLoop = (userId, sessionId, timeout) => {
        doWithPageview(sessionId, sessionId, () =>
            setTimeout(() =>
                reportPageviewLoop(userId, sessionId, 2 * timeout), timeout));
    };

    doWithUser((userId) => {
        doWithSession(userId, (userId, sessionId) => {
            reportPageviewLoop(userId, sessionId, 1000);
        });
    });    
})();
