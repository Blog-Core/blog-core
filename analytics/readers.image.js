// Pixel-based analytics.
// Generates user and session identifiers on client side.
// Shortcuts bots/crawlers/spiders.

(() => {
    const platform = navigator.platform || '';
    if (platform.match(/bot|crawler|spider/i)) {
        // Skips GoogleBot etc.
        return;
    }

    // Helper to check whether the current visitor is the
    // blog administrator.
    const isAdmin = () => {
        return document.cookie.match(/blog_core_admin\s*=\s*1/);
    };

    // Don't run the remainder of the script for the admin.
    if (isAdmin()) {
        return;
    }

    // Helper to generate random identifiers.
    const CHARACTERS = 'abcdefghijklmnopqrstuvwxyz0123456789';
    const generateId = () => {
        let id = '';
        for (let i = 0; i < 32; i++) {
            id += CHARACTERS.charAt(Math.floor(Math.random() * CHARACTERS.length));
        }
        return id;
    };

    // Reads the previous user id or generates a new one.
    const getUserId = () => {
        const match = document.cookie.match(/readers_user\s*=\s*([a-z0-9\-]+)/);
        if (match) {
            return match[1];
        }
        const id = generateId();
        const expires = new Date();
        expires.setFullYear(expires.getFullYear() + 1);
        document.cookie = 'readers_user=' + id + '; path=/; expires=' + expires.toUTCString();
        return id;
    };

    // Reads the previous user id or generates a new one.
    const getSessionId = () => {
        const match = document.cookie.match(/readers_session\s*=\s*([a-z0-9\-]+)/);
        if (match) {
            return match[1];
        }
        const id = generateId();
        document.cookie = 'readers_session=' + id + '; path=/';
        return id;
    };

    // Injects the tracking pixel into the page body.
    const injectPixel = () => {
        const image = document.createElement('img');
        image.width = 1;
        image.height = 1;
        image.style.position = 'fixed';
        image.style.bottom = '0';
        image.style.right = '0';
        image.alt = 'Reader information sent to the backend';
        const params = {
            u: getUserId(),
            s: getSessionId(),
            p: navigator.platform,
            t: document.title,
            e: window.bcEntryId || null,
            r: document.referrer || null
        };
        const query = Object.keys(params).map(
            k => `${k}=${encodeURIComponent(params[k])}`).join('&');
        image.src = `/bc/reader.png?${query}`;
        document.body.appendChild(image);
    };

    injectPixel();
})();
