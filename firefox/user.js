// DNS resolution on the proxy when using a SOCKS proxy
user_pref("network.proxy.socks_remote_dns", true);

// open search results in a new tab
user_pref("browser.search.openintab", true);

// the middle click won't try to load an url
user_pref("middlemouse.contentLoadURL", false);

// DO NOT TRACK
user_pref("privacy.donottrackheader.enabled", true);

// don't highlight the domain name in the address bar (added with bugzilla 451833)
// no really useful and make the rest of the url harder to read
user_pref("browser.urlbar.formatting.enabled", false);

// don't hide the http:// prefix (added with bugzilla 665580)
user_pref("browser.urlbar.trimURLs", false);

// don't load automatically all the plugins on all the pages
user_pref("plugins.click_to_play", true);
user_pref("plugin.state.flash", 1); // firefox 23

// enable telemetry if available
user_pref("toolkit.telemetry.enabled", true);

// don't put the tabs above the bookmarks
user_pref("browser.tabs.onTop", false);

// don't use inline autocomplete
// http://kb.mozillazine.org/Inline_autocomplete
user_pref("browser.urlbar.autoFill", false);

// I don't use the about:newtab feature, let me use the good old about:blank
user_pref("browser.newtab.url", "about:blank");

// saving screenshot of websites I visit ? seriously ?
user_pref("browser.pagethumbnails.capturing_disabled", true);

// disable third party cookie
user_pref("network.cookie.cookieBehavior", 1);
