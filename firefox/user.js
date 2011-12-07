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
