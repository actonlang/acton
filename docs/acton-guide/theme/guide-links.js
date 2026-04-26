// Cross-site navigation for the Acton Guide.

(function() {
    const SITES = [
        { label: "Guide", href: "https://acton.guide/", active: true },
        { label: "Play", href: "https://play.acton.guide/" },
        { label: "Ask", href: "https://ask.acton.guide/" }
    ];

    function createGuideLinks() {
        const nav = document.createElement("nav");
        nav.className = "acton-site-links";
        nav.setAttribute("aria-label", "Acton sites");

        for (const site of SITES) {
            const link = document.createElement("a");
            link.className = "acton-site-link";
            link.href = site.href;
            link.textContent = site.label;

            if (site.active) {
                link.classList.add("is-active");
                link.setAttribute("aria-current", "page");
            }

            nav.appendChild(link);
        }

        return nav;
    }

    function initialize() {
        const menuBar = document.querySelector(".menu-bar");
        const rightButtons = menuBar ? menuBar.querySelector(".right-buttons") : null;

        if (!menuBar || !rightButtons || document.querySelector(".acton-site-links")) {
            return;
        }

        const guideLinks = createGuideLinks();
        rightButtons.appendChild(guideLinks);
    }

    function handlePageChange() {
        setTimeout(initialize, 100);
    }

    document.addEventListener("DOMContentLoaded", initialize);

    let lastUrl = location.href;
    new MutationObserver(() => {
        const url = location.href;
        if (url !== lastUrl) {
            lastUrl = url;
            handlePageChange();
        }
    }).observe(document, { subtree: true, childList: true });

    window.addEventListener("popstate", handlePageChange);
})();
