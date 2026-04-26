// Keep mdBook's theme menu focused on the Acton visual themes.

(function() {
    const THEME_LABELS = {
        default_theme: "Auto",
        light: "Acton Light",
        ayu: "Acton Dark"
    };
    const LEGACY_THEME_MAP = {
        rust: "light",
        coal: "ayu",
        navy: "ayu"
    };

    function migrateLegacyTheme() {
        const saved = localStorage.getItem("mdbook-theme");
        const replacement = LEGACY_THEME_MAP[saved];

        if (replacement) {
            localStorage.setItem("mdbook-theme", replacement);
            document.documentElement.classList.remove(saved);
            document.documentElement.classList.add(replacement);
        }
    }

    function movePrintControl() {
        const leftButtons = document.querySelector(".menu-bar .left-buttons");
        const printButton = document.getElementById("print-button");
        const printLink = printButton ? printButton.closest("a") : null;

        if (!leftButtons || !printLink || printLink.parentElement === leftButtons) {
            return;
        }

        printLink.classList.add("acton-print-control");
        leftButtons.appendChild(printLink);
    }

    function pruneThemeMenu() {
        const themeList = document.getElementById("theme-list");

        if (!themeList) {
            return;
        }

        for (const button of themeList.querySelectorAll("button.theme")) {
            const label = THEME_LABELS[button.id];
            const item = button.closest("li");

            if (!label) {
                if (item) {
                    item.remove();
                }
                continue;
            }

            button.textContent = label;
        }
    }

    function initialize() {
        migrateLegacyTheme();
        movePrintControl();
        pruneThemeMenu();
    }

    initialize();
    document.addEventListener("DOMContentLoaded", initialize);
})();
