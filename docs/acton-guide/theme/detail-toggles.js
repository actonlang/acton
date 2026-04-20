// Detail-level toggles for the Acton Guide

(function() {
    const DEFAULT_PREFERENCES = {
        beginner: false,
        advanced: false
    };
    const STORAGE_KEY = "acton-docs-detail-preferences";
    const LEGACY_STORAGE_KEY = "acton-docs-skill-level";

    function normalizePreferences(value) {
        return {
            beginner: Boolean(value && value.beginner),
            advanced: Boolean(value && value.advanced)
        };
    }

    function migrateLegacyPreference() {
        const saved = localStorage.getItem(LEGACY_STORAGE_KEY);
        if (!saved) {
            return null;
        }

        const level = parseInt(saved, 10);
        if (level === 1) {
            return { beginner: true, advanced: false };
        }
        if (level === 3) {
            return { beginner: false, advanced: true };
        }
        return { beginner: false, advanced: false };
    }

    function loadPreferences() {
        const saved = localStorage.getItem(STORAGE_KEY);
        if (saved) {
            try {
                return normalizePreferences(JSON.parse(saved));
            } catch (_error) {
                // Fall through to defaults and legacy migration.
            }
        }

        const migrated = migrateLegacyPreference();
        if (migrated) {
            savePreferences(migrated);
            return migrated;
        }

        return { ...DEFAULT_PREFERENCES };
    }

    function savePreferences(preferences) {
        localStorage.setItem(
            STORAGE_KEY,
            JSON.stringify(normalizePreferences(preferences))
        );
    }

    function applyPreferences(preferences) {
        const normalized = normalizePreferences(preferences);
        document.body.setAttribute(
            "data-show-beginner",
            normalized.beginner ? "true" : "false"
        );
        document.body.setAttribute(
            "data-show-advanced",
            normalized.advanced ? "true" : "false"
        );
    }

    function syncControls(preferences) {
        const beginnerToggle = document.getElementById("toggleBeginnerDetails");
        const advancedToggle = document.getElementById("toggleAdvancedDetails");

        if (beginnerToggle) {
            beginnerToggle.setAttribute(
                "aria-pressed",
                preferences.beginner ? "true" : "false"
            );
            beginnerToggle.classList.toggle("is-active", preferences.beginner);
        }
        if (advancedToggle) {
            advancedToggle.setAttribute(
                "aria-pressed",
                preferences.advanced ? "true" : "false"
            );
            advancedToggle.classList.toggle("is-active", preferences.advanced);
        }
    }

    function currentPreferences() {
        return {
            beginner: document.body.getAttribute("data-show-beginner") === "true",
            advanced: document.body.getAttribute("data-show-advanced") === "true"
        };
    }

    function updatePreferences(preferences) {
        const normalized = normalizePreferences(preferences);
        applyPreferences(normalized);
        syncControls(normalized);
        savePreferences(normalized);
    }

    function createDetailControls() {
        const widget = document.createElement("div");
        widget.className = "skill-control-menu";
        widget.setAttribute("aria-label", "Documentation detail toggles");
        widget.innerHTML = `
            <div class="detail-toggle-wrapper">
                <button
                    type="button"
                    class="detail-toggle detail-toggle-beginner"
                    id="toggleBeginnerDetails"
                    aria-pressed="false"
                    title="Show beginner explanations"
                ><span class="detail-toggle-indicator" aria-hidden="true"></span><span>Beginner</span></button>
                <button
                    type="button"
                    class="detail-toggle detail-toggle-advanced"
                    id="toggleAdvancedDetails"
                    aria-pressed="false"
                    title="Show advanced details"
                ><span class="detail-toggle-indicator" aria-hidden="true"></span><span>Advanced</span></button>
            </div>
        `;
        return widget;
    }

    function attachListeners() {
        const beginnerToggle = document.getElementById("toggleBeginnerDetails");
        const advancedToggle = document.getElementById("toggleAdvancedDetails");

        if (beginnerToggle && !beginnerToggle.dataset.bound) {
            beginnerToggle.addEventListener("click", () => {
                updatePreferences({
                    ...currentPreferences(),
                    beginner: !currentPreferences().beginner
                });
            });
            beginnerToggle.dataset.bound = "true";
        }

        if (advancedToggle && !advancedToggle.dataset.bound) {
            advancedToggle.addEventListener("click", () => {
                updatePreferences({
                    ...currentPreferences(),
                    advanced: !currentPreferences().advanced
                });
            });
            advancedToggle.dataset.bound = "true";
        }
    }

    function initialize() {
        const preferences = loadPreferences();
        applyPreferences(preferences);

        const menuBar = document.querySelector(".menu-bar");
        if (!menuBar) {
            return;
        }

        const rightButtons = menuBar.querySelector(".right-buttons");
        if (!rightButtons) {
            return;
        }

        if (!document.querySelector(".skill-control-menu")) {
            const widget = createDetailControls();
            rightButtons.insertBefore(widget, rightButtons.firstChild);
        }

        syncControls(preferences);
        attachListeners();
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
