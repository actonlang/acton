// Skill Slider JavaScript for Acton Guide

(function() {
    // Default skill level (1=beginner, 2=standard, 3=advanced)
    const DEFAULT_SKILL = 2;
    const STORAGE_KEY = 'acton-docs-skill-level';
    
    // Get saved skill level or use default
    function getSavedSkillLevel() {
        const saved = localStorage.getItem(STORAGE_KEY);
        return saved ? parseInt(saved) : DEFAULT_SKILL;
    }
    
    // Save skill level
    function saveSkillLevel(level) {
        localStorage.setItem(STORAGE_KEY, level);
    }
    
    // Create skill slider widget for menu bar
    function createSkillSlider() {
        const widget = document.createElement('div');
        widget.className = 'skill-control-menu';
        widget.innerHTML = `
            <div class="skill-slider-wrapper">
                <input type="range" min="1" max="3" value="${getSavedSkillLevel()}" class="skill-slider" id="globalSkillSlider" title="Documentation Detail Level">
                <span class="skill-label active" data-level="${getSavedSkillLevel()}"></span>
            </div>
        `;
        return widget;
    }
    
    // Update skill level
    function updateSkillLevel(level) {
        document.body.setAttribute('data-skill', level);
        
        // Update label text
        const labels = ['', 'Beginner', 'Standard', 'Advanced'];
        const activeLabel = document.querySelector('.skill-control-menu .skill-label');
        if (activeLabel) {
            activeLabel.textContent = labels[level];
            activeLabel.setAttribute('data-level', level);
        }
        
        // Save preference
        saveSkillLevel(level);
    }
    
    // Initialize on page load
    function initialize() {
        // Set initial skill level
        const savedLevel = getSavedSkillLevel();
        updateSkillLevel(savedLevel);
        
        // Check if slider already exists in menu bar
        if (document.querySelector('.skill-control-menu')) return;
        
        // Find menu bar
        const menuBar = document.querySelector('.menu-bar');
        if (!menuBar) return;
        
        // Find the right section of menu bar (where theme picker is)
        const rightButtons = menuBar.querySelector('.right-buttons');
        if (!rightButtons) return;
        
        // Add skill slider widget before the theme button
        const widget = createSkillSlider();
        rightButtons.insertBefore(widget, rightButtons.firstChild);
        
        // Set up event listener
        const slider = document.getElementById('globalSkillSlider');
        if (slider) {
            slider.addEventListener('input', (e) => {
                updateSkillLevel(parseInt(e.target.value));
            });
        }
        
        // Update labels to show current state
        updateSkillLevel(savedLevel);
    }
    
    // Handle page navigation in mdBook
    function handlePageChange() {
        // Small delay to ensure DOM is ready
        setTimeout(initialize, 100);
    }
    
    // Initialize on first load
    document.addEventListener('DOMContentLoaded', initialize);
    
    // Re-initialize on mdBook page changes
    // mdBook uses AJAX for navigation, so we need to watch for changes
    let lastUrl = location.href;
    new MutationObserver(() => {
        const url = location.href;
        if (url !== lastUrl) {
            lastUrl = url;
            handlePageChange();
        }
    }).observe(document, {subtree: true, childList: true});
    
    // Also listen for popstate events
    window.addEventListener('popstate', handlePageChange);
})();