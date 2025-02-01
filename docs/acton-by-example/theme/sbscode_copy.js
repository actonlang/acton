document.addEventListener('DOMContentLoaded', () => {
    // Select all side-by-side-code tables
    const codeTables = document.querySelectorAll('table.side-by-side-code');

    codeTables.forEach(table => {
        // Get all code cells in this table
        const codeCells = table.querySelectorAll('td:last-child');
        if (codeCells.length === 0) return;

        // Create button container div
        const buttonDiv = document.createElement('div');
        buttonDiv.className = 'buttons';

        // Create copy button with mdBook's styling
        const copyButton = document.createElement('button');
        copyButton.className = 'fa fa-copy clip-button';
        copyButton.setAttribute('title', 'Copy to clipboard');
        copyButton.setAttribute('aria-label', 'Copy to clipboard');

        // Add tooltip span
        const tooltip = document.createElement('i');
        tooltip.className = 'tooltiptext';
        copyButton.appendChild(tooltip);

        // Get all code blocks from all cells in this table
        const getAllCode = () => {
            const allCodeBlocks = table.querySelectorAll('td:last-child pre code');
            return Array.from(allCodeBlocks)
                .map(block => block.textContent)
                .join('\n');
        };

        // Add click handler
        copyButton.addEventListener('click', async () => {
            const code = getAllCode();
            await navigator.clipboard.writeText(code);

            // Visual feedback using tooltip
            tooltip.textContent = 'Copied!';
            setTimeout(() => {
                tooltip.textContent = '';
            }, 2000);
        });

        // Add the button to the container and container to the first code cell
        buttonDiv.appendChild(copyButton);
        codeCells[0].appendChild(buttonDiv);
    });
});
