// Zoomable Mermaid SVG JS
function enableZoomPan(svg) {
    let scale = 1, panX = 0, panY = 0, isPanning = false, startX, startY;
    svg.parentElement.classList.add('zoom-container');
    svg.style.transformOrigin = "0 0";

    svg.addEventListener('wheel', function (e) {
        e.preventDefault();
        const delta = e.deltaY > 0 ? 0.9 : 1.1;
        scale *= delta;
        svg.style.transform = `translate(${panX}px,${panY}px) scale(${scale})`;
    });

    svg.addEventListener('mousedown', function (e) {
        isPanning = true;
        startX = e.clientX - panX;
        startY = e.clientY - panY;
        svg.style.cursor = "grabbing";
    });
    window.addEventListener('mousemove', function (e) {
        if (!isPanning) return;
        panX = e.clientX - startX;
        panY = e.clientY - startY;
        svg.style.transform = `translate(${panX}px,${panY}px) scale(${scale})`;
    });
    window.addEventListener('mouseup', function () {
        isPanning = false;
        svg.style.cursor = "grab";
    });
}

function observeMermaidSVG() {
    function tryEnableZoom() {
        const mermaidContainers = document.querySelectorAll('svg.mermaid-js');
        mermaidContainers.forEach(container => {
            const hasGraphics = container.querySelector('g, path, text');
            if (hasGraphics) {
                enableZoomPan(container);
                return true;
            }
        });
        return false;
    }

    let attempts = 0;
    const maxAttempts = 30;
    function poll() {
        if (tryEnableZoom() || attempts >= maxAttempts) return;
        attempts++;
        setTimeout(poll, 200);
    }
    poll();
}

function setupMermaidTooltips() {
    const container = document.body;
    let tooltip = null;
    let hideTimeout = null;
    let overNode = false;
    let overTooltip = false;

    function showTooltip(node) {
        if (tooltip) return;
        tooltip = document.createElement('div');
        tooltip.className = 'mermaid-tooltip';
        tooltip.innerHTML = node.getAttribute('title');
        container.appendChild(tooltip);

        // Position tooltip top-right of node, add scroll offset for correct placement when scrolled
        const rect = node.getBoundingClientRect();
        const tooltipRect = tooltip.getBoundingClientRect();
        const vpWidth = window.innerWidth;
        const vpHeight = window.innerHeight;
        let x = rect.right + 8 + window.scrollX;
        let y = rect.top + rect.height / 2 - 8 + window.scrollY;

        if (x + tooltipRect.width > vpWidth + window.scrollX) x = rect.left - tooltipRect.width - 8 + window.scrollX;
        if (y + tooltipRect.height > vpHeight + window.scrollY) y = vpHeight + window.scrollY - tooltipRect.height - 8;
        if (y < window.scrollY) y = window.scrollY + 8;

        tooltip.style.left = x + 'px';
        tooltip.style.top = y + 'px';

        // Tooltip hover tracking
        tooltip.addEventListener('mouseenter', () => {
            overTooltip = true;
            if (hideTimeout) clearTimeout(hideTimeout);
        });
        tooltip.addEventListener('mouseleave', () => {
            overTooltip = false;
            hideTimeout = setTimeout(hideIfOutside, 100);
        });
    }

    function hideIfOutside() {
        if (!overNode && !overTooltip && tooltip) {
            tooltip.remove();
            tooltip = null;
        }
    }

    // Event delegation: listen on document for nodes
    document.addEventListener('mouseover', e => {
        const node = e.target.closest('g.node[title]');
        if (node) {
            overNode = true;
            if (hideTimeout) clearTimeout(hideTimeout);
            showTooltip(node);
        }
    });

    document.addEventListener('mouseout', e => {
        const node = e.target.closest('g.node[title]');
        if (node) {
            overNode = false;
            hideTimeout = setTimeout(hideIfOutside, 100);
        }
    });
}

document.addEventListener('DOMContentLoaded', () => {
    observeMermaidSVG();
    setupMermaidTooltips();
});
