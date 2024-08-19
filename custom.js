document.querySelectorAll('.custom-tabs .nav-link').forEach(function(tab) {
    tab.addEventListener('click', function(event) {
        event.preventDefault(); // Prevent the default action (page jump)
        let hash = this.hash;

        // Find the corresponding tab pane and display it
        let targetPane = document.querySelector(hash);
        let allPanes = document.querySelectorAll('.custom-tabs .tab-pane');
        allPanes.forEach(function(pane) {
            pane.classList.remove('active', 'show');
        });

        targetPane.classList.add('active', 'show');

        // Ensure the tab's URL hash is updated without scrolling
        history.replaceState(null, null, hash);
    });
});
