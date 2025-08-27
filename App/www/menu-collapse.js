document.addEventListener('DOMContentLoaded', function() {
  const navLinks = document.querySelectorAll('.navbar-nav .nav-link:not(.dropdown-toggle)');
  const dropdownItems = document.querySelectorAll('.dropdown-item');

  function animateCollapse(navbarCollapse) {
    navbarCollapse.style.height = navbarCollapse.scrollHeight + 'px';
    navbarCollapse.offsetHeight; // force reflow

    navbarCollapse.classList.remove('show');
    navbarCollapse.classList.remove('collapse');
    navbarCollapse.classList.add('collapsing');

    navbarCollapse.style.transition = 'height 0.35s ease';
    navbarCollapse.style.height = '0px';

    navbarCollapse.addEventListener('transitionend', function handler() {
      navbarCollapse.classList.remove('collapsing');
      navbarCollapse.classList.add('collapse');
      navbarCollapse.style.height = '';
      navbarCollapse.style.transition = '';
      navbarCollapse.removeEventListener('transitionend', handler);
    });
  }

  function collapseMenu() {
    if (window.innerWidth < 992) {
      const navbarCollapse = document.querySelector('.navbar-collapse');
      const navbarToggle = document.querySelector('.navbar-toggle');

      if (navbarCollapse && navbarCollapse.classList.contains('show')) {
        animateCollapse(navbarCollapse);
      }

      if (navbarToggle) {
        navbarToggle.classList.add('collapsed');
        navbarToggle.setAttribute('aria-expanded', 'false');
      }
    }
  }

  navLinks.forEach(function(link) {
    link.addEventListener('click', function() {
      setTimeout(collapseMenu, 10);
    });
  });

  dropdownItems.forEach(function(item) {
    item.addEventListener('click', function() {
      setTimeout(collapseMenu, 10);
    });
  });
});
