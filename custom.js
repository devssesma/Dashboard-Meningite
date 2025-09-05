// Evitar que o tabset role automaticamente para o início
document.addEventListener("DOMContentLoaded", function() {
  const tabs = document.querySelectorAll('[data-bs-toggle="tab"]');
  tabs.forEach(function(tab) {
    tab.addEventListener("shown.bs.tab", function(event) {
      event.preventDefault();
    });
  });
});
