(function() {
  var key = 'epilondon_help_seen';
  function navType() {
    var entries = performance.getEntriesByType && performance.getEntriesByType('navigation');
    if (entries && entries.length && entries[0].type) {
      return entries[0].type;
    }
    if (performance.navigation && typeof performance.navigation.type !== 'undefined') {
      return performance.navigation.type === 1 ? 'reload' : 'navigate';
    }
    return 'navigate';
  }
  var type = navType();
  var seen = false;
  try { seen = localStorage.getItem(key) === '1'; } catch (e) { seen = false; }
  var shouldShow = (!seen) || type === 'reload';
  if (shouldShow) {
    try { localStorage.setItem(key, '1'); } catch (e) {}
  }
  var send = function() {
    if (!shouldShow) {
      return false;
    }
    if (window.Shiny && Shiny.setInputValue) {
      Shiny.setInputValue('help_auto', Date.now(), {priority: 'event'});
      return true;
    }
    return false;
  };
  var tries = 0;
  var retry = function() {
    tries += 1;
    if (send()) {
      return;
    }
    if (tries < 40) {
      setTimeout(retry, 250);
    }
  };
  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', retry);
  } else {
    retry();
  }
  document.addEventListener('shiny:connected', retry);
})();
