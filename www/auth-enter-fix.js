(function() {
  function isAuthInput(el) {
    if (!el || el.nodeType !== 1) {
      return false;
    }

    if (el.matches && (el.matches("#auth_user") || el.matches("#auth_pwd"))) {
      return true;
    }

    if (el.matches && el.matches("input[type='password']")) {
      return true;
    }

    if (el.matches && el.matches("input[name='auth_user'], input[name='user']")) {
      return true;
    }

    return false;
  }

  function findLoginButton() {
    return (
      document.querySelector("#auth_button") ||
      document.querySelector("#auth-go_auth") ||
      document.querySelector("button#go_auth") ||
      document.querySelector("#button") ||
      document.querySelector(".auth .btn, .auth button[type='submit']") ||
      document.querySelector("button[type='submit']")
    );
  }

  function flushInput(el) {
    if (!el || el.nodeType !== 1 || typeof el.value === "undefined") {
      return;
    }

    el.dispatchEvent(new Event("input", { bubbles: true }));
    el.dispatchEvent(new Event("change", { bubbles: true }));
    if (typeof el.blur === "function") {
      el.blur();
    }
  }

  document.addEventListener(
    "keydown",
    function(e) {
      if (e.key !== "Enter") {
        return;
      }
      if (e.shiftKey || e.ctrlKey || e.altKey || e.metaKey) {
        return;
      }
      if (!isAuthInput(e.target)) {
        return;
      }

      e.preventDefault();
      e.stopPropagation();

      var userInput =
        document.querySelector("#auth_user") ||
        document.querySelector("input[name='auth_user'], input[name='user']");
      var passInput =
        document.querySelector("#auth_pwd") ||
        document.querySelector("input[type='password']");

      flushInput(userInput);
      flushInput(passInput);
      flushInput(e.target);

      window.setTimeout(function() {
        var btn = findLoginButton();
        if (btn) {
          btn.click();
        }
      }, 40);
    },
    true
  );
})();
