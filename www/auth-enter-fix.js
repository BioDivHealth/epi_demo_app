(function() {
  function isAuthInput(el) {
    if (!el || el.nodeType !== 1) {
      return false;
    }

    return Boolean(
      el.matches &&
      (
        el.matches("#auth_user") ||
        el.matches("#auth_pwd") ||
        el.matches("#admin_login_user") ||
        el.matches("#admin_login_password") ||
        el.matches("input[name='auth_user']") ||
        el.matches("input[name='auth_pwd']")
      )
    );
  }

  function findLoginButton() {
    return (
      document.querySelector("#admin_login_submit") ||
      document.querySelector("#auth_button") ||
      document.querySelector("#auth-go_auth") ||
      document.querySelector("button#go_auth") ||
      document.querySelector(".auth button[type='submit']") ||
      document.querySelector(".auth .btn")
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
        document.querySelector("#admin_login_user") ||
        document.querySelector("#auth_user") ||
        document.querySelector("input[name='auth_user'], input[name='user']");
      var passInput =
        document.querySelector("#admin_login_password") ||
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
