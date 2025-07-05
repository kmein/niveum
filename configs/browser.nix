{
  config,
  pkgs,
  niveumPackages,
  ...
}: {
  environment.systemPackages = [
    niveumPackages.cro
    pkgs.tor-browser-bundle-bin
    pkgs.firefox
    pkgs.brave
  ];

  home-manager.users.me = {
    programs.firefox = {
      enable = true;
      profiles = let
        defaultSettings = {
          "beacon.enabled" = false;
          "browser.bookmarks.showMobileBookmarks" = true;
          "browser.newtab.preload" = false;
          "browser.search.isUS" = false;
          "browser.search.region" = "DE";
          "browser.send_pings" = false;
          "browser.shell.checkDefaultBrowser" = false;
          "browser.startup.homepage" = "chrome://browser/content/blanktab.html";
          "browser.uidensity" = 1;
          "browser.urlbar.placeholderName" = "Search";
          "datareporting.healthreport.service.enabled" = false;
          "datareporting.healthreport.uploadEnabled" = false;
          "datareporting.policy.dataSubmissionEnabled" = false;
          "datareporting.sessions.current.clean" = true;
          "distribution.searchplugins.defaultLocale" = "de-DE";
          "general.smoothScroll" = true;
          "identity.fxaccounts.account.device.name" = config.networking.hostName;
          "network.cookie.cookieBehavior" = 1;
          "privacy.donottrackheader.enabled" = true;
          "privacy.trackingprotection.enabled" = true;
          "privacy.trackingprotection.pbmode.enabled" = true;
          "privacy.trackingprotection.socialtracking.enabled" = true;
          "services.sync.declinedEngines" = "passwords";
          "services.sync.engine.passwords" = false;
          "signon.autofillForms" = false;
          "signon.rememberSignons" = false;
          "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
          "toolkit.telemetry.archive.enabled" = false;
          "toolkit.telemetry.bhrPing.enabled" = false;
          "toolkit.telemetry.cachedClientID" = "";
          "toolkit.telemetry.enabled" = false;
          "toolkit.telemetry.firstShutdownPing.enabled" = false;
          "toolkit.telemetry.hybridContent.enabled" = false;
          "toolkit.telemetry.newProfilePing.enabled" = false;
          "toolkit.telemetry.prompted" = 2;
          "toolkit.telemetry.rejected" = true;
          "toolkit.telemetry.server" = "";
          "toolkit.telemetry.shutdownPingSender.enabled" = false;
          "toolkit.telemetry.unified" = false;
          "toolkit.telemetry.unifiedIsOptIn" = false;
          "toolkit.telemetry.updatePing.enabled" = false;
          "ui.prefersReducedMotion" = 1;
        };
      in {
        default = {
          id = 0;
          isDefault = true;
          settings = defaultSettings;
          # extensions = with pkgs.nur.repos.rycee.firefox-addons; [
          #   ublock-origin
          #   darkreader
          #   sponsorblock
          #   consent-o-matic
          #   i-dont-care-about-cookies
          #   # auto-tab-discard TODO what is this
          # ];
          userChrome = ''
            #TabsToolbar {
              visibility: collapse !important;
            }
          '';
        };
      };
    };
  };

  environment.variables.BROWSER = "firefox";
}
