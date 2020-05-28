{ config, pkgs, ... }:

let
  scripts = "/home/wohanley/scripts";
in {
  # Let Home Manager install and manage itself.
  programs.home-manager = {
    enable = true;
    path = "/home/wohanley/code/home-manager";
  };

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "19.09";

  home.packages = [
    pkgs.glibcLocales
    pkgs.pass
  ];

  # https://github.com/NixOS/nix/issues/599
  # https://github.com/rycee/home-manager/issues/354
  home.sessionVariables.LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";

  services.gpg-agent = {
    enable = true;
    # enableSshSupport = true;
    defaultCacheTtl = 31536000;
    maxCacheTtl = 31536000;
    extraConfig = ''
      pinentry-program /usr/bin/pinentry
    '';
  };

  ###
  # Email
  ###

  accounts.email = {
    maildirBasePath = "/home/wohanley/.mail";
    accounts.me = {
      address = "me@wohanley.com";
      primary = true;
      userName = "me@wohanley.com";
      passwordCommand = "${pkgs.pass}/bin/pass email/me";
      realName = "William O'Hanley";
      imap.host = "valerie.asoshared.com";
      mbsync = {
        enable = true;
        create = "both";
        remove = "both";
        expunge = "both";
      };
      goimapnotify = {
        enable = true;
        # Not notifying on spam because I think it brings me up against ASO's
        # user+IP connection limit. mbsync will still pick them up
        boxes = [ "INBOX" "INBOX.Drafts" "INBOX.Sent" ];
        onNewMail = "${scripts}/mail/on-new-mail.sh me";
      };
      smtp.host = "valerie.asoshared.com";
      msmtp = {
        enable = true;
      };
      notmuch.enable = true;
    };
    accounts.uvic = {
      address = "whohanley@uvic.ca";
      userName = "whohanley";
      passwordCommand = "${pkgs.pass}/bin/pass email/uvic";
      realName = "William O'Hanley";
      imap.host = "imap.uvic.ca";
      mbsync = {
        enable = true;
        create = "both";
        remove = "both";
        expunge = "both";
      };
      goimapnotify = {
        enable = true;
        boxes = [ "INBOX" "Drafts" "Junk" "Sent" "mail/drafts" ];
        onNewMail = "${scripts}/mail/on-new-mail.sh uvic";
      };
      smtp = {
        host = "smtp.uvic.ca";
        port = 587;
        tls.useStartTls = true;
      };
      msmtp = {
        enable = true;
      };
    };
    accounts.gmail = {
      address = "willy.ohanley@gmail.com";
      userName = "willy.ohanley@gmail.com";
      passwordCommand = "${pkgs.pass}/bin/pass email/gmail";
      realName = "William O'Hanley";
      flavor = "gmail.com";
      mbsync = {
        enable = true;
        create = "both";
        remove = "both";
        expunge = "both";
      };
      goimapnotify = {
        enable = true;
        boxes = [ "INBOX" "Junk" "Sent" "Willy.OHanley@dal.ca" "[Gmail]/All Mail" "[Gmail]/Drafts" "[Gmail]/Sent Mail" "[Gmail]/Spam" ];
        onNewMail = "${scripts}/mail/on-new-mail.sh gmail";
      };
      msmtp = {
        enable = true;
      };
    };
  };

  programs.mbsync.enable = true;

  services.goimapnotify = {
    enable = true;
  };

  programs.msmtp.enable = true;

  programs.notmuch = {
    # unfortunately notmuch is also installed through pacman, because I can't
    # figure out how to make the shared lib available to python if it's
    # installed through nix
    enable = true;
    extraConfig.headers.index = ''
      headers.XBogosity=X-Bogosity
      headers.XSpamFlag=X-Spam-Flag
    '';
    new.tags = [ "new" ];
    hooks.postNew = ''
      # Apply initial tags (sent, notify, etc)
      ${pkgs.notmuch}/bin/notmuch tag --input ${scripts}/mail/notmuch-post-new-tags
      # Train bogofilter on my own sent mail (ham)
      ${pkgs.notmuch}/bin/notmuch search --output=files tag:new and tag:sent | xargs bogofilter -nB
      # Remove new, we're done
      ${pkgs.notmuch}/bin/notmuch tag -new -- tag:new
    '';
    search.excludeTags = ["deleted" "spam"];
  };
}
