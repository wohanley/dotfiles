;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs

   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused

   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t

   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(auto-completion
     bibtex
     clojure
     csv
     ;; deft
     ;; elixir
     emacs-lisp
     git
     python
     elm
     (go :variables
         go-use-gometalinter t
         go-tab-width 4)
     html
     ivy
     markdown
     notdeft
     org
     org-roam
     pdf
     python
     react
     ruby
     shell-scripts
     (spell-checking :variables
                     spell-checking-enable-by-default nil)
     sql
     syntax-checking
     systemd
     version-control
     who-dired
     who-email
     who-nix
     who-org
     yaml
     zotxt
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press `SPC f e R' (Vim style) or
     ;; `M-m f e R' (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     ;; better-defaults
     ;; helm
     ;; lsp
     ;; multiple-cursors
     ;; (shell :variables
     ;;        shell-default-height 30
     ;;        shell-default-position 'bottom)
     ;; treemacs
     )

   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   ;; To use a local version of a package, use the `:location' property:
   ;; '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages '(dtrt-indent)

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil then enable support for the portable dumper. You'll need
   ;; to compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper nil

   ;; Name of executable file pointing to emacs 27+. This executable must be
   ;; in your PATH.
   ;; (default "emacs")
   dotspacemacs-emacs-pdumper-executable-file "emacs"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=~/.emacs.d/.cache/dumps/spacemacs.pdmp
   ;; (default spacemacs.pdmp)
   dotspacemacs-emacs-dumper-dump-file "spacemacs.pdmp"

   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default t)
   dotspacemacs-verify-spacelpa-archives t

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'emacs

   ;; If non-nil show the version string in the Spacemacs buffer. It will
   ;; appear as (spacemacs version)@(emacs version)
   ;; (default t)
   dotspacemacs-startup-buffer-show-version t

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 10)
                                (projects . 5))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Default major mode for a new empty buffer. Possible values are mode
   ;; names such as `text-mode'; and `nil' to use Fundamental mode.
   ;; (default `text-mode')
   dotspacemacs-new-empty-buffer-major-mode 'text-mode

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(sanityinc-solarized-dark
                        sanityinc-solarized-light)

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(spacemacs :separator wave :separator-scale 1.5)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font or prioritized list of fonts.
   dotspacemacs-default-font '("Iosevka"
                               :size 14
                               :weight normal
                               :width normal)
                               ;; :powerline-scale 1.1)

   ;; The leader key (default "SPC")
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"

   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"

   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"

   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","

   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil

   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1

   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache

   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5

   ;; If non-nil, the paste transient-state is enabled. While enabled, after you
   ;; paste something, pressing `C-j' and `C-k' several times cycles through the
   ;; elements in the `kill-ring'. (default nil)
   dotspacemacs-enable-paste-transient-state nil

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4

   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil

   ;; If non-nil the frame is undecorated when Emacs starts up. Combine this
   ;; variable with `dotspacemacs-maximized-at-startup' in OSX to obtain
   ;; borderless fullscreen. (default nil)
   dotspacemacs-undecorated-at-startup nil

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line.
   ;; If you use Emacs as a daemon and wants unicode characters only in GUI set
   ;; the value to quoted `display-graphic-p'. (default t)
   dotspacemacs-mode-line-unicode-symbols t

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; Control line numbers activation.
   ;; If set to `t', `relative' or `visual' then line numbers are enabled in all
   ;; `prog-mode' and `text-mode' derivatives. If set to `relative', line
   ;; numbers are relative. If set to `visual', line numbers are also relative,
   ;; but lines are only visual lines are counted. For example, folded lines
   ;; will not be counted and wrapped lines are counted as multiple lines.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :visual nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; When used in a plist, `visual' takes precedence over `relative'.
   ;; (default nil)
   dotspacemacs-line-numbers '(:relative nil
                               :visual nil
                               :disabled-for-modes org-roam-backlinks-mode)

   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc...
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server nil

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%U@%a | %s"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env))

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."
  )

(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
  )

(defun who/kill-current-buffer (arg)
  "Kill the current buffer. Save modifications first unless prefix argument passed."
  (interactive "P")
  (if (and (buffer-file-name) (buffer-modified-p) (not arg)) (save-buffer))
  (kill-buffer (current-buffer)))

(defun who/save-buffers-kill-frame ()
  (interactive)
  (save-some-buffers)
  (spacemacs/frame-killer))

(defun who/undo-kill-buffer (arg)
  "Re-open the last buffer killed.  With ARG, re-open the nth buffer. See
http://emacs.stackexchange.com/questions/3330/how-to-reopen-just-killed-buffer-like-c-s-t-in-firefox-browser"
  (interactive "p")
  (let ((recently-killed-list (copy-sequence recentf-list))
        (buffer-files-list
         (delq nil (mapcar (lambda (buf)
                             (when (buffer-file-name buf)
                               (expand-file-name (buffer-file-name buf))))
                           (buffer-list)))))
    (mapc
     (lambda (buf-file)
       (setq recently-killed-list
             (delq buf-file recently-killed-list)))
     buffer-files-list)
    (find-file
     (if arg (nth arg recently-killed-list)
       (car recently-killed-list)))))

(defun who/flycheck-use-eslint-from-node-modules ()
  "Configure flycheck to use eslint from node_modules/.bin, if such an executable
exists at such a path somewhere in the parent of the buffer's file. See
http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable"
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

;; xdg-open and advising find-file per https://emacs.stackexchange.com/questions/3105/how-to-use-an-external-program-as-the-default-way-to-open-pdfs-from-emacs

(defun who/xdg-open (filename)
  (interactive "Filename: ")
  (let ((process-connection-type))
    (start-process "" nil "xdg-open" (expand-file-name filename))))

(defun who/find-file-auto (orig-fun &rest args)
  (let ((filename (car args)))
    (if (cl-find-if
         (lambda (regexp) (string-match regexp filename))
         '("\\.docx?\\'"))
        (who/xdg-open filename)
      (apply orig-fun args))))

(advice-add 'find-file :around 'who/find-file-auto)

(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."

  ;; (setenv "TZ" "Canada/Pacific")

  (setq mail-host-address "wohanley.com")
  (add-hook 'message-mode-hook #'turn-off-auto-fill)

  (setq epa-pinentry-mode 'loopback)
  ;; (pinentry-start)

  (add-hook 'text-mode-hook 'spacemacs/toggle-visual-line-navigation-on)

  ;; flash the frame to represent a bell
  (setq visible-bell t)

  ;; kill frame but keep daemon running per https://medium.com/@bobbypriambodo/blazingly-fast-spacemacs-with-persistent-server-92260f2118b7
  (spacemacs/set-leader-keys
    "q q" 'spacemacs/prompt-kill-emacs
    "q f" 'who/save-buffers-kill-frame
    "q F" 'spacemacs/frame-killer)

  ;; don't look for completions on tab
  (setq tab-always-indent t)

  ;; follow symlinks to source-controlled files without asking
  (setq vc-follow-symlinks t)

  ;; anything that writes to the buffer while the region is active will
  ;; overwrite it (like in most editors)
  (delete-selection-mode 1)
  ;; <home> and <end> should do The Right Thing
  (global-unset-key (kbd "<end>"))
  (global-set-key (kbd "<end>") 'move-end-of-line)
  (global-unset-key (kbd "<home>"))
  (global-set-key (kbd "<home>") 'back-to-indentation)
  ;; line-by-line scrolling
  (global-set-key [M-up] (lambda () (interactive) (scroll-down 5)))
  (global-set-key [M-down] (lambda () (interactive) (scroll-up 5)))
  ;; slurp and barf
  (global-set-key (kbd "C-s-<right>") 'sp-forward-slurp-sexp)
  (global-set-key (kbd "C-s-<left>") 'sp-backward-slurp-sexp)
  (global-set-key (kbd "C-S-s-<right>") 'sp-forward-barf-sexp)
  (global-set-key (kbd "C-S-s-<left>") 'sp-backward-barf-sexp)
  ;; save and kill current buffer (rebound from kill-buffer)
  (global-set-key (kbd "C-x k") 'who/kill-current-buffer)
  ;; undo kill buffer web browser style
  (global-set-key (kbd "C-S-t") 'who/undo-kill-buffer)
  ;; magit
  (global-set-key (kbd "C-x g") 'magit-status)
  ;; fold bindings
  (spacemacs/set-leader-keys "z c" 'evil-close-fold)
  (spacemacs/set-leader-keys "z o" 'evil-open-fold)
  ;; highlight symbol at point
  (spacemacs/toggle-automatic-symbol-highlight-on)
  ;; dtrt-indent does nice dynamic tab length
  ;; (dtrt-indent-mode t)
  ;; use local eslint
  (add-hook 'flycheck-mode-hook 'who/flycheck-use-eslint-from-node-modules)
  ;; Go
  (setq flycheck-gometalinter-fast t)
  (setq flycheck-gometalinter-deadline "10s")
  (setq flycheck-gometalinter-disable-linters '("gocyclo"))
  ;; Less
  (add-to-list 'auto-mode-alist '("\\.less\\'" . less-css-mode))
  (add-to-list 'auto-mode-alist '("\\.less\\'" . flycheck-mode))
  ;; .mak is a Mako template
  (add-to-list 'auto-mode-alist '("\\.mak\\'" . web-mode))
  ;; anaconda-mode can fuck itself
  (remove-hook 'anaconda-mode-response-read-fail-hook
               'anaconda-mode-show-unreadable-response)
  ;; so "# -*- coding: utf8 -*-" works
  (define-coding-system-alias 'utf8 'utf-8)
  ;; PDFs open too small without this
  (add-hook 'pdf-view-mode-hook 'pdf-view-fit-width-to-window))

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ahs-idle-interval 0.75)
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#839496" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"))
 '(css-indent-offset 2)
 '(custom-safe-themes
   (quote
    ("4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" default)))
 '(dtrt-indent-max-lines 500)
 '(elm-format-on-save t)
 '(evil-want-Y-yank-to-eol nil)
 '(fci-rule-color "#073642" t)
 '(js-indent-level 2)
 '(js2-mode-show-parse-errors nil)
 '(js2-mode-show-strict-warnings nil)
 '(package-selected-packages
   (quote
    (org-projectile org-category-capture org-present org-pomodoro alert log4e gntp org-mime org-download htmlize gnuplot org-roam winum git-gutter-fringe+ git-gutter-fringe fringe-helper git-gutter+ git-gutter fuzzy transient reformatter diff-hl clojure-snippets clj-refactor inflections paredit lv cider-eval-sexp-fu cider sesman queue parseedn clojure-mode parseclj a gh-md mmm-mode markdown-toc markdown-mode smeargle orgit org magit-gitflow gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link evil-magit magit magit-popup git-commit with-editor insert-shebang fish-mode company-shell rvm ruby-tools ruby-test-mode rubocop rspec-mode robe rbenv rake minitest chruby bundler inf-ruby csv-mode helm-themes helm-swoop helm-projectile helm-mode-manager helm-flx helm-descbinds helm-ag ace-jump-helm-line dtrt-indent yaml-mode company-web web-completion-data company-tern dash-functional company-statistics company-go company-anaconda company auto-yasnippet ac-ispell auto-complete web-mode web-beautify tern tagedit slim-mode scss-mode sass-mode pug-mode livid-mode skewer-mode simple-httpd less-css-mode json-mode json-snatcher json-reformat js2-refactor yasnippet multiple-cursors js2-mode js-doc haml-mode emmet-mode coffee-mode sql-indent flycheck-gometalinter go-guru go-eldoc go-mode flycheck-elm elm-mode flycheck-pos-tip pos-tip flycheck yapfify pyvenv pytest pyenv-mode py-isort pip-requirements live-py-mode hy-mode cython-mode anaconda-mode pythonic reveal-in-osx-finder pbcopy osx-trash osx-dictionary launchctl ws-butler window-numbering which-key wgrep volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline powerline smex restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox spinner org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint ivy-hydra info+ indent-guide ido-vertical-mode hydra hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation hide-comnt help-fns+ helm-make helm helm-core google-translate golden-ratio flx-ido flx fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu highlight elisp-slime-nav dumb-jump popup f s diminish define-word counsel-projectile projectile pkg-info epl counsel swiper ivy column-enforce-mode clean-aindent-mode bind-map bind-key auto-highlight-symbol auto-compile packed dash async aggressive-indent adaptive-wrap ace-window ace-link avy quelpa package-build spacemacs-theme)))

 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#cb4b16")
     (60 . "#b58900")
     (80 . "#859900")
     (100 . "#2aa198")
     (120 . "#268bd2")
     (140 . "#d33682")
     (160 . "#6c71c4")
     (180 . "#dc322f")
     (200 . "#cb4b16")
     (220 . "#b58900")
     (240 . "#859900")
     (260 . "#2aa198")
     (280 . "#268bd2")
     (300 . "#d33682")
     (320 . "#6c71c4")
     (340 . "#dc322f")
     (360 . "#cb4b16"))))
 '(vc-annotate-very-old-color nil)
 '(web-mode-code-indent-offset 2)
 '(web-mode-css-indent-offset 2)
 '(web-mode-markup-indent-offset 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(js2-error ((t nil)))
 '(js2-external-variable ((t nil))))
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ahs-idle-interval 0.75)
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#839496" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"))
 '(bibtex-completion-pdf-open-function (quote find-file))
 '(css-indent-offset 2)
 '(custom-safe-themes
   (quote
    ("4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" default)))
 '(dtrt-indent-max-lines 500)
 '(elm-format-on-save t)
 '(evil-want-Y-yank-to-eol nil)
 '(fci-rule-color "#073642" t)
 '(js-indent-level 2)
 '(js2-mode-show-parse-errors nil)
 '(js2-mode-show-strict-warnings nil)
 '(mail-envelope-from (quote header))
 '(mail-specify-envelope-from t)
 '(message-auto-save-directory "~/.mail/drafts/")
 '(message-kill-buffer-on-exit t)
 '(message-send-mail-function (quote message-send-mail-with-sendmail))
 '(message-sendmail-envelope-from (quote header))
 '(message-sendmail-f-is-evil nil)
 '(notdeft-directories (quote ("~/org/zettelkasten")))
 '(notmuch-always-prompt-for-sender t t)
 '(notmuch-archive-tags (quote ("+archived" "-inbox" "-unread")) t)
 '(notmuch-crypto-process-mime t t)
 '(notmuch-fcc-dirs
   (quote
    ((".*@wohanley.com" . "me/Sent")
     ("willy.ohanley@gmail.com" . "gmail/Sent")
     ("whohanley@uvic.ca" . "uvic/Sent"))) t)
 '(notmuch-hello-sections (quote (notmuch-hello-insert-saved-searches)) t)
 '(notmuch-message-headers (quote ("To" "Cc" "Subject" "Bcc")) t)
 '(notmuch-saved-searches
   (quote
    ((:name "inbox" :query "tag:inbox" :key "j" :search-type tree)
     (:name "unread" :query "tag:inbox and tag:unread" :key "u")
     (:name "spam" :query "tag:spam" :key "s")
     (:name "drafts" :query "tag:draft" :key "d"))) t)
 '(notmuch-search-oldest-first nil t)
 '(org-ref-completion-library (quote org-ref-ivy-cite))
 '(org-roam-directory "~/org/zettelkasten")
 '(package-selected-packages
   (quote
    (calfw-org calfw org-projectile org-category-capture org-present org-pomodoro alert log4e gntp org-mime org-download htmlize gnuplot org-roam winum git-gutter-fringe+ git-gutter-fringe fringe-helper git-gutter+ git-gutter fuzzy transient reformatter diff-hl clojure-snippets clj-refactor inflections paredit lv cider-eval-sexp-fu cider sesman queue parseedn clojure-mode parseclj a gh-md mmm-mode markdown-toc markdown-mode smeargle orgit org magit-gitflow gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link evil-magit magit magit-popup git-commit with-editor insert-shebang fish-mode company-shell rvm ruby-tools ruby-test-mode rubocop rspec-mode robe rbenv rake minitest chruby bundler inf-ruby csv-mode helm-themes helm-swoop helm-projectile helm-mode-manager helm-flx helm-descbinds helm-ag ace-jump-helm-line dtrt-indent yaml-mode company-web web-completion-data company-tern dash-functional company-statistics company-go company-anaconda company auto-yasnippet ac-ispell auto-complete web-mode web-beautify tern tagedit slim-mode scss-mode sass-mode pug-mode livid-mode skewer-mode simple-httpd less-css-mode json-mode json-snatcher json-reformat js2-refactor yasnippet multiple-cursors js2-mode js-doc haml-mode emmet-mode coffee-mode sql-indent flycheck-gometalinter go-guru go-eldoc go-mode flycheck-elm elm-mode flycheck-pos-tip pos-tip flycheck yapfify pyvenv pytest pyenv-mode py-isort pip-requirements live-py-mode hy-mode cython-mode anaconda-mode pythonic reveal-in-osx-finder pbcopy osx-trash osx-dictionary launchctl ws-butler window-numbering which-key wgrep volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline powerline smex restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox spinner org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint ivy-hydra info+ indent-guide ido-vertical-mode hydra hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation hide-comnt help-fns+ helm-make helm helm-core google-translate golden-ratio flx-ido flx fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu highlight elisp-slime-nav dumb-jump popup f s diminish define-word counsel-projectile projectile pkg-info epl counsel swiper ivy column-enforce-mode clean-aindent-mode bind-map bind-key auto-highlight-symbol auto-compile packed dash async aggressive-indent adaptive-wrap ace-window ace-link avy quelpa package-build spacemacs-theme)))
 '(sendmail-program "/home/wohanley/.nix-profile/bin/msmtp")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#cb4b16")
     (60 . "#b58900")
     (80 . "#859900")
     (100 . "#2aa198")
     (120 . "#268bd2")
     (140 . "#d33682")
     (160 . "#6c71c4")
     (180 . "#dc322f")
     (200 . "#cb4b16")
     (220 . "#b58900")
     (240 . "#859900")
     (260 . "#2aa198")
     (280 . "#268bd2")
     (300 . "#d33682")
     (320 . "#6c71c4")
     (340 . "#dc322f")
     (360 . "#cb4b16"))))
 '(vc-annotate-very-old-color nil)
 '(web-mode-code-indent-offset 2)
 '(web-mode-css-indent-offset 2)
 '(web-mode-markup-indent-offset 2)
 '(zotxt-default-bibliography-style "who-short" t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(js2-error ((t nil)))
 '(js2-external-variable ((t nil))))
)
