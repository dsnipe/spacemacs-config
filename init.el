;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-configuration-layer-path '("~/.spacemacs.d/private")
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-ask-for-lazy-installation t
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers
   '(
     sql
     helm
     ;; ivy
     better-defaults
     osx
     dash
     colors
     themes-megapack
     theming
     spotify
     (ibuffer :variables
              ibuffer-group-buffers-by 'projects)
     imenu-list
     gtags ;; gtags --gtagslabel=ctags or pygments (for elixir)
     (auto-completion :variables
                      auto-completion-tab-key-behavior 'complete
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-enable-sort-by-usage t)
     emacs-lisp
     git
     (version-control :variables
                      version-control-diff-tool 'diff-hl)
     restclient
     syntax-checking
     spacemacs-layouts
     (org :variables
          org-enable-github-support t)
     ;; org-extra
     deft
     github
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom
            shell-default-shell 'eshell)
     (ranger :variables
             ranger-show-preview t)
     (spell-checking :variables spell-checking-enable-by-default nil)
     (ruby :variables
           ruby-version-manager 'rbenv
           ruby-test-runner 'rspec)
     yaml
     markdown
     ruby-on-rails
     javascript
     react
     elixir
     erlang
     ;; java
     c-c++
     )
   dotspacemacs-additional-packages
   '(
     flx
     ;; helm-flx
     ;; helm-fuzzier
     minitest
     org-projectile
     counsel-projectile
     ace-mc
    )
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '(org-repo-todo)
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'. (default t)
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   dotspacemacs-editing-style '(hybrid :variables
                                       hybrid-mode-enable-evilified-state t
                                       hybrid-mode-enable-hjkl-bindings t
                                       hybrid-mode-default-state 'normal)
   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 5
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; `official', `random' chooses a random text banner in `core/banners'
   dotspacemacs-startup-banner nil
   dotspacemacs-startup-lists '(projects bookmarks recents)
   dotspacemacs-themes '(atom-one-dark
                         spacemacs-light
                         atom-dark
                         seti
                         colorsarenice-light
                         smyx
                         monokai
                         lush
                         ample
                         flatland
                         spacegray
                         spacemacs-dark
                         monokai
                         zenburn)
   ;; If non nil the cursor color matches the state color.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 14
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m)
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-command-key ":"
   dotspacemacs-remap-Y-to-y$ t
   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 2
   ;;Possible values are `original', `cache' or `nil' to disable auto-saving.
   dotspacemacs-auto-save-file-location 'cache
   ;; If non nil then `ido' replaces `helm' for some commands. For now only
   ;; `find-files' (SPC f f), `find-spacemacs-file' (SPC f e s), and
   ;; `find-contrib-file' (SPC f e c) are replaced. (default nil)
   dotspacemacs-use-ido nil
   ;; If non nil, `helm' will try to miminimize the space it uses. (default nil)
   dotspacemacs-helm-resize t
   dotspacemacs-helm-no-header t
   dotspacemacs-helm-position 'bottom
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-micro-state t
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   dotspacemacs-loading-progress-bar nil
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup t
   dotspacemacs-active-transparency 50
   dotspacemacs-inactive-transparency 90
   ;; If non nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title nil
   ;; If non nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   dotspacemacs-smooth-scrolling t
   dotspacemacs-smartparens-strict-mode nil
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-persistent-server t
   dotspacemacs-search-tools '("ag"  "ack" "grep")
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'changed
   ;; If non-nil, `J' and `K' move lines up and down when in visual mode. Default value is `nil'.
   dotspacemacs-visual-line-move-text t
   ;; Allow choosing between different code folding methods. Currently supported are evil and origami.
   ;; Default value is evil
   dotspacemacs-folding-method 'origami
   ))

(defun dotspacemacs/user-init ()
  (setq ns-use-srgb-colorspace nil) ;; fixes bad colors in separator in the powerline
  (setq paradox-github-token "")
  (setq-default evil-escape-key-sequence "kj")
  ;; Frontend configs
  (setq-default
   js2-basic-offset 2
   js2-missing-semi-one-line-override t
   js2-mode-show-strict-warnings nil
   js2-mode-show-parse-errors nil
   js-indent-level 2
   jsx-indent-level 2
   css-indent-offset 2
   web-mode-markup-indent-offset 2
   web-mode-css-indent-offset 2
   web-mode-code-indent-offset 2
   web-mode-attr-indent-offset 2)
  ;; Intendation for JSX
  (with-eval-after-load 'web-mode
    (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
    (add-to-list 'web-mode-indentation-params '("lineup-contacts" . nil))
    (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil)))
  (defvar evil-mc-mode-line-prefix "ⓜ"
    "Override of the default mode line string for `evil-mc-mode'.")
  )

(defun dotspacemacs/user-config ()
  ;; (helm-fuzzier-mode 1)
  ;; (golden-ratio-mode 1)
  ;; (setq golden-ratio-auto-scale t)
  (add-hook 'ruby-mode-hook 'minitest-mode)
  (eval-after-load 'minitest
    '(minitest-install-snippets))
  (global-set-key (kbd "C-<return>") 'spacemacs/insert-line-below-no-indent)
  (global-set-key (kbd "C-S-<return>") 'spacemacs/insert-line-above-no-indent)

  (spacemacs/set-leader-keys
    "SPC" 'evil-avy-goto-word-or-subword-1)

  (diff-hl-flydiff-mode 1)
  (setq ruby-insert-encoding-magic-comment nil)

  (setq deft-directory "~/Dropbox/org/notes")
  (setq deft-use-filename-as-title t)
  (setq deft-use-filter-string-for-filename t)
  (setq deft-auto-save-interval 0)
  (spacemacs/set-leader-keys-for-major-mode 'deft-mode
    "q" 'quit-window)
  (defun deft-absolute-filename (slug &optional extension)
    "Return an absolute filename to file named SLUG with optional EXTENSION.
If EXTENSION is not given, `deft-extension' is assumed.
If SLUG has spaces, replace them with underscores.
Also force the file name to be all lower case."
    (let* ((slug-no-space (replace-regexp-in-string
                           "\\(.*?\\) *\\'" "\\1"
                           slug)) ; remove trailing spaces if any
           (slug-no-space (replace-regexp-in-string
                           " " "_"
                           slug-no-space)) ; replace spaces with _
           (slug-no-space (downcase slug-no-space))) ; lower case
      (concat (file-name-as-directory (expand-file-name deft-directory))
              slug-no-space
              "." (or extension deft-extension))))

  ;; Some additions for themes
  (setq theming-modifications
        '((smyx (company-tooltip-annotation-selection :inherit company-tooltip-selection))))
  (setq theming-modifications
        '((seti (company-tooltip-annotation-selection :inherit company-tooltip-selection)
                (default ((t (:background "#20272e")))))))
  (setq theming-modifications
        '((atom-one-dark (trailing-whitespace :background "#6c6c6c"))))
  (spacemacs/update-theme)

  ;; Evil Multicursor
  (add-hook 'prog-mode-hook 'turn-on-evil-mc-mode)
  (add-hook 'text-mode-hook 'turn-on-evil-mc-mode)

  ;; Ivy Counsel settings
  ;; (defun ivy-other-window ()
  ;;   "Specific action to open in other window"
  ;;   (interactive)
  ;;   (execute-kbd-macro "\M-oj")
  ;;   (other-window))
  ;; ;; Make fuzzy search by default (except buffer list)
  ;; (setq ivy-re-builders-alist
  ;;       '((ivy-switch-buffer . ivy--regex-plus)
  ;;         (t . ivy--regex-fuzzy)))
  ;; (spacemacs/set-leader-keys
  ;;   ;; "pl" 'counsel-projectile
  ;;   "pf" 'counsel-projectile-find-file
  ;;   "pb" 'counsel-projectile-switch-to-buffer)
  ;; (define-key ivy-minibuffer-map (kbd "C-o") 'ivy-other-window)

  ;; My keybindings
  (define-key evil-mode (kbd "C-,") 'ggtags-find-definition)

  ;; TODO: Doesn't work actually, will need to find a solution
  (spacemacs/toggle-visual-line-navigation-on)
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#282828" "#FAB1AB" "#D1FA71" "#FFA600" "#7b68ee" "#dc8cc3" "#96D9F1" "#F7F7F7"])
 '(custom-safe-themes
   (quote
    ("8288b9b453cdd2398339a9fd0cec94105bc5ca79b86695bd7bf0381b1fbe8147" default)))
 '(evil-want-Y-yank-to-eol t)
 '(fci-rule-color "#151515" t)
 '(paradox-automatically-star t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-annotation-selection ((t (:inherit company-tooltip-selection))))
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil))))
 '(trailing-whitespace ((t (:background "#6c6c6c")))))
