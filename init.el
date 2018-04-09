;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
(setq-default
   dotspacemacs-distribution 'spacemacs
   ;; dotspacemacs-configuration-layer-path '("~/.spacemacs.d/private")
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-ask-for-lazy-installation t
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers
   '(
     python
     csv
      ;; clojure
      ;; java
      ;; c-c++
      treemacs
      terraform
      sql
      helm
      better-defaults
      osx
      dash
      docker
      colors
      themes-megapack
      theming
      (ibuffer :variables
               ibuffer-group-buffers-by 'projects)
      imenu-list
      gtags ;; gtags --gtagslabel=ctags or pygments (for elixir)
      (auto-completion :variables
                       auto-completion-tab-key-behavior 'cycle
                       auto-completion-complete-with-key-sequence "jj"
                       auto-completion-enable-snippets-in-popup t,
                       auto-completion-enable-help-tooltip 'manual,
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
      (markdown :variables markdown-live-preview-engine 'vmd)
      ruby-on-rails
      javascript
      react
      elixir
      erlang
      )
    dotspacemacs-additional-packages
    '(
      ;; flx
      ;; helm-flx
      ;; helm-fuzzier
      evil-multiedit
      minitest
      ;; company-childframe
      ;; helm-ext
      )
    ;; A list of packages and/or extensions that will not be install and loaded.
    dotspacemacs-excluded-packages '(org-repo-todo)
    ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
    ;; are declared in a layer which is not a member of
    ;; the list `dotspacemacs-configuration-layers'. (default t)
    ))
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
   dotspacemacs-verbose-loading t
   ;; `official', `random' chooses a random text banner in `core/banners'
   dotspacemacs-startup-banner nil
   dotspacemacs-startup-lists '(projects bookmarks recents)
   dotspacemacs-themes '(monokai
                         spacemacs-light
                         atom-one-dark
                         smyx
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
   ;; ;; start. (default nil)
   ;; dotspacemacs-auto-resume-layouts nil
   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 2
   ;;Possible values are `original', `cache' or `nil' to disable auto-saving.
   dotspacemacs-auto-save-file-location 'cache
   ;; If non nil then `ido' replaces `helm' for some commands. For now only
   ;; `find-files' (SPC f f), `find-spacemacs-file' (SPC f e s), and
   ;; `find-contrib-file' (SPC f e c) are replaced. (default nil)
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
   dotspacemacs-switch-to-buffer-prefers-purpose nil
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
   dotspacemacs-mode-line-unicode-symbols nil
   dotspacemacs-smooth-scrolling t
   dotspacemacs-smartparens-strict-mode nil
   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers '(:relative t
                               :disabled-for-modes dired-mode
                                                   doc-view-mode
                                                   markdown-mode
                                                   org-mode
                                                   pdf-view-mode
                                                   text-mode

                                                   :size-limit-kb 1000)
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis t
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
  ;; (push '("melpa-stable" . "stable.melpa.org/packages/") configuration-layer-elpa-archives)
  ;; (push '(use-package . "melpa-stable") package-pinned-packages)


  (setq ns-use-srgb-colorspace nil) ;; fixes bad colors in separator in the powerline
  (setq paradox-github-token "")
  (setq rspec-use-chruby t) ;; use ruby from chruby for RSpecs
  (setq-default evil-escape-key-sequence "kj")
  ;; Frontend configs
  (setq-default
   js2-basic-offset 2
   js2-missing-semi-one-line-override t
   js2-mode-show-strict-warnings nil
   js2-mode-show-parse-errors nil
   js-indent-level 2
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
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "KIEX_HOME")
  (exec-path-from-shell-copy-env "MIX_ARCHIVES")

  (spacemacs/toggle-mode-line-minor-modes-off)
  (spacemacs/toggle-visual-line-navigation-on)
  (display-time)
  ;; (company-childframe-mode t)

  (add-hook 'ruby-mode-hook 'minitest-mode)

  (eval-after-load 'minitest
    '(minitest-install-snippets))

  (global-set-key (kbd "C-<return>") 'spacemacs/insert-line-below-no-indent)
  (global-set-key (kbd "C-S-<return>") 'spacemacs/insert-line-above-no-indent)

  (spacemacs/set-leader-keys
    "SPC" 'evil-avy-goto-word-or-subword-1)

  (setq ruby-insert-encoding-magic-comment nil)

  ;; Deft settings
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

  ;; helm ext
  ;; (helm-ext-ff-enable-skipping-dots t)
  ;; (setq helm-ext-ff-skipping-dots-recenter t)
  ;; (helm-ext-ff-enable-zsh-path-expansion t)
  ;; (helm-ext-ff-enable-auto-path-expansion t)
  ;; (helm-ext-minibuffer-enable-header-line-maybe t)

  ;; Markdown mode
  (setq markdown-open-command "/usr/local/bin/mark")


  ;; Highlights all matches of the selection in the buffer.
  (define-key evil-visual-state-map "R" 'evil-multiedit-match-all)
  ;; Match the word under cursor (i.e. make it an edit region). Consecutive presses will
  ;; incrementally add the next unmatched match.
  (define-key evil-normal-state-map (kbd-mac-command "d") 'evil-multiedit-match-symbol-and-next)
  ;; Match selected region.
  (define-key evil-visual-state-map (kbd-mac-command "d") 'evil-multiedit-match-symbol-and-next)
  ;; Same as M-d but in reverse.
  ;; (global-set-key (kbd-mac-command "1") 'winum-select-window-1)
  (define-key evil-normal-state-map (kbd-mac-command "D") 'evil-multiedit-match-symbol-and-prev)
  (define-key evil-visual-state-map (kbd-mac-command "D") 'evil-multiedit-match-symbol-and-prev)
  ;; OPTIONAL: If you prefer to grab symbols rather than words, use
  ;; `evil-multiedit-match-symbol-and-next` (or prev).
  ;; Restore the last group of multiedit regions.
  (define-key evil-visual-state-map (kbd-mac-command "C-d") 'evil-multiedit-restore)
  ;; RET will toggle the region under the cursor
  ;; (define-key evil-multiedit-state-map (kbd "RET") 'evil-multiedit-toggle-or-restrict-region)
  ;; ...and in visual mode, RET will disable all fields outside the selected region
  (define-key evil-motion-state-map (kbd "RET") 'evil-multiedit-toggle-or-restrict-region)
  ;; For moving between edit regions
  ;; (define-key evil-multiedit-state-map (kbd "C-n") 'evil-multiedit-next)
  ;; (define-key evil-multiedit-state-map (kbd "C-p") 'evil-multiedit-prev)
  ;; (define-key evil-multiedit-insert-state-map (kbd "C-n") 'evil-multiedit-next)
  ;; (define-key evil-multiedit-insert-state-map (kbd "C-p") 'evil-multiedit-prev)

  ;; Ex command that allows you to invoke evil-multiedit with a regular expression, e.g.
  (evil-ex-define-cmd "ie[dit]" 'evil-multiedit-ex-match)

  (defface spacemacs-multiedit-face
    '((((class color) (min-colors 88) (background light))
       :background "darkseagreen2")
      (((class color) (min-colors 88) (background dark))
       :background "darkolivegreen")
      (((class color) (min-colors 16) (background light))
       :background "darkseagreen2")
      (((class color) (min-colors 16) (background dark))
       :background "darkolivegreen")
      (((class color) (min-colors 8))
       :background "green" :foreground "black")
      (t :inverse-video t))
    "Basic face for highlighting."
    :group 'basic-faces)

  ;; My keybindings
  ;; (define-key evil-mode (kbd "C-,") 'ggtags-find-definition)
  )
;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#bcbcbc" "#d70008" "#5faf00" "#875f00" "#268bd2" "#800080" "#008080" "#5f5f87"])
 '(cursor-type (quote bar))
 '(custom-safe-themes
   (quote
    ("8288b9b453cdd2398339a9fd0cec94105bc5ca79b86695bd7bf0381b1fbe8147" default)))
 '(evil-want-Y-yank-to-eol t)
 '(fci-rule-color "#151515" t)
 '(package-selected-packages
   (quote
    (colorsarenice-theme org packed elixir-mode auto-complete simple-httpd pcache alert haml-mode powerline rake inflections spinner markdown-mode hydra autothemer bind-key tern company smartparens bind-map iedit highlight flycheck request helm helm-core projectile yasnippet skewer-mode js2-mode gh magit magit-popup git-commit with-editor inf-ruby dash pcre2el go-guru go-eldoc company-go go-mode evil-multiedit yapfify pyvenv pytest pyenv-mode py-isort pip-requirements live-py-mode hy-mode helm-pydoc cython-mode company-anaconda anaconda-mode pythonic zonokai-theme zenburn-theme zen-and-art-theme yaml-mode xterm-color ws-butler window-numbering which-key web-mode web-beautify volatile-highlights vi-tilde-fringe uuidgen use-package underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme tronesque-theme toxi-theme toc-org tao-theme tangotango-theme tango-plus-theme tango-2-theme tagedit sunny-day-theme sublime-themes subatomic256-theme subatomic-theme sql-indent spotify spacemacs-theme spaceline spacegray-theme soothe-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme smeargle slim-mode shell-pop seti-theme scss-mode sass-mode rvm ruby-tools ruby-test-mode rubocop rspec-mode robe reverse-theme reveal-in-osx-finder restclient restart-emacs rbenv ranger rainbow-mode rainbow-identifiers rainbow-delimiters railscasts-theme quelpa purple-haze-theme pug-mode projectile-rails professional-theme popwin planet-theme phoenix-dark-pink-theme phoenix-dark-mono-theme persp-mode pbcopy pastels-on-dark-theme paradox ox-gfm osx-trash osx-dictionary origami orgit organic-green-theme org-projectile org-present org-pomodoro org-plus-contrib org-download org-bullets open-junk-file omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme ob-http ob-elixir noctilux-theme niflheim-theme neotree naquadah-theme mwim mustang-theme multi-term move-text monokai-theme monochrome-theme molokai-theme moe-theme mmm-mode minitest minimal-theme material-theme markdown-toc majapahit-theme magit-gitflow magit-gh-pulls macrostep lush-theme lorem-ipsum livid-mode linum-relative link-hint light-soap-theme less-css-mode launchctl json-mode js2-refactor js-doc jbeans-theme jazz-theme ir-black-theme inkpot-theme info+ indent-guide imenu-list ido-vertical-mode ibuffer-projectile hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers highlight-indentation hide-comnt heroku-theme hemisu-theme help-fns+ helm-themes helm-swoop helm-spotify helm-projectile helm-mode-manager helm-make helm-gtags helm-gitignore helm-flx helm-descbinds helm-dash helm-css-scss helm-company helm-c-yasnippet helm-ag hc-zenburn-theme gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme google-translate golden-ratio gnuplot github-search github-clone github-browse-file gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gist gh-md ggtags gandalf-theme flyspell-correct-helm flycheck-pos-tip flycheck-mix flx-ido flatui-theme flatland-theme firebelly-theme fill-column-indicator feature-mode farmhouse-theme fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu espresso-theme eshell-z eshell-prompt-extras esh-help erlang emmet-mode elisp-slime-nav dumb-jump dracula-theme django-theme disaster diff-hl deft dash-at-point darktooth-theme darkokai-theme darkmine-theme darkburn-theme dakrone-theme cyberpunk-theme counsel-projectile company-web company-tern company-statistics company-c-headers column-enforce-mode color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized color-identifiers-mode coffee-mode cmake-mode clues-theme clean-aindent-mode clang-format chruby cherry-blossom-theme busybee-theme bundler bubbleberry-theme birds-of-paradise-plus-theme badwolf-theme auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile atom-one-dark-theme atom-dark-theme apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme alect-themes alchemist aggressive-indent afternoon-theme adaptive-wrap ace-window ace-mc ace-link ace-jump-helm-line ac-ispell)))
 '(paradox-automatically-star t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil))))
 '(company-tooltip-annotation-selection ((t (:inherit company-tooltip-selection))))
 '(trailing-whitespace ((t (:background "#6c6c6c")))))
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
 '(cursor-type 'bar)
 '(custom-safe-themes
   '("8288b9b453cdd2398339a9fd0cec94105bc5ca79b86695bd7bf0381b1fbe8147" default))
 '(evil-want-Y-yank-to-eol t)
 '(fci-rule-color "#151515" t)
 '(package-selected-packages
   '(yasnippet-snippets winum white-sand-theme vmd-mode unfill treemacs-projectile treemacs-evil treemacs pfuture symon string-inflection spaceline-all-the-icons all-the-icons memoize solarized-theme ruby-refactor ruby-hash-syntax restclient-helm rebecca-theme pippel pipenv password-generator overseer org-category-capture log4e gntp org-mime org-brain ob-restclient nameless madhat2r-theme multiple-cursors importmagic epc ctable concurrent deferred impatient-mode parent-mode helm-xref helm-purpose window-purpose gitignore-mode fringe-helper git-gutter+ git-gutter marshal logito ht fuzzy flyspell-correct pos-tip flycheck-credo flx exotica-theme evil-org ghub evil-lion evil-cleverparens paredit anzu editorconfig dockerfile-mode docker tablist docker-tramp json-snatcher json-reformat csv-mode counsel swiper ivy web-completion-data company-terraform terraform-mode hcl-mode dash-functional company-restclient know-your-http-well company-childframe posframe centered-cursor-mode browse-at-remote f s pkg-info epl avy popup font-lock+ evil goto-chg undo-tree diminish async colorsarenice-theme org packed elixir-mode auto-complete simple-httpd pcache alert haml-mode powerline rake inflections spinner markdown-mode hydra autothemer bind-key tern company smartparens bind-map iedit highlight flycheck request helm helm-core projectile yasnippet skewer-mode js2-mode gh magit magit-popup git-commit with-editor inf-ruby dash pcre2el go-guru go-eldoc company-go go-mode evil-multiedit yapfify pyvenv pytest pyenv-mode py-isort pip-requirements live-py-mode hy-mode helm-pydoc cython-mode company-anaconda anaconda-mode pythonic zonokai-theme zenburn-theme zen-and-art-theme yaml-mode xterm-color ws-butler window-numbering which-key web-mode web-beautify volatile-highlights vi-tilde-fringe uuidgen use-package underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme tronesque-theme toxi-theme toc-org tao-theme tangotango-theme tango-plus-theme tango-2-theme tagedit sunny-day-theme sublime-themes subatomic256-theme subatomic-theme sql-indent spotify spacemacs-theme spaceline spacegray-theme soothe-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme smeargle slim-mode shell-pop seti-theme scss-mode sass-mode rvm ruby-tools ruby-test-mode rubocop rspec-mode robe reverse-theme reveal-in-osx-finder restclient restart-emacs rbenv ranger rainbow-mode rainbow-identifiers rainbow-delimiters railscasts-theme quelpa purple-haze-theme pug-mode projectile-rails professional-theme popwin planet-theme phoenix-dark-pink-theme phoenix-dark-mono-theme persp-mode pbcopy pastels-on-dark-theme paradox ox-gfm osx-trash osx-dictionary origami orgit organic-green-theme org-projectile org-present org-pomodoro org-plus-contrib org-download org-bullets open-junk-file omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme ob-http ob-elixir noctilux-theme niflheim-theme neotree naquadah-theme mwim mustang-theme multi-term move-text monokai-theme monochrome-theme molokai-theme moe-theme mmm-mode minitest minimal-theme material-theme markdown-toc majapahit-theme magit-gitflow magit-gh-pulls macrostep lush-theme lorem-ipsum livid-mode linum-relative link-hint light-soap-theme less-css-mode launchctl json-mode js2-refactor js-doc jbeans-theme jazz-theme ir-black-theme inkpot-theme info+ indent-guide imenu-list ido-vertical-mode ibuffer-projectile hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers highlight-indentation hide-comnt heroku-theme hemisu-theme help-fns+ helm-themes helm-swoop helm-spotify helm-projectile helm-mode-manager helm-make helm-gtags helm-gitignore helm-flx helm-descbinds helm-dash helm-css-scss helm-company helm-c-yasnippet helm-ag hc-zenburn-theme gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme google-translate golden-ratio gnuplot github-search github-clone github-browse-file gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gist gh-md ggtags gandalf-theme flyspell-correct-helm flycheck-pos-tip flycheck-mix flx-ido flatui-theme flatland-theme firebelly-theme fill-column-indicator feature-mode farmhouse-theme fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu espresso-theme eshell-z eshell-prompt-extras esh-help erlang emmet-mode elisp-slime-nav dumb-jump dracula-theme django-theme disaster diff-hl deft dash-at-point darktooth-theme darkokai-theme darkmine-theme darkburn-theme dakrone-theme cyberpunk-theme counsel-projectile company-web company-tern company-statistics company-c-headers column-enforce-mode color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized color-identifiers-mode coffee-mode cmake-mode clues-theme clean-aindent-mode clang-format chruby cherry-blossom-theme busybee-theme bundler bubbleberry-theme birds-of-paradise-plus-theme badwolf-theme auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile atom-one-dark-theme atom-dark-theme apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme alect-themes alchemist aggressive-indent afternoon-theme adaptive-wrap ace-window ace-mc ace-link ace-jump-helm-line ac-ispell))
 '(paradox-automatically-star t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-annotation-selection ((t (:inherit company-tooltip-selection)))))
)
