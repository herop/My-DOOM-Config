(setq custom-file null-device)
;;(custom-set-faces '(fixed-pitch ((t (:family "Iosevka"))))) ; or set it to nil

(map! :leader
      :desc "Dired"
      "d d" #'dired
      :leader
      :desc "Dired jump to current"
      "d j" #'dired-jump
      (:after dired
       (:map dired-mode-map
        :leader
        :desc "Peep-dired image previews"
        "d p" #'peep-dired
        :leader
        :desc "Dired view file"
        "d v" #'dired-view-file)))
;; Make 'h' and 'l' go back and forward in dired. Much faster to navigate the directory structure!
(evil-define-key 'normal dired-mode-map
  (kbd "h") 'dired-up-directory
  (kbd "l") 'dired-open-file) ; use dired-find-file instead if not using dired-open package
;; If peep-dired is enabled, you will get image previews as you go up/down with 'j' and 'k'
(evil-define-key 'normal peep-dired-mode-map
  (kbd "j") 'peep-dired-next-file
  (kbd "k") 'peep-dired-prev-file)
(add-hook 'peep-dired-hook 'evil-normalize-keymaps)
;; Get file icons in dired
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
;; With dired-open plugin, you can launch external programs for certain extensions
;; For example, I set all .png files to open in 'sxiv' and all .mp4 files to open in 'mpv'
(setq dired-open-extensions '(("gif" . "sxiv")
                              ("jpg" . "sxiv")
                              ("png" . "sxiv")
                              ("mkv" . "mpv")
                              ("mp4" . "mpv")))
(after! dired
  (setq dired-listing-switches "-agho --group-directories-first")
  (setq delete-by-moving-to-trash t))

(setq browse-url-browser-function 'eww-browse-url)
(map! :leader
      :desc "Eww web browser"
      "e w" #'eww
      :leader
      :desc "Eww reload page"
      "e R" #'eww-reload
      :leader
      :desc "Search web for text between BEG/END"
      "s w" #'eww-search-words)

(setq doom-font (font-spec :family "Fantasque Sans Mono" :size 18)
      doom-big-font (font-spec :family "Fantasque Sans Mono" :size 24)
      doom-variable-pitch-font (font-spec :family "Ubuntu" :size 16))
(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))

(use-package! emojify
  :config
  (when (member "Noto Color Emoji" (font-family-list))
    (set-fontset-font
     t 'symbol (font-spec :family "Noto Color Emoji") nil 'prepend))
  (setq emojify-display-style 'unicode)
  (setq emojify-emoji-styles '(unicode)))

(require 'ivy-posframe)
;; Global mode
;; display at `ivy-posframe-style'
(setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display)))
;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-window-center)))
(setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-bottom-left)))
;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-window-bottom-left)))
;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-top-center)))

(ivy-posframe-mode 1) ; 1 enables posframe-mode, 0 disables it.

(map! :leader
      (:prefix ("v" . "ivy-views")
      :desc "Ivy push view"
      "p" #'ivy-push-view
      :leader
      :desc "Ivy switch view"
      "s" #'ivy-switch-view))

(setq display-line-numbers-type t)
(map! :leader
      :desc "Toggle truncate lines"
      "t t" #'toggle-truncate-lines)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(after! neotree
  (setq neo-smart-open t
        neo-window-fixed-size nil))
(map! :leader
      :desc "Toggle neotree file viewer"
      "t n" #'toggle-neotree)

(after! org
        (require 'org-bullets)  ; Nicer bullets in org-mode
        (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
        (add-to-list 'auto-mode-alist '("\.\(org\|agenda_files\|txt\)$" . org-mode))
        (setq org-directory "~/kDrive/BC/Emacs/org/"
              +org-capture-journal-file "~/kDrive/BC/Emacs/org/journal.org"
              org-default-notes-file (expand-file-name "notes.org" org-directory)
              org-agenda-files (directory-files-recursively "~/kDrive/BC/Emacs/org/" "\\.org$")
              org-ellipsis " ▼ "
              org-log-done 'time
              org-journal-dir "~/kDrive/BC/Emacs/org/"
              org-startup-folded t
              org-journal-date-format "%B %d, %Y (%A)"
              org-journal-file-format "%Y-%m-%d"
              org-hide-emphasis-markers t
              ;; ex. of org-link-abbrev-alist in action
              ;; [[arch-wiki:Name_of_Page][Description]]
              org-link-abbrev-alist    ; This overwrites the default Doom org-link-abbrev-list
              '(("google" . "http://www.google.com/search?q=")
                ("arch-wiki" . "https://wiki.archlinux.org/index.php/")
                ("ddg" . "https://duckduckgo.com/?q=")
                ("wiki" . "https://en.wikipedia.org/wiki/"))
              org-todo-keywords        ; This overwrites the default Doom org-todo-keywords
              '((sequence
                 "TODO(t)"           ; A task that is ready to be tackled
                 "ONIT(o)"
                 "NEXT(n)"
                 "PROJ(p)"           ; A project that contains other tasks
                 "MEET(m)"
                 "WAIT(w@/!)"           ; Something is holding up this task
                 "|"                 ; The pipe necessary to separate "active" states and "inactive" states
                 "DONE(d)"           ; Task has been completed
                 "CANCL(c@/t)" )))) ; Task has been cancelled
  (map! :leader
        :desc "Insert Schedule"
        "m s" #'org-schedule
        :desc "Inactive Timestamp"
        "m !" #'org-timestamp-inactive)

(defun dt/org-babel-tangle-async (file)
  "Invoke `org-babel-tangle-file' asynchronously."
  (message "Tangling %s..." (buffer-file-name))
  (async-start
   (let ((args (list file)))
  `(lambda ()
        (require 'org)
        ;;(load "~/.emacs.d/init.el")
        (let ((start-time (current-time)))
          (apply #'org-babel-tangle-file ',args)
          (format "%.2f" (float-time (time-since start-time))))))
   (let ((message-string (format "Tangling %S completed after " file)))
     `(lambda (tangle-time)
        (message (concat ,message-string
                         (format "%s seconds" tangle-time)))))))

(defun dt/org-babel-tangle-current-buffer-async ()
  "Tangle current buffer asynchronously."
  (dt/org-babel-tangle-async (buffer-file-name)))

;;(define-key evil-normal-state-map "u" 'undo-fu-only-undo)
;;(define-key evil-normal-state-map "\C-r" 'undo-fu-only-redo)
;;(setq doom-modeline-major-mode-icon t)
;;(after! org
;;  (setq undo-outer-limit 24000000)
;;  ;; Open Links in new window
;;  (setq org-link-frame-setup '(
;;                               (vm . vm-visit-folder-other-frame)
;;                               (vm-imap . vm-visit-imap-folder-other-frame)
;;                               (file . find-file-other-window)
;;                               (wl . wl-other-frame))))

;; (setq doom-theme 'doom-dracula)
;; LEUVEN Theme by github.com/fniessen
;; Fontify code in blocks in code blocks
(setq doom-theme 'leuven)
(after! org
  (setq org-src-fontify-natively t)
  ;; Fontify the whole line for headings (with a background color).
  (setq org-fontify-whole-heading-line t)
  ;; Faces for specific TODO keywords.
  (setq org-todo-keyword-faces
        '(("WAIT" . leuven-org-waiting-for-kwd)))
  ;; Mode-line colors
  ;; Org non-standard faces.
  (defface leuven-org-waiting-for-kwd
    '((t :weight bold :box "#89C58F"
         :foreground "#89C58F" :background "#E2FEDE"))
    "Face used to display state WAIT.")
;;  (defface mode-line
;;    '((t (:box (:line-width 1 :color "#1A2F54") :foreground "green" :background "DarkOrange")))
;;    "Orange and green colors in mode-line.")
  )

(map! :leader
      :desc "Annotation-Mode"
      "t A" #'annotate-mode)
(map! :leader
 (:prefix ("A" . "Annotations")
      :desc "Annote me"
      "a" #'annotate-annotate
      :leader
      :desc "Refresh"
      "r" #'annotate-load-annotations))

(map! :leader
      :desc "XWidget Browser" "o x" #'xwidget-webkit-browse-url)

(map! :leader
      :desc "Olivetti-Mode"
      "t o" #'olivetti-mode
      :leader
      :desc "Olivetti Set Width"
      "b o" #'olivetti-set-width)
(add-hook 'text-mode-hook #'olivetti-mode)

(map! :leader
      :desc "Copy to register"
      "r c" #'copy-to-register
      :leader
      :desc "Frameset to register"
      "r f" #'frameset-to-register
      :leader
      :desc "Insert contents of register"
      "r i" #'insert-register
      :leader
      :desc "Jump to register"
      "r j" #'jump-to-register
      :leader
      :desc "List registers"
      "r l" #'list-registers
      :leader
      :desc "Number to register"
      "r n" #'number-to-register
      :leader
      :desc "Interactively choose a register"
      "r r" #'counsel-register
      :leader
      :desc "View a register"
      "r v" #'view-register
      :leader
      :desc "Window configuration to register"
      "r w" #'window-configuration-to-register
      :leader
      :desc "Increment register"
      "r +" #'increment-register
      :leader
      :desc "Point to register"
      "r SPC" #'point-to-register)

(defun prefer-horizontal-split ()
  (set-variable 'split-height-threshold nil t)
  (set-variable 'split-width-threshold 40 t)) ; make this as low as needed
(add-hook 'markdown-mode-hook 'prefer-horizontal-split)
(map! :leader
      :desc "Clone indirect buffer other window"
      "b c" #'clone-indirect-buffer-other-window)

(require 'sublimity-scroll)
(require 'sublimity-map)
(require 'sublimity-attractive)
(sublimity-mode 0)

(map! :leader
      :desc "Winner redo"
      "w <right>" #'winner-redo
      :leader
      :desc "Winner undo"
      "w <left>" #'winner-undo)

;; Preliminaries as seen in @jethro config.el
(use-package! org-roam
  :init
  (map! :leader
        :prefix "n"
        :desc "org-roam" "l" #'org-roam-buffer-toggle
        :desc "org-roam-node-insert" "i" #'org-roam-node-insert
        :desc "org-roam-dailies-find-date" "d" #'org-roam-dailies-find-date
        :desc "org-roam-ref-find" "r" #'org-roam-ref-find
        :desc "org-roam-dailies-find-today" "t" #'org-roam-dailies-find-today
        :desc "org-roam-dailies-find-yesterday" "y" #'org-roam-dailies-find-yesterday
        :desc "org-roam-dailies-capture-today" "j" #'org-roam-dailies-capture-today
        :desc "org-roam-node-find" "f" #'org-roam-node-find
        :desc "org-roam-capture" "c" #'org-roam-capture
        :desc "org-roam-unlinked-references" "u" #'org-roam-unlinked-references)
  (setq org-roam-directory "~/kDrive/BC/Emacs/org"
        org-roam-dailies-directory "daily/"
        org-roam-db-gc-threshold gc-cons-threshold
        org-id-link-to-org-use-id t
        org-roam-completion-everywhere t
        org-roam-node-display-template "${title:100} ${tags:50}")
  :config
  (org-roam-setup)
  (define-key org-roam-mode-map [mouse-1] #'org-roam-visit-thing)
  (add-to-list 'display-buffer-alist
               '(("\\*org\\*"
                  (display-buffer-in-direction)
                  (direction . right)
                  (window-width . 0.33)
                  (window-height . fit-window-to-buffer))))
(setq org-roam-mode-sections
       (list #'org-roam-backlinks-insert-section
             #'org-roam-reflinks-insert-section
             #'org-roam-unlinked-references-insert-section))
(setq org-roam-capture-templates
        '(("d" "default" plain
           "%?"
           :if-new (file+head "${slug}.org"
                              "#+TITLE: ${title}\n#+CREATED: %<%Y-%m-%d>\n#+ROAM_TAGS:\n#+ROAM_ALIASES:\n#+ROAM_REFS\n#+FILETAGS:\n")
           :immediate-finish t
           :unnarrowed t)))
      (setq org-roam-dailies-capture-templates
        '(("d" "default" entry
           "* %?"
           :if-new (file+head "%<%Y-%m-%d>.org"
                              "#+TITLE: %<%A, %e %B %Y>\n#+ROAM_TAGS:\n#+ROAM_ALIASES:\n#+ROAM_REFS\n#+FILETAGS:\n")))
        ))

;;(use-package org-roam-server
;;  :config
;;  (setq org-roam-server-host "127.0.0.1"
;;        org-roam-server-port 8080
;;        org-roam-server-export-inline-images t
;;        org-roam-server-authenticate nil
;;        org-roam-server-network-label-truncate t
;;        org-roam-server-network-label-truncate-length 60
;;        org-roam-server-network-label-wrap-length 20))

(use-package! org-journal
      :config
      (setq org-journal-dir "~/kDrive/BC/Emacs/org/"
      org-journal-date-prefix "#+TITLE: "
      org-journal-file-format "%Y-%m-%d.org"
      org-journal-date-format "%A, %d %B %Y"
    org-journal-enable-agenda-integration t))

;;(use-package! languagetool
  (map! :leader
        (:prefix ("l" . "languagecheck")
        :desc "Check with transient" "c" #'languagetool-check
        :leader
        :desc "Clear Buffer" "b" #'languagetool-clear-buffer
        :leader
        :desc "Check at point" "p" #'languagetool-correct-at-point
        :leader
        :desc "Correct buffer" "C" #'languagetool-correct-buffer
        :leader
        :desc "Set language" "s" #'languagetool-set-language))
  :config
  (setq languagetool-language-tool-jar "/home/jburkhard/.languagetool/languagetool-commandline.jar"
    ;; languagetool-server-language-tool-jar "/usr/share/java/languagetool/languagetool-server.jar"
        languagetool-java-arguments '("-Dfile.encoding=UTF-8")
        languagetool-default-language "en-US"
        languagetool-java-bin "/usr/bin/java")

(setq deft-directory "~/kDrive/BC/Emacs/org/")
(map! :leader
      :prefix "n"
      :desc "Open deft"
      "D" #'deft)

(map! :leader
      :desc "ERC"
      "o i" #'erc-tls)
(setq erc-server "irc.libera.chat"
      erc-nick "HeroP"
      erc-user-full-name "Jochen"
      erc-track-shorten-start 8
      erc-autojoin-chennels-alist '(("irc-libera-chat" "#systemcrafters" "#emacs"))
      erc-kill-buffer-on-part t
      erc-aut-query 'bury)

(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(set-frame-parameter (selected-frame) 'alpha '(95 50))
