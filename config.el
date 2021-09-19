;; Toggle Calibre
(map! :leader
      :desc "CalibreDB"
      "t C" #'calibredb)
;; Toggle buffer follow mode
(map! :leader
      :desc "Follow Mode"
      "b f" #'follow-mode)
(setq custom-file null-device
      delete-by-moving-to-trash t
      undo-limit 80000000
      evil-want-fine-undo t
      auto-save-default t
      scroll-margin 2)
      (display-time-mode 1)
;;      (unless (string-match-p "^Power N/A" (battery))
      (display-battery-mode 1)
;; Window and buffer management
(setq evil-vsplit-window-right t
      evil-split-window-below t)
(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (consult-buffer))
(setq-default
       ;; Take window space from all other windows
      window-combination-resize t
      x-stretch-cursor t)
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
  )

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

(setq emojify-emoji-set "twemoji-v2")
(defvar emojify-disabled-emojis
  '(;; Org
    "◼" "☑" "☸" "⚙" "⏩" "⏪" "⬆" "⬇" "❓"
    ;; Terminal powerline
    "✔"
    ;; Box drawing
    "▶" "◀")
  "Characters that should never be affected by `emojify-mode'.")
(defadvice! emojify-delete-from-data ()
  "Ensure `emojify-disabled-emojis' don't appear in `emojify-emojis'."
  :after #'emojify-set-emoji-data
  (dolist (emoji emojify-disabled-emojis)
    (remhash emoji emojify-emojis)))
(defun emojify--replace-text-with-emoji (orig-fn emoji text buffer start end &optional target)
  "Modify `emojify--propertize-text-for-emoji' to replace ascii/github emoticons with unicode emojis, on the fly."
  (if (or (not emoticon-to-emoji) (= 1 (length text)))
      (funcall orig-fn emoji text buffer start end target)
    (delete-region start end)
    (insert (ht-get emoji "unicode"))))

(define-minor-mode emoticon-to-emoji
  "Write ascii/gh emojis, and have them converted to unicode live."
  :global nil
  :init-value nil
  (if emoticon-to-emoji
      (progn
        (setq-local emojify-emoji-styles '(ascii github unicode))
        (advice-add 'emojify--propertize-text-for-emoji :around #'emojify--replace-text-with-emoji)
        (unless emojify-mode
          (emojify-turn-on-emojify-mode)))
    (setq-local emojify-emoji-styles (default-value 'emojify-emoji-styles))
    (advice-remove 'emojify--propertize-text-for-emoji #'emojify--replace-text-with-emoji)))

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

(setq display-line-numbers-type 'relative)
(map! :leader
      :desc "Toggle truncate lines"
      "t t" #'toggle-truncate-lines)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(after! org
        (require 'org-bullets)  ; Nicer bullets in org-mode
        (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
        (add-to-list 'auto-mode-alist '("\.\(org\|agenda_files\|txt\)$" . org-mode))
        (setq org-directory "~/kDrive/BC/Emacs/org/"
              +org-capture-journal-file "~/kDrive/BC/Emacs/org/journal.org"
              org-default-notes-file (expand-file-name "notes.org" org-directory)
              org-agenda-files (directory-files-recursively "~/kDrive/BC/Emacs/org/" "\\.org$")
              org-refile-allow-creating-parent-nodes 'confirm
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
                 "CANCL(c@/!)" )))) ; Task has been cancelled
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

(map! :map evil-org-mode-map
      :after evil-org
      :n "g k" #'org-previous-visible-heading
      :n "g j" #'org-next-visible-heading)

(after! org-capture
  (defun +doct-icon-declaration-to-icon (declaration)
    "Convert :icon declaration to icon"
    (let ((name (pop declaration))
          (set  (intern (concat "all-the-icons-" (plist-get declaration :set))))
          (face (intern (concat "all-the-icons-" (plist-get declaration :color))))
          (v-adjust (or (plist-get declaration :v-adjust) 0.01)))
      (apply set `(,name :face ,face :v-adjust ,v-adjust))))

  (defun +doct-iconify-capture-templates (groups)
    "Add declaration's :icon to each template group in GROUPS."
    (let ((templates (doct-flatten-lists-in groups)))
      (setq doct-templates (mapcar (lambda (template)
                                     (when-let* ((props (nthcdr (if (= (length template) 4) 2 5) template))
                                                 (spec (plist-get (plist-get props :doct) :icon)))
                                       (setf (nth 1 template) (concat (+doct-icon-declaration-to-icon spec)
                                                                      "\t"
                                                                      (nth 1 template))))
                                     template)
                                   templates))))

  (setq doct-after-conversion-functions '(+doct-iconify-capture-templates))

  (defvar +org-capture-recipies  "~/kDrive/BC/Emacs/org/recipies.org")

  (defun set-org-capture-templates ()
    (setq org-capture-templates
          (doct `(("Personal todo" :keys "t"
                   :icon ("checklist" :set "octicon" :color "green")
                   :file +org-capture-todo-file
                   :prepend t
                   :headline "Inbox"
                   :type entry
                   :template ("* TODO %?"
                              "%i %a")
                   )
                  ("Personal note" :keys "n"
                   :icon ("sticky-note-o" :set "faicon" :color "green")
                   :file +org-capture-todo-file
                   :prepend t
                   :headline "Inbox"
                   :type entry
                   :template ("* %?"
                              "%i %a"))
                  ("Email" :keys "e"
                   :icon ("envelope" :set "faicon" :color "blue")
                   :file +org-capture-todo-file
                   :prepend t
                   :headline "Inbox"
                   :type entry
                   :template ("* TODO %^{type|reply to|contact} %\\3 %? :email:"
                              "Send an email %^{urgancy|soon|ASAP|anon|at some point|eventually} to %^{recipiant}"
                              "about %^{topic}"
                              "%U %i %a"))
                  ("Interesting" :keys "i"
                   :icon ("eye" :set "faicon" :color "lcyan")
                   :file +org-capture-todo-file
                   :prepend t
                   :headline "Interesting"
                   :type entry
                   :template ("* [ ] %{desc}%? :%{i-type}:"
                              "%i %a")
                   :children (("Webpage" :keys "w"
                               :icon ("globe" :set "faicon" :color "green")
                               :desc "%(org-cliplink-capture) "
                               :i-type "read:web"
                               )
                              ("Article" :keys "a"
                               :icon ("file-text" :set "octicon" :color "yellow")
                               :desc ""
                               :i-type "read:reaserch"
                               )
                              ("\tRecipie" :keys "r"
                               :icon ("spoon" :set "faicon" :color "dorange")
                               :file +org-capture-recipies
                               :headline "Unsorted"
                               :template "%(org-chef-get-recipe-from-url)"
                               )
                              ("Information" :keys "i"
                               :icon ("info-circle" :set "faicon" :color "blue")
                               :desc ""
                               :i-type "read:info"
                               )
                              ("Idea" :keys "I"
                               :icon ("bubble_chart" :set "material" :color "silver")
                               :desc ""
                               :i-type "idea"
                               )))
                  ("Tasks" :keys "k"
                   :icon ("inbox" :set "octicon" :color "yellow")
                   :file +org-capture-todo-file
                   :prepend t
                   :headline "Tasks"
                   :type entry
                   :template ("* TODO %? %^%{extra}"
                              "%i %a")
                   :children (("General Task" :keys "k"
                               :icon ("inbox" :set "octicon" :color "yellow")
                               :extra ""
                               )
                              ("Task with deadline" :keys "d"
                               :icon ("timer" :set "material" :color "orange" :v-adjust -0.1)
                               :extra "\nDEADLINE: %^{Deadline:}t"
                               )
                              ("Scheduled Task" :keys "s"
                               :icon ("calendar" :set "octicon" :color "orange")
                               :extra "\nSCHEDULED: %^{Start time:}t"
                               )
                              ))
                  ("Project" :keys "p"
                   :icon ("repo" :set "octicon" :color "silver")
                   :prepend t
                   :type entry
                   :headline "Inbox"
                   :template ("* %{time-or-todo} %?"
                              "%i"
                              "%a")
                   :file ""
                   :custom (:time-or-todo "")
                   :children (("Project-local todo" :keys "t"
                               :icon ("checklist" :set "octicon" :color "green")
                               :time-or-todo "TODO"
                               :file +org-capture-project-todo-file)
                              ("Project-local note" :keys "n"
                               :icon ("sticky-note" :set "faicon" :color "yellow")
                               :time-or-todo "%U"
                               :file +org-capture-project-notes-file)
                              ("Project-local changelog" :keys "c"
                               :icon ("list" :set "faicon" :color "blue")
                               :time-or-todo "%U"
                               :heading "Unreleased"
                               :file +org-capture-project-changelog-file))
                   )
                  ("\tCentralised project templates"
                   :keys "o"
                   :type entry
                   :prepend t
                   :template ("* %{time-or-todo} %?"
                              "%i"
                              "%a")
                   :children (("Project todo"
                               :keys "t"
                               :prepend nil
                               :time-or-todo "TODO"
                               :heading "Tasks"
                               :file +org-capture-central-project-todo-file)
                              ("Project note"
                               :keys "n"
                               :time-or-todo "%U"
                               :heading "Notes"
                               :file +org-capture-central-project-notes-file)
                              ("Project changelog"
                               :keys "c"
                               :time-or-todo "%U"
                               :heading "Unreleased"
                               :file +org-capture-central-project-changelog-file))
                   )))))

  (set-org-capture-templates)
  (unless (display-graphic-p)
    (add-hook 'server-after-make-frame-hook
              (defun org-capture-reinitialise-hook ()
                (when (display-graphic-p)
                  (set-org-capture-templates)
                  (remove-hook 'server-after-make-frame-hook
                               #'org-capture-reinitialise-hook))))))
(defun org-capture-select-template-prettier (&optional keys)
  "Select a capture template, in a prettier way than default
Lisp programs can force the template by setting KEYS to a string."
  (let ((org-capture-templates
         (or (org-contextualize-keys
              (org-capture-upgrade-templates org-capture-templates)
              org-capture-templates-contexts)
             '(("t" "Task" entry (file+headline "" "Tasks")
                "* TODO %?\n  %u\n  %a")))))
    (if keys
        (or (assoc keys org-capture-templates)
            (error "No capture template referred to by \"%s\" keys" keys))
      (org-mks org-capture-templates
               "Select a capture template\n━━━━━━━━━━━━━━━━━━━━━━━━━"
               "Template key: "
               `(("q" ,(concat (all-the-icons-octicon "stop" :face 'all-the-icons-red :v-adjust 0.01) "\tAbort")))))))
(advice-add 'org-capture-select-template :override #'org-capture-select-template-prettier)

(defun org-mks-pretty (table title &optional prompt specials)
  "Select a member of an alist with multiple keys. Prettified.

TABLE is the alist which should contain entries where the car is a string.
There should be two types of entries.

1. prefix descriptions like (\"a\" \"Description\")
   This indicates that `a' is a prefix key for multi-letter selection, and
   that there are entries following with keys like \"ab\", \"ax\"…

2. Select-able members must have more than two elements, with the first
   being the string of keys that lead to selecting it, and the second a
   short description string of the item.

The command will then make a temporary buffer listing all entries
that can be selected with a single key, and all the single key
prefixes.  When you press the key for a single-letter entry, it is selected.
When you press a prefix key, the commands (and maybe further prefixes)
under this key will be shown and offered for selection.

TITLE will be placed over the selection in the temporary buffer,
PROMPT will be used when prompting for a key.  SPECIALS is an
alist with (\"key\" \"description\") entries.  When one of these
is selected, only the bare key is returned."
  (save-window-excursion
    (let ((inhibit-quit t)
          (buffer (org-switch-to-buffer-other-window "*Org Select*"))
          (prompt (or prompt "Select: "))
          case-fold-search
          current)
      (unwind-protect
          (catch 'exit
            (while t
              (setq-local evil-normal-state-cursor (list nil))
              (erase-buffer)
              (insert title "\n\n")
              (let ((des-keys nil)
                    (allowed-keys '("\C-g"))
                    (tab-alternatives '("\s" "\t" "\r"))
                    (cursor-type nil))
                ;; Populate allowed keys and descriptions keys
                ;; available with CURRENT selector.
                (let ((re (format "\\`%s\\(.\\)\\'"
                                  (if current (regexp-quote current) "")))
                      (prefix (if current (concat current " ") "")))
                  (dolist (entry table)
                    (pcase entry
                      ;; Description.
                      (`(,(and key (pred (string-match re))) ,desc)
                       (let ((k (match-string 1 key)))
                         (push k des-keys)
                         ;; Keys ending in tab, space or RET are equivalent.
                         (if (member k tab-alternatives)
                             (push "\t" allowed-keys)
                           (push k allowed-keys))
                         (insert (propertize prefix 'face 'font-lock-comment-face) (propertize k 'face 'bold) (propertize "›" 'face 'font-lock-comment-face) "  " desc "…" "\n")))
                      ;; Usable entry.
                      (`(,(and key (pred (string-match re))) ,desc . ,_)
                       (let ((k (match-string 1 key)))
                         (insert (propertize prefix 'face 'font-lock-comment-face) (propertize k 'face 'bold) "   " desc "\n")
                         (push k allowed-keys)))
                      (_ nil))))
                ;; Insert special entries, if any.
                (when specials
                  (insert "─────────────────────────\n")
                  (pcase-dolist (`(,key ,description) specials)
                    (insert (format "%s   %s\n" (propertize key 'face '(bold all-the-icons-red)) description))
                    (push key allowed-keys)))
                ;; Display UI and let user select an entry or
                ;; a sub-level prefix.
                (goto-char (point-min))
                (unless (pos-visible-in-window-p (point-max))
                  (org-fit-window-to-buffer))
                (let ((pressed (org--mks-read-key allowed-keys
                                                  prompt
                                                  (not (pos-visible-in-window-p (1- (point-max)))))))
                  (setq current (concat current pressed))
                  (cond
                   ((equal pressed "\C-g") (user-error "Abort"))
                   ;; Selection is a prefix: open a new menu.
                   ((member pressed des-keys))
                   ;; Selection matches an association: return it.
                   ((let ((entry (assoc current table)))
                      (and entry (throw 'exit entry))))
                   ;; Selection matches a special entry: return the
                   ;; selection prefix.
                   ((assoc current specials) (throw 'exit current))
                   (t (error "No entry available")))))))
        (when buffer (kill-buffer buffer))))))
(advice-add 'org-mks :override #'org-mks-pretty)
;; even prettier now
(setf (alist-get 'height +org-capture-frame-parameters) 15)
;; (alist-get 'name +org-capture-frame-parameters) "❖ Capture") ;; ATM hardcoded in other places, so changing breaks stuff
(setq +org-capture-fn
      (lambda ()
        (interactive)
        (set-window-parameter nil 'mode-line-format 'none)
        (org-capture)))

(require 'org-download)
(add-hook 'org-mode-hook 'org-download-enable)
;; Use org-download-method directory is being used by default
(setq-default org-download-image-dir "~/kDrive/BC/Emacs/org/files/images")
(setq org-download-screenshot-method "gnome-screenshot -a -f %s")
(map! :leader
     (:prefix ("D" . "Org-Downloader")
     :desc "Org-Download-Clipboard"
     "c" #'org-download-clipboard
     :leader
     :desc "Org-Download-Yank"
     "y" #'org-download-yank))

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
        :desc "org-roam-graph" "g" #'org-roam-graph
        :desc "org-roam-node-insert" "i" #'org-roam-node-insert
        :desc "org-roam-dailies-goto-date" "d" #'org-roam-dailies-goto-date
        :desc "org-roam-ref-find" "r" #'org-roam-ref-find
        :desc "org-roam-dailies-goto-today" "t" #'org-roam-dailies-goto-today
        :desc "org-roam-dailies-goto-yesterday" "y" #'org-roam-dailies-goto-yesterday
        :desc "org-roam-dailies-capture-today" "j" #'org-roam-dailies-capture-today
        :desc "org-roam-dailies-goto-tomorrow" "T" #'org-roam-dailies-goto-tomorrow
        :desc "org-roam-node-find" "f" #'org-roam-node-find
        :desc "org-roam-capture" "c" #'org-roam-capture)
  (setq org-roam-directory "~/kDrive/BC/Emacs/org"
        org-roam-dailies-directory "daily/"
        org-roam-db-gc-threshold gc-cons-threshold
        org-roam-v2-ack t
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
(setq org-roam-mode-section-functions
       (list #'org-roam-backlinks-section
             #'org-roam-reflinks-section
             #'org-roam-unlinked-references-section
           ))
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

(setq deft-directory "~/kDrive/BC/Emacs/org/"
deft-strip-summary-regexp ":PROPERTIES:\n\\(.+\n\\)+:END:\n"
deft-use-filename-as-title 't)
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

(use-package! calibredb
  :commands calibredb
  :config
  (setq calibredb-root-dir "~/kDrive/BC/Media/Calibre"
        calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir))
  (map! :map calibredb-show-mode-map
        :ne "?" #'calibredb-entry-dispatch
        :ne "o" #'calibredb-find-file
        :ne "O" #'calibredb-find-file-other-frame
        :ne "V" #'calibredb-open-file-with-default-tool
        :ne "s" #'calibredb-set-metadata-dispatch
        :ne "e" #'calibredb-export-dispatch
        :ne "q" #'calibredb-entry-quit
        :ne "." #'calibredb-open-dired
        :ne [tab] #'calibredb-toggle-view-at-point
        :ne "M-t" #'calibredb-set-metadata--tags
        :ne "M-a" #'calibredb-set-metadata--author_sort
        :ne "M-A" #'calibredb-set-metadata--authors
        :ne "M-T" #'calibredb-set-metadata--title
        :ne "M-c" #'calibredb-set-metadata--comments)
  (map! :map calibredb-search-mode-map
        :ne [mouse-3] #'calibredb-search-mouse
        :ne "RET" #'calibredb-find-file
        :ne "?" #'calibredb-dispatch
        :ne "a" #'calibredb-add
        :ne "A" #'calibredb-add-dir
        :ne "c" #'calibredb-clone
        :ne "d" #'calibredb-remove
        :ne "D" #'calibredb-remove-marked-items
        :ne "j" #'calibredb-next-entry
        :ne "k" #'calibredb-previous-entry
        :ne "l" #'calibredb-virtual-library-list
        :ne "L" #'calibredb-library-list
        :ne "n" #'calibredb-virtual-library-next
        :ne "N" #'calibredb-library-next
        :ne "p" #'calibredb-virtual-library-previous
        :ne "P" #'calibredb-library-previous
        :ne "s" #'calibredb-set-metadata-dispatch
        :ne "S" #'calibredb-switch-library
        :ne "o" #'calibredb-find-file
        :ne "O" #'calibredb-find-file-other-frame
        :ne "v" #'calibredb-view
        :ne "V" #'calibredb-open-file-with-default-tool
        :ne "." #'calibredb-open-dired
        :ne "b" #'calibredb-catalog-bib-dispatch
        :ne "e" #'calibredb-export-dispatch
        :ne "r" #'calibredb-search-refresh-and-clear-filter
        :ne "R" #'calibredb-search-clear-filter
        :ne "q" #'calibredb-search-quit
        :ne "m" #'calibredb-mark-and-forward
        :ne "f" #'calibredb-toggle-favorite-at-point
        :ne "x" #'calibredb-toggle-archive-at-point
        :ne "h" #'calibredb-toggle-highlight-at-point
        :ne "u" #'calibredb-unmark-and-forward
        :ne "i" #'calibredb-edit-annotation
        :ne "DEL" #'calibredb-unmark-and-backward
        :ne [backtab] #'calibredb-toggle-view
        :ne [tab] #'calibredb-toggle-view-at-point
        :ne "M-n" #'calibredb-show-next-entry
        :ne "M-p" #'calibredb-show-previous-entry
        :ne "/" #'calibredb-search-live-filter
        :ne "M-t" #'calibredb-set-metadata--tags
        :ne "M-a" #'calibredb-set-metadata--author_sort
        :ne "M-A" #'calibredb-set-metadata--authors
        :ne "M-T" #'calibredb-set-metadata--title
        :ne "M-c" #'calibredb-set-metadata--comments))
;; Now, let's actually read the books
(use-package! nov
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (map! :map nov-mode-map
        :n "RET" #'nov-scroll-up)

  (defun doom-modeline-segment--nov-info ()
    (concat
     " "
     (propertize
      (cdr (assoc 'creator nov-metadata))
      'face 'doom-modeline-project-parent-dir)
     " "
     (cdr (assoc 'title nov-metadata))
     " "
     (propertize
      (format "%d/%d"
              (1+ nov-documents-index)
              (length nov-documents))
      'face 'doom-modeline-info)))

  (advice-add 'nov-render-title :override #'ignore)

  (defun +nov-mode-setup ()
    (face-remap-add-relative 'variable-pitch
                             :family "Merriweather"
                             :height 1.4
                             :width 'semi-expanded)
    (face-remap-add-relative 'default :height 1.3)
    (setq-local line-spacing 0.2
                next-screen-context-lines 4
                shr-use-colors nil)
    (require 'visual-fill-column nil t)
    (setq-local visual-fill-column-center-text t
                visual-fill-column-width 80
                nov-text-width 80)
    (visual-fill-column-mode 1)
    (hl-line-mode -1)

    (add-to-list '+lookup-definition-functions #'+lookup/dictionary-definition)

    (setq-local mode-line-format
                `((:eval
                   (doom-modeline-segment--workspace-name))
                  (:eval
                   (doom-modeline-segment--window-number))
                  (:eval
                   (doom-modeline-segment--nov-info))
                  ,(propertize
                    " %P "
                    'face 'doom-modeline-buffer-minor-mode)
                  ,(propertize
                    " "
                    'face (if (doom-modeline--active) 'mode-line 'mode-line-inactive)
                    'display `((space
                                :align-to
                                (- (+ right right-fringe right-margin)
                                   ,(* (let ((width (doom-modeline--font-width)))
                                         (or (and (= width 1) 1)
                                             (/ width (frame-char-width) 1.0)))
                                       (string-width
                                        (format-mode-line (cons "" '(:eval (doom-modeline-segment--major-mode))))))))))
                  (:eval (doom-modeline-segment--major-mode)))))

  (add-hook 'nov-mode-hook #'+nov-mode-setup))

(add-to-list 'initial-frame-alist '(fullscreen . maximized))
;; (set-frame-parameter (selected-frame) 'alpha '(95 50))
