;;; pani-notmuch-calendar.el --- Answer calendar invites from notmuch -*- lexical-binding: t; -*-

;;; Commentary:

;; Replaces notmuch's rendering of text/calendar parts with:
;;
;;  - a summary of the event (title, time, location, organizer, recurrence),
;;  - [Accept] [Tentative] [Decline] [Add only] buttons for invitations,
;;  - a [Remove from calendar] button for cancellations,
;;  - optionally, your org day agenda for the event's date.
;;
;; Accept/Tentative send an RFC 5546 REPLY to the organizer and capture the
;; event via `org-capture'; tentative events are titled "<summary> (TENTATIVE)".
;; Decline sends the reply and removes any previously captured entry.
;; With a prefix argument, the reply is left open for editing instead of sent.
;;
;; Structure follows notmuch-calendar-x (Tom Hinton): the event UID is the
;; Org ID of the captured entry and its SEQUENCE is stored alongside, so a
;; rescheduled invitation replaces the old entry instead of duplicating it.
;;
;; Parsing, reply generation and org timestamps come from gnus-icalendar's
;; public functions rather than the private `icalendar--*' helpers, which
;; Emacs 31 declares obsolete.  Those functions handle VTIMEZONE conversion,
;; all-day events, simple RRULE repeaters, and build a reply containing only
;; your own ATTENDEE line with PARTSTAT=ACCEPTED/TENTATIVE/DECLINED.
;;
;; The capture file must be in `org-agenda-files'.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'gnus-icalendar)
(require 'notmuch-lib)
(require 'notmuch-show)
(require 'notmuch-mua)
(require 'notmuch-tag)
(require 'mml)
(require 'org)
(require 'org-id)
(require 'org-capture)
(require 'org-agenda)

;;;; Options

(defgroup pani/notmuch-calendar nil
  "Answer calendar invitations from notmuch."
  :group 'notmuch)

(defcustom pani/notmuch-calendar-capture-target
  (list 'file (expand-file-name "calendar.org" org-directory))
  "Where accepted and tentative events are captured.
Any target accepted by `org-capture-templates', e.g.
  (file \"~/org/calendar.org\")
  (file+headline \"~/org/calendar.org\" \"Meetings\")
  (file+olp+datetree \"~/org/calendar.org\")"
  :type 'sexp)

(defcustom pani/notmuch-calendar-identities nil
  "Regexps matching your attendee address(es) in invitations.
When nil, use `user-mail-address' and all addresses from
`notmuch-user-emails'."
  :type '(repeat regexp))

(defcustom pani/notmuch-calendar-tentative-suffix " (TENTATIVE)"
  "Appended to the title of events captured as tentative."
  :type 'string)

(defcustom pani/notmuch-calendar-include-description t
  "Whether to copy the invitation's description into the Org entry."
  :type 'boolean)

(defcustom pani/notmuch-calendar-inline-agenda t
  "Whether to show your org day agenda for the event's date."
  :type 'boolean)

(defcustom pani/notmuch-calendar-tag-messages t
  "Whether to tag answered invitations +accepted, +tentative or +declined."
  :type 'boolean)

;;;; iCalendar helpers

(defun pani/notmuch-calendar--normalize (text)
  "Return TEXT with CRLF line endings converted to LF."
  (replace-regexp-in-string "\r\n" "\n" text))

(defun pani/notmuch-calendar--prop (ics name)
  "Return the value of the first property NAME in ICS, or nil.
Used for the few fields gnus-icalendar does not expose."
  (let ((case-fold-search t)
	(text (replace-regexp-in-string "\n[ \t]" "" ics)))
    (when (string-match
	   (format "^%s\\(?:;[^:\n]*\\)?:\\(.*\\)$" (regexp-quote name))
	   text)
      (string-trim (match-string 1 text)))))

(defun pani/notmuch-calendar--identities ()
  "Return regexps identifying the user as an attendee."
  (or pani/notmuch-calendar-identities
      (delete-dups
       (mapcar #'regexp-quote
	       (delq nil (cons user-mail-address
			       (ignore-errors (notmuch-user-emails))))))))

(defun pani/notmuch-calendar--parse (ics)
  "Parse ICS into a `gnus-icalendar-event', or return nil."
  (ignore-errors
    (with-temp-buffer
      (insert ics)
      (gnus-icalendar-event-from-buffer
       (current-buffer) (pani/notmuch-calendar--identities)))))

(defun pani/notmuch-calendar--sequence (ics)
  "Return the SEQUENCE number of ICS (0 if absent)."
  (string-to-number (or (pani/notmuch-calendar--prop ics "SEQUENCE") "0")))

(defun pani/notmuch-calendar--key (ics event)
  "Return the Org ID used to track EVENT from ICS.
This is the UID, plus the RECURRENCE-ID for changes to a single
occurrence of a series, so those get their own entry."
  (let ((uid (gnus-icalendar-event:uid event))
	(recid (pani/notmuch-calendar--prop ics "RECURRENCE-ID")))
    (when (string-empty-p uid)
      (setq uid (md5 (format "%s|%s" (gnus-icalendar-event:summary event)
			     (gnus-icalendar-event:start event)))))
    (if recid (concat uid "_" recid) uid)))

(defun pani/notmuch-calendar--strip-mailto (addr)
  "Remove a leading mailto: from ADDR."
  (and addr (replace-regexp-in-string "\\`[mM][aA][iI][lL][tT][oO]:" "" addr)))

(defun pani/notmuch-calendar--one-line (s)
  "Collapse whitespace and newlines in S, or return nil if S is blank."
  (when s
    (let ((s (string-trim (replace-regexp-in-string "[ \t\n]+" " " s))))
      (unless (string-empty-p s) s))))

;;;; Org side

(defun pani/notmuch-calendar--entry-info (key)
  "Return (:sequence N :status STRING) of the entry with ID KEY, or nil."
  (when-let* ((m (ignore-errors (org-id-find key 'marker))))
    (prog1 (with-current-buffer (marker-buffer m)
	     (org-with-wide-buffer
	      (goto-char m)
	      (list :sequence (string-to-number
			       (or (org-entry-get nil "ICAL_SEQUENCE") "-1"))
		    :status (org-entry-get nil "ICAL_STATUS"))))
      (set-marker m nil))))

(defun pani/notmuch-calendar--remove-entry (key)
  "Delete the Org entry with ID KEY, if any.  Return non-nil if deleted."
  (when-let* ((m (ignore-errors (org-id-find key 'marker))))
    (with-current-buffer (marker-buffer m)
      (org-with-wide-buffer
       (goto-char m)
       (org-back-to-heading t)
       (delete-region (point) (org-end-of-subtree t t)))
      (save-buffer))
    (set-marker m nil)
    t))

(defun pani/notmuch-calendar--body-escape (s)
  "Escape lines of S that Org would read as headings or keywords."
  (replace-regexp-in-string "^\\([*]\\|[ \t]*#\\+\\)" ",\\1" s))

(defun pani/notmuch-calendar--entry-text (event key seq status level)
  "Return the Org entry for EVENT with ID KEY, SEQ and STATUS at LEVEL."
  (let* ((summary (or (pani/notmuch-calendar--one-line
		       (gnus-icalendar-event:summary event))
		      "No summary"))
	 (title (if (eq status 'tentative)
		    (concat summary pani/notmuch-calendar-tentative-suffix)
		  summary))
	 (organizer (pani/notmuch-calendar--one-line
		     (pani/notmuch-calendar--strip-mailto
		      (gnus-icalendar-event:organizer event))))
	 (location (pani/notmuch-calendar--one-line
		    (gnus-icalendar-event:location event)))
	 (description (and pani/notmuch-calendar-include-description
			   (gnus-icalendar-event:description event)))
	 (description (and description (string-trim description))))
    (concat (make-string level ?*) " " title "\n"
	    ":PROPERTIES:\n"
	    ":ID: " key "\n"
	    ":ICAL_SEQUENCE: " (number-to-string seq) "\n"
	    ":ICAL_STATUS: " (symbol-name status) "\n"
	    (if organizer (format ":ORGANIZER: [[mailto:%s]]\n" organizer) "")
	    (if location (format ":LOCATION: %s\n" location) "")
	    ":END:\n"
	    (gnus-icalendar-event:org-timestamp event) "\n"
	    (if (and description (not (string-empty-p description)))
		(concat "\n" (pani/notmuch-calendar--body-escape description) "\n")
	      ""))))

(defun pani/notmuch-calendar--capture (event key seq status)
  "Capture EVENT (ID KEY, sequence SEQ) with STATUS, replacing any old entry.
`org-capture' places a placeholder heading at the configured target
(so datetree/headline targets work as usual), which is then replaced
with the real entry.  This keeps the invitation's text away from
org-capture's %-escape expansion."
  (pani/notmuch-calendar--remove-entry key)
  (let ((org-capture-templates
	 `(("x" "notmuch invitation" entry
	    ,pani/notmuch-calendar-capture-target
	    "* pani-notmuch-calendar-placeholder"
	    :immediate-finish t))))
    ;; Date-based targets (datetree) file by the event's date.
    (let ((org-overriding-default-time (gnus-icalendar-event:start-time event)))
      (org-capture nil "x")))
  (let ((m org-capture-last-stored-marker))
    (with-current-buffer (marker-buffer m)
      (org-with-wide-buffer
       (goto-char m)
       (org-back-to-heading t)
       (let ((level (org-current-level)))
	 (delete-region (point) (org-end-of-subtree t t))
	 (insert (pani/notmuch-calendar--entry-text event key seq status level))))
      (org-id-add-location key (buffer-file-name (buffer-base-buffer)))
      (save-buffer))))

(defun pani/notmuch-calendar--faces-only (s)
  "Return a copy of S keeping only its faces (also as `font-lock-face')."
  (let ((out (substring-no-properties s))
	(pos 0)
	(len (length s)))
    (while (< pos len)
      (let ((next (next-single-property-change pos 'face s len))
	    (face (get-text-property pos 'face s)))
	(when face
	  (add-text-properties pos next `(face ,face font-lock-face ,face) out))
	(setq pos next)))
    out))

(defun pani/notmuch-calendar--day-agenda (time)
  "Return the org day agenda for TIME as a string, or an error note."
  (condition-case err
      (let ((org-agenda-markers nil)	; don't touch the real agenda's markers
	    (org-agenda-sticky nil)
	    (org-agenda-skip-unavailable-files t) ; never prompt while rendering
	    (org-agenda-buffer-name " *pani-notmuch-calendar-agenda*")
	    (org-agenda-window-setup 'current-window)
	    (org-agenda-use-time-grid t)
	    (org-agenda-remove-tags t))
	(save-window-excursion
	  (org-agenda-list nil (format-time-string "%Y-%m-%d" time) 'day)
	  (prog1 (pani/notmuch-calendar--faces-only (buffer-string))
	    (kill-buffer)
	    (org-agenda-reset-markers))))
    (error (format "(could not build agenda: %s)\n" (error-message-string err)))))

;;;; Replying

(defun pani/notmuch-calendar--from (headers)
  "Return a From: value for replying to a message with HEADERS.
Uses whichever of your addresses the invitation was sent to."
  (let* ((recipients (downcase (concat (plist-get headers :To) ","
				       (plist-get headers :Cc))))
	 (addr (or (seq-find (lambda (a) (string-search (downcase a) recipients))
			     (notmuch-user-emails))
		   (notmuch-user-primary-email))))
    (message-make-from (notmuch-user-name) addr)))

(defun pani/notmuch-calendar--build-reply (ics status)
  "Return the iCalendar REPLY to ICS with STATUS."
  (with-temp-buffer
    (insert ics)
    (gnus-icalendar-event-reply-from-buffer
     (current-buffer) status (pani/notmuch-calendar--identities))))

(defvar-local pani/notmuch-calendar-reply-buffer nil
  "Non-nil in a compose buffer holding a calendar invitation reply.")

(defun pani/notmuch-calendar--compose (ctx event status reply-ics edit on-send)
  "Compose the reply REPLY-ICS with STATUS to EVENT; send unless EDIT.
CTX is the message context stored on the button. ON-SEND is called
once the reply has actually been sent."
  (let* ((organizer (pani/notmuch-calendar--strip-mailto
		     (gnus-icalendar-event:organizer event)))
	 (summary (gnus-icalendar-event:summary event))
	 (label (capitalize (symbol-name status)))
	 (id (plist-get ctx :id))
	 (message-signature nil))
    (when (or (null organizer) (string-empty-p organizer))
      (user-error "Invitation has no organizer to reply to"))
    (notmuch-mua-mail organizer (format "%s: %s" label summary)
		      `((From . ,(plist-get ctx :from))
			(In-Reply-To . ,(format "<%s>" id))
			(References . ,(format "<%s>" id))))
    (setq pani/notmuch-calendar-reply-buffer t)
    (message-add-action on-send 'send)
    (goto-char (point-max))
    (insert "<#multipart type=alternative>\n"
	    "<#part type=\"text/plain\" charset=utf-8 disposition=inline>\n"
	    (format "%s has %s the invitation \"%s\".\n"
		    (notmuch-user-name)
		    (pcase status
		      ('accepted "accepted")
		      ('tentative "tentatively accepted")
		      ('declined "declined"))
		    summary))
    (insert "<#part type=\"text/calendar; method=REPLY\" charset=utf-8 disposition=inline>\n")
    (let ((cal-beg (point)))
      (insert (pani/notmuch-calendar--normalize reply-ics))
      (unless (bolp) (insert "\n"))
      (mml-quote-region cal-beg (point)))
    (insert "<#/multipart>\n")
    (unless edit
      (notmuch-mua-send-and-exit))))

;;;; Button actions

(defun pani/notmuch-calendar--refresh (buf)
  "Re-render notmuch-show buffer BUF."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (notmuch-show-refresh-view))))

(defun pani/notmuch-calendar--respond (button)
  "Answer the invitation stored on BUTTON.
The calendar entry and tags are only changed once the reply is sent,
so a reply opened for editing and then discarded changes nothing."
  (let* ((ics (button-get button 'pani-ics))
	 (ctx (button-get button 'pani-ctx))
	 (status (button-get button 'pani-status))
	 (edit current-prefix-arg)
	 (show-buf (current-buffer))
	 (event (or (pani/notmuch-calendar--parse ics)
		    (user-error "Could not parse invitation")))
	 (key (pani/notmuch-calendar--key ics event))
	 (seq (pani/notmuch-calendar--sequence ics))
	 ;; Build the reply first so a failure has no side effects.
	 (reply (and (not (eq status 'added))
		     (pani/notmuch-calendar--build-reply ics status)))
	 (commit
	  (lambda ()
	    (if (eq status 'declined)
		(pani/notmuch-calendar--remove-entry key)
	      (pani/notmuch-calendar--capture
	       event key seq (if (eq status 'added) 'accepted status)))
	    (when (and reply pani/notmuch-calendar-tag-messages)
	      (notmuch-tag (notmuch-id-to-query (plist-get ctx :id))
			   (mapcar (lambda (s)
				     (concat (if (eq s status) "+" "-")
					     (symbol-name s)))
				   '(accepted tentative declined))))
	    (pani/notmuch-calendar--refresh show-buf))))
    (if reply
	(pani/notmuch-calendar--compose ctx event status reply edit commit)
      (funcall commit)
      (message "Added \"%s\" to calendar without replying"
	       (gnus-icalendar-event:summary event)))))

(defun pani/notmuch-calendar--remove (button)
  "Remove the cancelled event stored on BUTTON from the calendar."
  (let ((show-buf (current-buffer)))
    (if (pani/notmuch-calendar--remove-entry (button-get button 'pani-key))
	(message "Removed from calendar")
      (message "Entry not found"))
    (pani/notmuch-calendar--refresh show-buf)))

(define-button-type 'pani/notmuch-calendar-button
  'follow-link t
  'face 'custom-button
  'mouse-face 'custom-button-mouse
  'help-echo "Answer invitation (with prefix arg: edit reply before sending)")

(defun pani/notmuch-calendar--insert-button (label action &rest props)
  "Insert a button with LABEL calling ACTION, with extra PROPS."
  (apply #'insert-button label
	 'type 'pani/notmuch-calendar-button
	 'action action
	 props)
  (insert " "))

;;;; Rendering

(defun pani/notmuch-calendar--line (label value)
  "Insert a LABEL: VALUE line if VALUE is non-blank."
  (when-let* ((v (pani/notmuch-calendar--one-line value)))
    (insert (propertize (format "%-11s" label) 'face 'bold 'font-lock-face 'bold)
	    v "\n")))

(defun pani/notmuch-calendar--insert-event (msg ics event)
  "Insert the rendering of EVENT (parsed from ICS) for notmuch MSG."
  (let* ((method (upcase (or (gnus-icalendar-event:method event) "")))
	 (key (pani/notmuch-calendar--key ics event))
	 (seq (pani/notmuch-calendar--sequence ics))
	 (entry (pani/notmuch-calendar--entry-info key))
	 (entry-seq (plist-get entry :sequence))
	 (headers (plist-get msg :headers))
	 (ctx (list :id (plist-get msg :id)
		    :from (pani/notmuch-calendar--from headers))))
    (insert (propertize (or (gnus-icalendar-event:summary event) "No summary")
			'face '(bold :height 1.1)
			'font-lock-face '(bold :height 1.1))
	    "\n")
    (pani/notmuch-calendar--line "When:" (gnus-icalendar-event:org-timestamp event))
    (pani/notmuch-calendar--line "Where:" (gnus-icalendar-event:location event))
    (pani/notmuch-calendar--line "Organizer:" (pani/notmuch-calendar--strip-mailto
					       (gnus-icalendar-event:organizer event)))
    (pcase method
      ("REQUEST"
       (cond
	((and entry (> entry-seq seq))
	 (insert "Outdated invitation: a newer version is already in your calendar.\n"))
	(t
	 (cond ((and entry (> seq entry-seq))
		(insert "Updated invitation (your calendar has an older version).\n"))
	       (entry
		(insert (format "In your calendar as %s.\n" (plist-get entry :status)))))
	 (dolist (b '(("[ Accept ]" accepted)
		      ("[ Tentative ]" tentative)
		      ("[ Decline ]" declined)
		      ("[ Add only ]" added)))
	   (pani/notmuch-calendar--insert-button
	    (car b) #'pani/notmuch-calendar--respond
	    'pani-status (cadr b) 'pani-ics ics 'pani-ctx ctx))
	 (insert "\n"))))
      ("CANCEL"
       (insert "This event has been cancelled by the organizer.\n")
       (when entry
	 (pani/notmuch-calendar--insert-button
	  "[ Remove from calendar ]" #'pani/notmuch-calendar--remove 'pani-key key)
	 (insert "\n")))
      ("REPLY"
       (let ((case-fold-search t)
	     (text (replace-regexp-in-string "\n[ \t]" "" ics)))
	 (when (string-match "^ATTENDEE[^\n]*PARTSTAT=\\([A-Z-]+\\)[^\n]*:mailto:\\(.*\\)$"
			     text)
	   (pani/notmuch-calendar--line
	    "Response:" (format "%s %s" (match-string 2 text)
				(downcase (match-string 1 text)))))))
      (_
       (pani/notmuch-calendar--insert-button
	"[ Add to calendar ]" #'pani/notmuch-calendar--respond
	'pani-status 'added 'pani-ics ics 'pani-ctx ctx)
       (insert "\n")))
    (when (and pani/notmuch-calendar-inline-agenda
	       (member method '("REQUEST" "CANCEL")))
      (insert "\n" (pani/notmuch-calendar--day-agenda
		    (gnus-icalendar-event:start-time event))))))

(defun pani/notmuch-calendar-insert-part (msg part _content-type _nth _depth _button)
  "Render a text/calendar PART of MSG.  Return nil to fall back to plain text."
  (let* ((ics (pani/notmuch-calendar--normalize
	       (notmuch-get-bodypart-text msg part notmuch-show-process-crypto)))
	 (event (pani/notmuch-calendar--parse ics)))
    (when event
      (pani/notmuch-calendar--insert-event msg ics event)
      t)))

(defun pani/notmuch-calendar--prefer-calendar (types)
  "Move text/calendar to the front of the multipart/alternative TYPES.
Outlook sends text/plain, text/html and text/calendar alternatives, and
notmuch would otherwise show the plain text and hide the invitation."
  (let ((cal (seq-filter (lambda (ty) (string-prefix-p "text/calendar" ty)) types)))
    (append cal (seq-difference types cal))))

;;;###autoload
(define-minor-mode pani/notmuch-calendar-mode
  "Render calendar invitations in notmuch with reply and capture buttons."
  :global t
  (if pani/notmuch-calendar-mode
      (progn
	(advice-add 'notmuch-show-insert-part-text/calendar
		    :override #'pani/notmuch-calendar-insert-part)
	(advice-add 'notmuch-multipart/alternative-choose
		    :filter-return #'pani/notmuch-calendar--prefer-calendar))
    (advice-remove 'notmuch-show-insert-part-text/calendar
		   #'pani/notmuch-calendar-insert-part)
    (advice-remove 'notmuch-multipart/alternative-choose
		   #'pani/notmuch-calendar--prefer-calendar)))

(provide 'pani-notmuch-calendar)
;;; pani-notmuch-calendar.el ends here
