;;; fanfou.el --- Simple Emacs-based client for Fanfou

;; Author: Neil Roberts, ngn999, lifanxi
;; Keywords: Fanfou

;; Copyright 2008, 2009, 2010  Neil Roberts
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Commentary:

;; A Fanfou client for emacs that can view your friends timeline and
;; publish new statuses.

;; This requires the oauth package from here:
;;    http://www.emacswiki.org/emacs/OAuthLibrary

;;; You should add the following to your Emacs configuration file:

;; (autoload 'fanfou-get-friends-timeline "fanfou" nil t)
;; (autoload 'fanfou-status-edit "fanfou" nil t)
;; (global-set-key "\C-xt" 'fanfou-get-friends-timeline)
;; (add-hook 'fanfou-status-edit-mode-hook 'longlines-mode)

;; You can view the statuses by pressing C-x t. While in the timeline
;; buffer you can press C-c C-s to post a new status or C-c C-r to
;; reply to the status at point. Once the message is finished press
;; C-c C-c to publish.

;; To use Fanfou you need to specify a consumer key and consumer
;; secret for OAuth authentication. Fanfou's idea is that an
;; application developer would hardcode these keys into an application
;; and then try to hide them. However that's not really possible with
;; an open source application so instead they are left blank here. To
;; get keys you could either register your own Fanfou application or
;; possibly steal another key from another application. Once you have
;; the value you can customize the fanfou group to set them.

;; The first time you use Fanfou it will use OAuth to get an access
;; token from Fanfou. This will require you to login to a web page
;; and copy a code. The access token is saved so this should only be
;; needed once.

;;; Code:
(require 'cl)
(require 'url)
(require 'url-http)
(require 'xml)
(require 'oauth)

(defgroup fanfou nil "Fanfou status viewer"
  :group 'applications)

(defgroup fanfou-faces nil "Faces for displaying Fanfou statuses"
  :group 'fanfou)

(defface fanfou-header-face
  '((t (:background "light gray")))
  "base face for headers"
  :group 'fanfou-faces)

(defface fanfou-user-name-face
  '((t (:weight bold :inherit fanfou-header-face)))
  "face for user name headers"
  :group 'fanfou-faces)

(defface fanfou-time-stamp-face
  '((t (:slant italic :inherit fanfou-header-face)))
  "face for time stamp headers"
  :group 'fanfou-faces)

(defface fanfou-status-overlong-face
  '((t (:foreground "red")))
  "face used for characters in overly long Fanfou statuses.
The face is also used in the mode line if the character count
remaining drops to negative.")

(defface fanfou-new-tweets-sep-face
  '((t (:background "black" :foreground "white")))
  "Used when printing the separator between tweets already seen,
and tweets newly arrived."
  :group 'fanfou-faces)

(defconst fanfou-friends-timeline-url
  "http://api.fanfou.com/statuses/friends_timeline.xml"
  "URL used to receive the friends timeline")

(defconst fanfou-replies-timeline-url
  "http://api.fanfou.com/statuses/replies.xml"
  "URL used to receive the replies timeline")

(defconst fanfou-status-update-url
  "http://api.fanfou.com/statuses/update.xml"
  "URL used to update Fanfou status")

(defconst fanfou-month-map
  '(("Jan" . 1)
    ("Feb" . 2)
    ("Mar" . 3)
    ("Apr" . 4)
    ("May" . 5)
    ("Jun" . 6)
    ("Jul" . 7)
    ("Aug" . 8)
    ("Sep" . 9)
    ("Oct" . 10)
    ("Nov" . 11)
    ("Dec" . 12))
  "Assoc list mapping month abbreviations to month numbers")

(defcustom fanfou-consumer-key
  "ad5f22a5d181a37960379707a1834d6e"
  "The consumer key for Fanfou to gain an OAuth access token."
  :type 'string
  :group 'fanfou)

(defcustom fanfou-consumer-secret
  "5e6fb3d8dc5cb38b0d2aff5a166731a4"
  "The consumer secret for Fanfou to gain an OAuth access token."
  :type 'string
  :group 'fanfou)

(defvar fanfou-access-token nil
  "Access token to authenticate with Fanfou.
If nil, fanfou will try to read a saved access token. If there
isn't one, it will try to fetch a new token from Fanfou.")

(defconst fanfou-access-token-file
  "~/.fanfou-access-token"
  "Name of a file to store the access token in.")

(defconst fanfou-request-url
  "http://fanfou.com/oauth/request_token")

(defconst fanfou-access-url
  "http://fanfou.com/oauth/access_token")

(defconst fanfou-authorize-url
  "http://fanfou.com/oauth/authorize")

(defcustom fanfou-maximum-status-length 140
  "Maximum length to allow in a Fanfou status update."
  :type 'integer
  :group 'fanfou)

(defcustom fanfou-fetch-status-count nil
  "Number of status to retrieve when displaying a timeline.
If nil, it will be left up to the Fanfou server to choose a default."
  :type '(choice (const :tag "Default" nil) (integer))
  :group 'fanfou)

(defcustom fanfou-include-replies nil
  "Whether to include the replies list in your friends timeline.
If t, the replies list will be merged and sorted with your
friends timeline."
  :type 'boolean
  :group 'fanfou)

(defcustom fanfou-status-source "Efan"
  "What to send as the source of status updates.
The Fanfou website will use this to display a message like:

about 3 minutes ago from fanfou."
  :type 'string
  :group 'fanfou)

(defcustom fanfou-time-format 'fanfou-format-time-for-display
  "Function or string describing the format to display time stamps in.
If the value is a string it should be a format string with %
characters and it will be passed to format-time-string.

If the value is a function it will be called with a single
argument which will be a two element list containing the high and
low part of the number of seconds since epoch. The value can be
converted to broken down time using decode-time.

Otherwise the variable can be nil in which case the time string
from Fanfou will be displayed directly."
  :type '(choice (const :tag "No translation" nil)
                 string
                 function)
  :group 'fanfou)

(defcustom fanfou-new-tweets-sep
  (propertize
   "----[ NEW ]--------------------------------------------------------------"
   'face 'fanfou-new-tweets-sep-face)
  "Will be printed in between the last seen tweet, and the newly
received tweets."
  :type 'string
  :group 'fanfou)

(defconst fanfou-default-status-format
  (concat (propertize "%-32n"
                      'face 'fanfou-user-name-face)
          (propertize "%33t"
                      'face 'fanfou-time-stamp-face)
          (propertize " %r"
                      'face 'fanfou-header-face)
          "\n%M\n\n")
  "The default status format.
This can be set as the value for fanfou-status-format to make it
display the tweets with a long header line with the user's full
name, time of posting and a reply button followed by the content
of the tweet on a new line.")

(defconst fanfou-web-status-format
  (concat (propertize "%u"
                      'face 'fanfou-user-name-face)
          " %M\n"
          (propertize "%t from %s"
                      'face 'fanfou-time-stamp-face)
          "\n\n")
  "A status format to appear more like the fanfou website.
This can be set as the value for fanfou-status-format to make it
display the tweets in a style similar to the fanfou website. The
screen name of the tweeter preceeds the message and the time and
source is given on the next line.")

(defcustom fanfou-status-format
  fanfou-default-status-format
  "Format string describing how to display fanfou statuses
It should be a string containing '%' characters followed by one
of the following commands:

%n - the full name of the person posting the tweet
%u - the screen name of the person posting the tweet
%t - the time the tweet was created. This gets formatted
     according to fanfou-time-format
%r - a reply button
%m - the tweet's text
%M - the tweet's text but filled with fill-region
%s - the name of the program used to send the tweet

%i - the numeric id of the tweet
%T - whether the tweet was truncated

%U - the screen name of the person who the tweet was a reply to
%R - the status id of the tweet that this is a reply to
%S - the user id of the tweet that this is a reply to

%I - the id of the user posting the tweet
%l - the location of the user posting the tweet
%d - a description of the user posting the tweet
%A - a URL to the image for the person posting the tweet
%L - a URL to the home page for the person posting the tweet
%F - the number of followers of the person posting the tweet
%P - whether posts from this user are protected

%% - a literal percent character

Any other text is copied directly into the buffer. Text
properties are preserved and the properties of the % markers will
be applied to the resulting string.

The marker can optionally be given a padding value after the %
symbol. If the value is negative, the padding will be added to
the right otherwise it will be added to the left."
  :type `(choice (const :tag "Default" ,fanfou-default-status-format)
                 (const :tag "Web" ,fanfou-web-status-format)
                 string)
  :group 'fanfou)

(defvar fanfou-status-edit-remaining-length ""
  "Characters remaining in a Fanfou status update.
This is displayed in the mode line.")

(put 'fanfou-status-edit-remaining-length 'risky-local-variable t)

(defvar fanfou-status-edit-overlay nil
  "Overlay used to highlight overlong status messages.")

(defvar fanfou-status-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map text-mode-map)
    (define-key map "\C-c\C-c" 'fanfou-status-post)
    (define-key map "\C-c\C-k" 'fanfou-kill-status-buffer)
    map)
  "Keymap for `fanfou-status-edit-mode'.")

(defvar fanfou-timeline-view-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map text-mode-map)
    (define-key map "\C-c\C-r" 'fanfou-reply)
    (define-key map "\C-c\C-s" 'fanfou-status-edit)
    map)
  "Keymap for `fanfou-timeline-view-mode'.")

(defvar fanfou-frame-configuration nil
  "Frame configuration from immediately before a fanfou command
is called")

(defvar fanfou-last-id-seen nil
  "The last fanfou id that was seen while building up the status
  list.")

(defvar fanfou-last-id-saved nil
  "The last id that was saved as seen; this tweet will have a
  line above it (or whatever fanfou-new-tweets-sep is set to)
  when shown again to demarcate old tweets from new ones.")

(defun fanfou-retrieve-url (url cb &optional cbargs)
  "Wrapper around url-retrieve.
Fetches an access token and retains it if we don't already have
one."

  ;; If we don't already have an access token then fetch it now
  (when (null fanfou-access-token)
    ;; Check if we saved a key from a previous instance
    (if (file-exists-p fanfou-access-token-file)
        (with-temp-buffer
          (insert-file-contents fanfou-access-token-file)
          (setq fanfou-access-token (read (current-buffer))))
      ;; Otherwise fetch it from fanfou
      (setq fanfou-access-token
            (oauth-authorize-app fanfou-consumer-key
                                 fanfou-consumer-secret
                                 fanfou-request-url
                                 fanfou-access-url
                                 fanfou-authorize-url))
      ;; Save the token for next time
      (with-temp-file fanfou-access-token-file
        (prin1 fanfou-access-token (current-buffer)))))

  (oauth-url-retrieve fanfou-access-token url cb cbargs))

(defun fanfou-retrieve-timeline-url (url cb &optional cbargs)
  "Wrapper around fanfou-retrieve-url which sets the count parameter.
This can be used for Fanfou API methods that fetch a
timeline. It sets the count parameter based on the
fanfou-fetch-status-count variable."
  (when fanfou-fetch-status-count
     (setq url (concat url
                       "?count="
                       (number-to-string fanfou-fetch-status-count))))
  (fanfou-retrieve-url url cb cbargs))

(defun fanfou-get-friends-timeline ()
  "Fetch and display the friends timeline.
The results are formatted and displayed in a buffer called
*Fanfou friends timeline*

If the variable `fanfou-include-replies' is non-nil, the replies
timeline will also be merged into the friends timeline and
displayed."
  (interactive)
  (fanfou-retrieve-timeline-url fanfou-friends-timeline-url
                                 'fanfou-fetched-friends-timeline
                                 (list (if fanfou-include-replies
                                           (list fanfou-replies-timeline-url)
                                         nil)
                                       ;; next arg is list of status to merge
                                       nil)))

(defun fanfou-fetched-friends-timeline (status other-urls status-list)
  "Callback handler for fetching the Fanfou friends timeline."
  (let ((result-buffer (current-buffer)) doc)
    ;; Make sure the temporary results buffer is killed even if the
    ;; xml parsing raises an error
    (unwind-protect
        (progn
          ;; Skip the mime headers
          (goto-char (point-min))
          (re-search-forward "\n\n")
          ;; Parse the rest of the document
		  (mm-enable-multibyte)
		  (decode-coding-region (point-min) (point-max) 'utf-8)
          (setq doc (xml-parse-region (point) (point-max))))
      (kill-buffer result-buffer))
    ;; Merge the new list with the current list of statuses
    (setq status-list (fanfou-merge-status-lists status-list
                                                  (xml-get-children (car doc)
                                                                    'status)))
    ;; If there's more URLs then start fetching those
    (if other-urls
        (fanfou-retrieve-timeline-url (car other-urls)
                                       'fanfou-fetched-friends-timeline
                                       (list (cdr other-urls) status-list))
      ;; Otherwise display the results
      ;; Get a clean buffer to display the results
      (let ((buf (get-buffer-create "*Fanfou friends timeline*"))
            (compiled-format (fanfou-compile-format-string
                              fanfou-status-format)))
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (erase-buffer)
            (kill-all-local-variables)
            ;; If the GET failed then display an error instead
            (if (plist-get status :error)
                (fanfou-show-error doc)
              ;; Otherwise process each status node
	      (progn
		(setq fanfou-last-id-seen 'nil)
		(while status-list
		  (fanfou-format-status-node (car status-list)
					      compiled-format)
		  (setq status-list (cdr status-list)))
		(setq fanfou-last-id-saved fanfou-last-id-seen))))
          (goto-char (point-min))
          (fanfou-timeline-view-mode))
        (view-buffer buf 'kill-buffer)))))

;; Angle brackets ("<" and ">") are entity-encoded.
;; See Question 7) "Encoding affects status character count" at
;; http://apiwiki.fanfou.com/Things-Every-Developer-Should-Know
(defun fanfou-decode-entity-encoding (str)
  (let (result)
    (setq result (replace-regexp-in-string "&gt;" ">" str))
    (setq result (replace-regexp-in-string "&lt;" "<" result))))

(defun fanfou-get-node-text (node)
  "Return the text of XML node NODE.
All of the text elements are concatenated together and returned
as a single string."
  (let (text-parts)
    (dolist (part (xml-node-children node))
      (when (stringp part)
        (push (fanfou-decode-entity-encoding part) text-parts)))
    (apply 'concat (nreverse text-parts))))

(defun fanfou-get-attrib-node (node attrib)
  "Get the text of a child attribute node.
If the children of XML node NODE are formatted like
<attrib1>data</attrib1> <attrib2>data</attrib2> ... then this
fuction will return the text of the child node named ATTRIB or
nil if it isn't found."
  (let ((child (xml-get-children node attrib)))
    (if (consp child)
        (fanfou-get-node-text (car child))
      nil)))

(defun fanfou-reply-button-pressed (button)
  "Calls fanfou-reply for the position where BUTTON is."
  (fanfou-reply (overlay-start button)))

(defun fanfou-reply (pos)
  "Sets up a status edit buffer to reply to the message at POS.
fanfou-reply-status-id is set to the id of the status
corresponding to the status so that it will be marked as a
reply. The status' screen name is initially entered into the
buffer.

When called interactively POS is set to point."
  (interactive "d")
  (let ((status-screen-name (get-text-property pos 'fanfou-status-screen-name))
        (status-id (get-text-property pos 'fanfou-status-id)))
    (when (null status-screen-name)
      (error "Missing screen name in status"))
    (when (null status-id)
      (error "Missing status id"))
    (fanfou-status-edit)
    (setq fanfou-reply-status-id status-id)
    (insert "@" status-screen-name " ")))

(defun fanfou-show-error (doc)
  "Show a Fanfou error message.
DOC should be the XML parsed document returned in the error
message. If any information about the error can be retrieved it
will also be displayed."
  (insert "An error occured while trying to process a Fanfou request.\n\n")
  (let (error-node)
    (if (and (consp doc)
             (consp (car doc))
             (eq 'hash (caar doc))
             (setq error-node (xml-get-children (car doc) 'error)))
        (insert (fanfou-get-node-text (car error-node)))
      (xml-print doc))))

(defun fanfou-format-time-for-display (time)
  "Convert TIME to a friendly human readable string.
TIME should be a high/low pair as returned by encode-time."
  ;; This is based on a similar function from Tweet
  (let* ((now (current-time))
         (age (subtract-time now time))
         (age-days (- (time-to-days now) (time-to-days time))))
    (if (or (< (car age) 0)
            (>= (car age) 16) ; more than about 12 days
            (>= age-days 7))
        (format-time-string "%x at %H:%M" time)
      (let* ((age-seconds (logior (lsh (car age) 16) (cadr age)))
             (age-minutes (/ age-seconds 60))
             (age-hours (/ age-minutes 60)))
        (cond ((< age-seconds 60)
               "Less than a minute ago")
              ((<= age-minutes 1)
               "About a minute ago")
              ((< age-minutes 60)
               (format "About %d minutes ago" age-minutes))
              ((<= age-hours 1)
               "About an hour ago")
              ((< age-minutes 360)
               (format "About %d hours ago" age-hours))
              ((<= age-days 0)
               (format-time-string "Today at %H:%M" time))
              ((<= age-days 1)
               (format-time-string "Yesterday at %H:%M" time))
              (t
               (format-time-string "Last %A at %H:%M" time)))))))

(defun fanfou-compile-format-string (format-string)
  "Converts FORMAT-STRING into a list that is easier to scan.
See fanfou-status-format for a description of the format. The
returned list contains elements that are one of the following:

- A string. This should be inserted directly into the buffer.

- A four element list like (RIGHT-PAD WIDTH COMMAND
  PROPERTIES). RIGHT-PAD is t if the - flag was specified or nil
  otherwise. WIDTH is the amount to pad the string to or nil if
  no padding was specified. COMMAND is an integer representing
  the character code for the command. PROPERTIES is a list of
  text properties that should be applied to the resulting
  string."
  (let (parts last-point)
    (with-temp-buffer
      (insert format-string)
      (goto-char (point-min))
      (setq last-point (point))
      (while (re-search-forward "%\\(-?\\)\\([0-9]*\\)\\([a-zA-Z%]\\)" nil t)
        ;; Push the preceeding string (if any) to copy directly into
        ;; the buffer
        (when (> (match-beginning 0) last-point)
          (push (buffer-substring last-point (match-beginning 0)) parts))
        ;; Make the three element list describing the command
        (push (list (> (match-end 1) (match-beginning 1)) ; is - flag given?
                    (if (> (match-end 2) (match-beginning 2)) ; is width given?
                        (string-to-number (match-string 2)) ; extract the width
                      nil) ; otherwise set to nil
                    ;; copy the single character for the command number directly
                    (char-after (match-beginning 3))
                    ;; extract all of the properties so they can be
                    ;; copied into the final string
                    (text-properties-at (match-beginning 0)))
              parts)
        ;; Move last point to the end of the last match
        (setq last-point (match-end 0)))
      ;; Add any trailing text
      (when (< last-point (point-max))
        (push (buffer-substring last-point (point-max)) parts)))
    (nreverse parts)))

(defconst fanfou-status-commands
  '((?i . id)
    (?R . in_reply_to_status_id)
    (?S . in_reply_to_user_id)
    (?U . in_reply_to_screen_name)
    (?T . truncated))
  "Alist mapping format commands to XML nodes in the status element.")

(defconst fanfou-user-commands
  '((?n . name)
    (?u . screen_name)
    (?I . id)
    (?l . location)
    (?d . description)
    (?A . profile_image_url)
    (?L . url)
    (?F . followers_count)
    (?P . protected))
  "Alist mapping format commands to XML nodes in the user element.")

(defun fanfou-insert-status-part-for-command (status-node command)
  "Extract the string for COMMAND from STATUS-NODE and insert.
The command should be integer representing one of the characters
supported by fanfou-status-format."
  (let ((user-node (car (xml-get-children status-node 'user))))
    (cond ((= command ?t)
           (let ((val (fanfou-get-attrib-node status-node 'created_at)))
             (when val
               (cond ((stringp fanfou-time-format)
                      (insert (format-time-string fanfou-time-format
                                                  (fanfou-time-to-time val))))
                     ((functionp fanfou-time-format)
                      (insert (funcall fanfou-time-format
                                       (fanfou-time-to-time val))))
                     ((null fanfou-time-format)
                      (insert val))
                     (t (error "Invalid value for fanfou-time-format"))))))
          ((= command ?r)
           (insert-button "reply"
                          'action 'fanfou-reply-button-pressed))
          ((or (= command ?m) (= command ?M))
           (let ((val (fanfou-get-attrib-node status-node 'text)))
             (when val
               (if (= command ?M)
                   (fill-region (prog1 (point) (insert val)) (point))
                 (insert val)))))
          ((= command ?s)
           (let ((val (fanfou-get-attrib-node status-node 'source)))
             (when val
               (with-temp-buffer
                 (insert val)
                 (setq val (fanfou-get-node-text
                            (car (xml-parse-region (point-min) (point-max))))))
               (when val
                 (insert val)))))
          ((= command ?%)
           (insert ?%))
          (t
           (let (val elem)
             (cond ((setq elem (assoc command fanfou-user-commands))
                    (setq val (fanfou-get-attrib-node
                               user-node (cdr elem))))
                   ((setq elem (assoc command fanfou-status-commands))
                    (setq val (fanfou-get-attrib-node
                               status-node (cdr elem)))))
             (when val
               (insert val)))))))

(defun fanfou-format-status-node (status-node format)
  "Insert the contents of a Fanfou status node.
The status is formatted with text properties according to FORMAT
and insterted into the current buffer. FORMAT should be a
compiled format string as returned by
fanfou-compile-format-string."
  (let ((status-begin (point)))
    (let (elem this-id)
      (setq elem (assoc ?i fanfou-status-commands))
      (setq this-id (fanfou-get-attrib-node
		     status-node (cdr elem)))

      (if (string= fanfou-last-id-saved this-id)
	  (progn (insert fanfou-new-tweets-sep)
		 (newline)))

      (if (not fanfou-last-id-seen)
	  (setq fanfou-last-id-seen this-id)))

    (while format
      (if (stringp (car format))
          (insert (car format))
        (let ((part-start (point))
              (right-pad (caar format))
              (padding (cadar format))
              (command (caddar format))
              (properties (nth 3 (car format))))
          (fanfou-insert-status-part-for-command status-node command)
          (when (and padding
                     (< (- (point) part-start) padding))
            (setq padding (make-string
                           (+ padding (- part-start (point))) ? ))
            (if right-pad
                (insert padding)
              (let ((part-end (point)))
                (goto-char part-start)
                (insert padding)
                (goto-char (+ part-end (length padding))))))
          (add-text-properties part-start (point) properties)))
      (setq format (cdr format)))
    (let ((user-node (car (xml-get-children status-node 'user))))
      (add-text-properties status-begin (point)
                           `(fanfou-status-screen-name
                             ,(fanfou-get-attrib-node user-node 'screen_name)
                             fanfou-status-id
                             ,(fanfou-get-attrib-node status-node 'id))))))

(defun fanfou-remove-duplicate-statuses (a b)
  "Destructively modifies A to removes statuses that are also in B.
The new head of A is returned."
  (let (last (na a) nb)
    (while na
      (setq nb b)
      ;; Looking for a matching node in b
      (if (catch 'found
            (while nb
              (if (string-equal (fanfou-get-attrib-node (car na) 'id)
                                (fanfou-get-attrib-node (car nb) 'id))
                  (throw 'found t))
              (setq nb (cdr nb)))
            nil)
          ;; If we found one then skip this node
          (if last
              (setcdr last (cdr na))
            (setq a (cdr na)))
        (setq last na))
      (setq na (cdr na))))
  a)

(defun fanfou-merge-status-lists (a b)
  "Merge the two fanfou status lists.
The lists should be just the status nodes from the parsed XML
output. They are interleaved so that the resulting list is still
sorted by time. Duplicate entries are removed. The resulting list
is then returned."
  ;; Remove duplicates from a
  (setq a (fanfou-remove-duplicate-statuses a b))

  (let (result)
    (while (cond ((null a) ; have we reached the end of a?
                  ;; return result + b
                  (setq result (nconc (nreverse result) b))
                  nil)
                 ((null b) ; have we reached the end of b?
                  ;; return result + a
                  (setq result (nconc (nreverse result) a))
                  nil)
                 ((fanfou-status-time-lessp (car a) (car b))
                  ;; choose b
                  (push (car b) result)
                  (setq b (cdr b))
                  t)
                 (t
                  ;; choose a
                  (push (car a) result)
                  (setq a (cdr a))
                  t)))
    result))

(defun fanfou-status-time-lessp (a b)
  "Return whether the time stamp of status node A is < B."
  (time-less-p (fanfou-time-to-time (fanfou-get-attrib-node
                                      a 'created_at))
               (fanfou-time-to-time (fanfou-get-attrib-node
                                      b 'created_at))))

(defun fanfou-time-to-time (time)
  "Convert TIME to a number of seconds since some epoch."
  (let ((case-fold-search t))
    (if (null (string-match (concat "\\`[a-z]\\{3\\} "
                                    "\\([a-z]\\{3\\}\\) "
                                    "\\([0-9]\\{1,2\\}\\) "
                                    "\\([0-9]\\{2\\}\\):"
                                    "\\([0-9]\\{2\\}\\):"
                                    "\\([0-9]\\{2\\}\\) "
                                    "\\([+-][0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\) "
                                    "\\([0-9]\\{4\\}\\)\\'") time))
        (error "Invalid time string: %s" time))
    (encode-time (string-to-number (match-string 5 time))
                 (string-to-number (match-string 4 time))
                 (string-to-number (match-string 3 time))
                 (string-to-number (match-string 2 time))
                 (cdr (assoc (match-string 1 time) fanfou-month-map))
                 (string-to-number (match-string 8 time))
                 (concat (match-string 6 time) ":" (match-string 7 time)))))

(defun fanfou-status-get-string ()
   "Get the contents of the current buffer as a string.
All groups of spaces in the string are replaced with a single
space."
   (let ((other-buffer (current-buffer)))
     (with-temp-buffer
       (insert-buffer-substring-no-properties other-buffer)
       (goto-char (point-min))
       (while (re-search-forward "[\n\t ]+" nil t)
         (replace-match " " t t))
       (buffer-substring (point-min) (point-max)))))

(defun fanfou-status-post ()
  "Update your Fanfou status.
The contents of the current buffer are used for the status. The
current buffer is then killed. If there is too much text in the
buffer then you will be asked for confirmation.

If the fanfou-reply-status-id variable is not nil then this will
be sent to mark the status as a reply. The reply button on the
status list automatically sets that varaible."
  (interactive)
  (when (or (<= (buffer-size) fanfou-maximum-status-length)
            (y-or-n-p (format (concat "The message is %i characters long. "
                                      "Are you sure? ") (buffer-size))))
    (message "Sending status...")
	;; (fanfou-status-get-string)
    ;; (setq msgcontent (url-hexify-string
	;; 	      (fanfou-status-get-string)))
    ;; (message msgcontent)
    (let ((url-request-method "POST")
	  (url-request-extra-headers `(("Content-Type" . "application/x-www-form-urlencoded")))
	  ;; TODO Need to add "status" and value to oauth-post-var-alist
	  (oauth-post-vars-alist (cons (cons "status" (fanfou-status-get-string)) '()))
	  (url-request-data (concat "status=" (url-hexify-string
										   (fanfou-status-get-string)))))
      (when fanfou-reply-status-id
	;; TODO also need to add "in_reply_to_status_id" and value to oauth-post-vars-alist
        (setq url-request-data (concat url-request-data
                                       "&in_reply_to_status_id="
                                       fanfou-reply-status-id))
	(push (cons "in_reply_to_status_id" fanfou-reply-status-id) oauth-post-vars-alist))
      (fanfou-retrieve-url fanfou-status-update-url
			   'fanfou-status-callback))))

(defun fanfou-status-callback (status)
  "Function called after Fanfou status has been sent."
  (let ((errmsg (plist-get status :error)))
    (when errmsg
      (signal (car errmsg) (cdr errmsg)))
    (fanfou-kill-status-buffer)
    (message "Succesfully updated Fanfou status.")))

(defun fanfou-kill-status-buffer ()
  "Kill the *Fanfou Status* buffer and restore the previous
frame configuration."
  (interactive)
  (kill-buffer "*Fanfou Status*")
  (set-frame-configuration fanfou-frame-configuration))

(defun fanfou-status-edit ()
  "Edit your fanfou status in a new buffer.
A new buffer is popped up in a special edit mode. Press
\\[fanfou-status-post] when you are finished editing to send the
message."
  (setq fanfou-frame-configuration (current-frame-configuration))
  (interactive)
  (pop-to-buffer "*Fanfou Status*")
  (fanfou-status-edit-mode))

(defun fanfou-status-edit-update-length ()
  "Updates the character count in Fanfou status buffers.
This should be run after the text in the buffer is changed. Any
characters after the maximum status update length are
hightlighted in the face fanfou-status-overlong-face and the
character count on the mode line is updated."
  ;; Update the remaining characters in the mode line
  (let ((remaining (- fanfou-maximum-status-length
                      (buffer-size))))
    (setq fanfou-status-edit-remaining-length
          (concat " "
                  (if (>= remaining 0)
                      (number-to-string remaining)
                    (propertize (number-to-string remaining)
                                'face 'fanfou-status-overlong-face))
                  " ")))
  (force-mode-line-update)
  ;; Highlight the characters in the buffer that are over the limit
  (if (> (buffer-size) fanfou-maximum-status-length)
      (let ((start (+ (point-min) fanfou-maximum-status-length)))
        (if (null fanfou-status-edit-overlay)
            (overlay-put (setq fanfou-status-edit-overlay
                               (make-overlay start (point-max)))
                         'face 'fanfou-status-overlong-face)
          (move-overlay fanfou-status-edit-overlay
                        start (point-max))))
    ;; Buffer is not too long so just hide the overlay
    (when fanfou-status-edit-overlay
      (delete-overlay fanfou-status-edit-overlay))))

(defun fanfou-status-edit-after-change (begin end old-size)
  (fanfou-status-edit-update-length))

(define-derived-mode fanfou-status-edit-mode text-mode "Fanfou Status Edit"
  "Major mode for updating your Fanfou status."
  ;; Schedule to update the character count after altering the buffer
  (make-local-variable 'after-change-functions)
  (add-hook 'after-change-functions 'fanfou-status-edit-after-change)
  ;; Add the remaining character count to the mode line
  (make-local-variable 'fanfou-status-edit-remaining-length)
  ;; Copy the mode line format list so we can safely edit it without
  ;; affecting other buffers
  (setq mode-line-format (copy-sequence mode-line-format))
  ;; Add the remaining characters variable after the mode display
  (let ((n mode-line-format))
    (catch 'found
      (while n
        (when (eq 'mode-line-modes (car n))
          (setcdr n (cons 'fanfou-status-edit-remaining-length
                          (cdr n)))
          (throw 'found nil))
        (setq n (cdr n)))))
  ;; Make a buffer-local reference to the overlay for overlong
  ;; messages
  (make-local-variable 'fanfou-status-edit-overlay)
  ;; A buffer local variable for the reply id. This is filled in when
  ;; the reply button is pressed
  (make-local-variable 'fanfou-reply-status-id)
  (setq fanfou-reply-status-id nil)
  ;; Update the mode line immediatly
  (fanfou-status-edit-update-length))

(define-derived-mode fanfou-timeline-view-mode fundamental-mode
  "Fanfou Timeline"
  "Major mode for viewing timelines from Fanfou.")

(provide 'fanfou)

