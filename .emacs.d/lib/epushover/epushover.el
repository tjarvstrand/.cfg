;; Copyright 2012 Thomas Järvstrand <tjarvstrand@gmail.com>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(defcustom epushover-token nil
  "The pushover application token")

(defcustom epushover-user-key nil
  "The pushover user-key")

(defun epushover-notify (title msg)
  (let ((post-data (epushover-post-data title msg)))
    (message "post-data %s" post-data)
    (start-process "epushover"
                   nil ;; no buffer
                   "wget"
                   "--output-document=-" ;; send to stdout
                   "--no-verbose"
                   (format "--post-data=%s" post-data)
                   "https://api.pushover.net/1/messages.json")))

(defun epushover-post-data (title msg)
  (format "token=%s&user=%s&title=%s&message=%s"
          epushover-token
          epushover-user-key
          title
          msg))

(provide 'epushover)
