;;; copilot-lsp --- lsp-mode client  copilot-lsp  -*- lexical-binding: t -*-

;; Copyright (C) 2024 Rodrigo Virote Kassick

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; Author: Rodrigo Virote Kassick <kassick@gmail.com>
;; Version: 0.1
;; Package-Requires: (lsp-mode secrets s compile dash cl-lib request company)
;; Keywords: lsp-mode, generative-ai, code-assistant
;; URL: https://github.com/kassick/copilot-lsp.el

;; Commentary:

;; LSP client for copilot-lsp -- https://gitlab.com/gitlab-org/editor-extensions/copilot-lsp

;; Code:

(require 'lsp-mode)
(require 'cl-lib)
(require 'secrets)
(require 's)
(require 'compile)
(require 'dash)
(require 'request)
(require 'company)

(lsp-dependency 'copilot-lsp
                '(:system "copilot-lsp")
                '(:npm :package "copilot-node-server"
                       :path "language-server.js"))

;; (lsp-register-custom-settings
;;  '(
;;    ;; TODO: add these as custom variables
;;    ("copilot-lsp.logLevel" "debug")
;;    ("copilot-lsp.telemetry.enabled" json-false)
;;    ))

(defgroup copilot-lsp ()
  "Copilot LSP configuration"
  ;; :group 'lsp-mode
  :tag "Copilot LSP"
  :link '(url-link "https://www.npmjs.com/package/copilot-node-server"))

(defcustom copilot-lsp-major-modes '(python-mode
                                     python-ts-mode
                                     go-mode
                                     go-ts-mode
                                     js-mode
                                     js-ts-mode
                                     java-mode
                                     java-ts-mode
                                     kotlin-mode
                                     kotlin-ts-mode
                                     ruby-mode
                                     ruby-ts-mode
                                     rust-mode
                                     rust-ts-mode
                                     tsx-ts-mode
                                     typescript-mode
                                     typescript-ts-mode
                                     vue-mode
                                     yaml-mode
                                     yaml-ts-mode)
  "The major modes for which copilot-lsp should be used"
  :type '(repeat symbol)
  :group 'copilot-lsp)

(defun copilot-lsp--client-active-for-mode-p (fname mode)
  (and copilot-lsp-enabled (member mode copilot-lsp-major-modes)))

(defun copilot-lsp--find-active-workspaces ()
  "Returns a list of copilot-lsp workspaces"
  (-some->> (lsp-session)
    (lsp--session-workspaces)
    (--filter (member (lsp--client-server-id (lsp--workspace-client it))
                      '(copilot-lsp copilot-lsp-remote)))))

(defun copilot-lsp--set-enabled-value (symbol value)
  (when (not (and (boundp symbol)
                  (equal (symbol-value symbol) value)))
    (set symbol value)
    (if value
        ;; Restart lsp on all relevant buffers
        (cl-loop for buf in (buffer-list) do
                 (with-current-buffer buf
                   (when (and
                          ;; Ignore internal buffers
                          (not (string-prefix-p " " (buffer-name)))

                          ;; Only buffer vising files
                          (buffer-file-name)

                          ;; only for the modes where we should activate copilot-lsp for
                          (copilot-lsp--client-active-for-mode-p (buffer-file-name)
                                                                 major-mode)

                          ;; only if the client isn't already running
                          (--none? (lsp-find-workspace it (buffer-file-name)) '(gitab-lsp copilot-lsp-remote)))
                     (lsp--warn "Starting copilot-lsp LSP for mode %S on %s" major-mode (lsp-workspace-root))
                     (lsp))))

      ;; Globally stop all LSP servers
      (cl-loop for workspace in (copilot-lsp--find-active-workspaces) do
               (lsp--warn "Stopping copilot-lsp for %s per user request" (lsp--workspace-print workspace))
               (with-lsp-workspace workspace (lsp-workspace-restart workspace))))))

(defcustom copilot-lsp-enabled t
  "Whether the server should be started to provide completions.
This setting should be set with setopt or via customize.

(setopt copilot-lsp-enabled t)"
  :type 'boolean
  :group 'copilot-lsp
  :set #'copilot-lsp--set-enabled-value)


(defcustom copilot-lsp-langserver-command-args '("--stdio")
  "Command to start copilot-langserver."
  :type '(repeat string)
  :group 'copilot-lsp)

(defcustom copilot-lsp-executable "copilot-lsp"
  "The system-wise executable of copilot-lsp.
When this executable is not found, you can stil use
lsp-install-server to fetch an emacs-local version of the LSP."
  :type 'string
  :group 'copilot-lsp)




(lsp-interface
 (SignInInitiateResponse (:status :userCode :verificationUri :expiresIn :interval :user) nil)
 (SignInConfirmResponse (:status :user))
 (CheckStatusResponse (:status :user)))

(defun copilot-lsp-authenticated-as ()
  "Returns nil when not authorized; otherwise, the user name"
  (-if-let (workspace (--some (lsp-find-workspace it (buffer-file-name))
                              '(copilot-lsp copilot-lsp-remote)))
      (-if-let (checkStatusResponse (with-lsp-workspace workspace
                                      (lsp-request "checkStatus" '(:dummy "dummy"))))
          (-let* (((&CheckStatusResponse? :status :user) checkStatusResponse))
            (unless (s-present-p status)
              (error "No status in response %S" checkStatusResponse))
            ;; Result:
            (when (s-equals-p status "OK")
              user))
        (error "No response from the LSP server"))
    (error "No copilot-lsp workspace found!")))

;;;###autoload
(defun copilot-lsp-check-status ()
  (interactive)

  (condition-case err
      (progn
        (let ((user (copilot-lsp-authenticated-as)))
          (if user
              (message "Authenticated as %s" user)
            (user-error "Not Authenticated"))))
    (t (user-error "Error checking status: %s" err))))


;;;###autoload
(defun copilot-lsp-login ()
  (interactive)

  (-when-let (workspace (--some (lsp-find-workspace it) '(copilot-lsp copilot-lsp-remote)))
    (with-lsp-workspace workspace
      (-when-let* ((response (lsp-request "signInInitiate" '(:dummy "dummy"))))
        (-let (((&SignInInitiateResponse? :status :user-code :verification-uri :expires-in :interval :user) response))

          ;; Bail if already signed in
          (when (s-equals-p status "AlreadySignedIn")
            (user-error "Already signed in as %s" user))

          (if (display-graphic-p)
              (progn
                (gui-set-selection 'CLIPBOARD user-code)
                (read-from-minibuffer (format "Your one-time code %s is copied. Press \
ENTER to open GitHub in your browser. If your browser does not open \
automatically, browse to %s." user-code verification-uri))
                (browse-url verification-uri)
                (read-from-minibuffer "Press ENTER if you finish authorizing."))
            ;; Console:
            (read-from-minibuffer (format "First copy your one-time code: %s. Press ENTER to continue." user-code))
            (read-from-minibuffer (format "Please open %s in your browser. Press ENTER if you finish authorizing." verification-uri)))

          (message "Verifying...")
          (-let* ((confirmResponse (lsp-request "signInConfirm" (list :userCode user-code)))
                  ((&SignInConfirmResponse? :status :user) confirmResponse))
            (when (s-equals-p status "NotAuthorized")
              (user-error "User %s is not authorized" user))
            (message "User %s is authorized: %s" user status))

          ;; Do we need to confirm?
          (-let* ((checkStatusResponse (lsp-request "checkStatus" '(:dummy "dummy")))
                  ((&CheckStatusResponse? :status :user) checkStatusResponse))
            (when (s-equals-p status "NotAuthorized")
              (user-error "User %s is not authorized" user))

            (message "Authenticated as %s" user)))))))




(defun copilot-lsp--server-initialization-options ()
  ;; TODO: These values?
  ;; COPILOT_AGENT_VERBOSE=1?
  "
        configuration:[
{title:\"Copilot\",
 properties:{
   \"github.copilot.advanced\":{type:\"object\",title:\"Advanced Settings\",
   properties: {
     authProvider:{
       type:\"string\",enum:[\"github\",\"github-enterprise\"],enumDescriptions:[\"GitHub.com\",\"GitHu
        b Enterprise\"],default:\"github\",description:\"The GitHub identity to use
         for Copilot\"},
    \"debug.overrideEngine\":{type:\"string\",default:\"\",descrip
        tion:\"Override engine name\"},
    \"debug.overrideProxyUrl\":{type:\"string\",de
        fault:\"\",description:\"Override GitHub authentication proxy full URL\"},
    \"debug.testOverrideProxyUrl\":{type:\"string\",default:\"\",description:\"Over
        ride GitHub authentication proxy URL when running tests\"},
    \"debug.overrideCapiUrl\":{type:\"string\",default:\"\",description:\"Override GitHub Copil
        ot API full URL\"},
    \"debug.testOverrideCapiUrl\":{type:\"string\",default:\"\"
        ,description:\"Override GitHub Copilot API URL when running tests\"},
    \"debug.filterLogCategories\":{type:\"array\",default:[],description:\"Show only
         log categories listed in this setting. If an array is empty, show all
        loggers\"}}},
    \"github.copilot.enable\":{
      type:\"object\",
      default:{
        \"*\":!0,
        plaintext:!1,
        markdown:!1,
        scminput:!1
      },
      additionalProperties:\"boolean\",markdownDescription:\"Enable or disable Copilot completions for specified [lan
        guages](https://code.visualstudio.com/docs/languages/identifiers)\"
    },
    \"github.copilot.inlineSuggest.enable\":{type:\"boolean\",default:!0,deprecati
        onMessage:\"Deprecated: Please use github.copilot.editor.enableAutoCompl
        etions instead.\",description:\"Show inline suggestions\"},
    \"github.copilot.editor.enableAutoCompletions\":{
      type:\"boolean\",scope:\"language-overridable\",default:!0,description:\"Automatically show inline completions\"}}}]

"
  ;; Trying to replicate Copilot.vim initialization here ...
  (list :editorInfo (list :name "emacs" :version (symbol-value 'emacs-version))
        :editorPluginInfo (list :name "copilot-lsp" :version "1.38.0")
        :editorConfig (list :enableAutoCompletions t
                            :disabledLanguages '())
        :name "emacs"
        :version "0.1.0"))

(defun copilot-lsp--server-initialized-fn (workspace)
  (unless (copilot-lsp-authenticated-as)
    (copilot-lsp-login))

  ;; Copilot-lsp places the executionCommandProvider under workspace -- this
  ;; is wrong according to the spec
  (-when-let* ((workspace (--some (lsp-find-workspace it) '(copilot-lsp copilot-lsp-remote)))
               (caps (lsp--workspace-server-capabilities workspace))
               (_ (not (lsp:server-capabilities-execute-command-provider? caps)))
               (workspaceEntry (ht-get (lsp--workspace-server-capabilities workspace) "workspace"))
               (executionCommandProvider (ht-get workspaceEntry "executeCommandProvider"))
               )
    (lsp:set-server-capabilities-execute-command-provider? caps executionCommandProvider))

  ;; TODO: Initialization! What values does it need?
  ;; (let* (
  ;;        (config-ht (ht-get (lsp-configuration-section "copilot-lsp")
  ;;                           "copilot-lsp"))

  ;;        )

  ;; Do we need to send anything here?

  ;; (with-lsp-workspace workspace
  ;;   (lsp--set-configuration config-ht)))
  )

;; Server installed by emacs
(lsp-register-client
 (make-lsp-client
  :server-id 'copilot-lsp
  :new-connection (lsp-stdio-connection (lambda ()
                                          `(
                                            "node"
                                            ,(-if-let (candidates (directory-files-recursively
                                                                   (f-join lsp-server-install-dir "npm" "copilot-node-server")
                                                                   "^language-server.js$"))
                                                 (car candidates)
                                               (error "language-server.js not found"))
                                            ,@copilot-lsp-langserver-command-args)))
  :activation-fn #'copilot-lsp--client-active-for-mode-p
  :multi-root nil
  :priority -2
  :add-on? t
  :completion-in-comments? t
  :initialization-options #'copilot-lsp--server-initialization-options
  :initialized-fn #'copilot-lsp--server-initialized-fn
  :download-server-fn (lambda (_client callback error-callback _update?)
                        (lsp-package-ensure 'copilot-lsp callback error-callback))
  :notification-handlers (lsp-ht
                          ("$/progress" (lambda (&rest args) (message "$/progress with %S" args)))
                          ("featureFlagsNotification" (-lambda (workspace (&hash "rt" rt "sn" sn "chat" chat))
                                                        (lsp-message "Feature flags: Chat=%S rt=%S sn=%S" chat rt sn)))
                          ("statusNotification" (lambda (workspace status)
                                                  ;; status is ht "status" "inProgress" "message" ""
                                                  ;; status is ht "status" "Normal" "message" ""
                                                  nil))
                          ("window/logMessage" (-lambda (workspace (&MessageParams :type :message))
                                                 (if (< type 2)
                                                     ;; Error, show as message
                                                     (message message)
                                                   ;; warnings, logs, debugs, etc.
                                                   (lsp-message message))
                                                 ))
                          ("conversation/preconditionsNotification" #'ignore))

  :action-handlers (lsp-ht
                    ("window/showMessageRequest" (lambda (&rest args) (message "window/showMessageRequest %S" args)))
                    ("window/showDocument" (lambda (&rest args) (message "window/showDocument %S" args)))
                    )))

;; ;; Server found in PATH
;; (lsp-register-client
;;  (make-lsp-client
;;   :server-id 'copilot-lsp-remote
;;   :remote? t
;;   :new-connection (lsp-stdio-connection (lambda ()
;;                                           (cons
;;                                            (executable-find copilot-lsp-executable)
;;                                            copilot-lsp-langserver-command-args)))
;;   :activation-fn #'copilot-lsp--client-active-for-mode-p
;;   :multi-root nil
;;   :priority -2
;;   :add-on? t
;;   :completion-in-comments? t
;;   :initialization-options #'copilot-lsp--server-initialization-options
;;   :initialized-fn #'copilot-lsp--server-initialized-fn
;;   :notification-handlers (lsp-ht ("$/gitlab/token/check" 'copilot-lsp-token-check-callback))
;;   :action-handlers (lsp-ht
;;                     ;; This results in a key error, so let's just ignore it ...
;;                     ("gitlab.ls.codeSuggestionAccepted" #'ignore))))



;;;###autoload
(defun copilot-lsp-enable ()
  "Enables copilot-lsp"
  (interactive)
  (setopt copilot-lsp-enabled t)
  (message "Server Enabled"))

;;;###autoload
(defun copilot-lsp-disable ()
  "Disables copilot-lsp"
  (interactive)
  (setopt copilot-lsp-enabled nil)
  (message "Server Disabled"))

;;;###autoload
(defun copilot-lsp-toggle ()
  "Tottle whether copilot-lsp is enabled"
  (interactive)

  (if copilot-lsp-enabled
      (copilot-lsp-disable)
    (copilot-lsp-enable)))

(provide 'copilot-lsp)
