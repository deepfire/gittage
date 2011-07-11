;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: DESIRE; Base: 10; indent-tabs-mode: nil; show-trailing-whitespace: t -*-
;;;
;;;  (c) copyright 2007-2011 by
;;;           Samium Gromoff (_deepfire@feelingofgreen.ru)
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA  02111-1307  USA.

(in-package :gittage)

;;; mini-dependencies:
(defun extract-delimited-substrings (string start-delim-string end-delim-char)
  (iter (with posn = 0)
        (for href-posn = (search start-delim-string string :start2 posn))
        (while href-posn)
        (for start-posn = (+ href-posn (length start-delim-string)))
        (for close-posn = (position end-delim-char string :start start-posn))
        (when close-posn
          (collect (subseq string start-posn close-posn))
          (setf posn (1+ close-posn)))
        (while close-posn)))

(defun princ-version-to-string (version)
  "Return a textual representation of VERSION."
  (flatten-path-list (mapcar #'princ-to-string version) nil nil "."))

(defun bump-version-component (version n mode)
  "Increase Nth component of VERSION, counting from tail, by one, or, when
TEN-CEILING is non-NIL, just enough to make the component rounded by 10."
  (let ((last (butlast version n)))
    (case mode
      (:incf (incf (lastcar last)))
      (:ten-ceiling (setf (lastcar last) (* 10 (ceiling (1+ (lastcar last)) 10))))
      (:mult-ten (setf (lastcar last) (* 10 (lastcar last)))))
    (append last (make-list n :initial-element 0))))

(defun next-version-variants (version)
  "Produce plausible variants of versions following VERSION,
in order of strictly decreasing likelihood."
  (remove-duplicates (iter (for i from 0 below (length version))
                           (collect (append version (list 1)))
                           (collect (bump-version-component version i :incf))
                           (unless (= i (1- (length version)))
                             (let ((10-bumped (bump-version-component version i :ten-ceiling)))
                               (collect 10-bumped)
                               (collect (bump-version-component version i :mult-ten))
                               (appending (iter (for j from 0 below (1+ i))
                                                (collect (butlast 10-bumped j)))))))
                     :test #'equal))

(defun invoke-with-file-from-www (filename url fn)
  (unwind-protect (with-explanation ("retrieving ~A" url)
                    (wget url "-O" filename)
                    (funcall fn))
    (delete-file filename)))

(defmacro with-file-from-www ((filename url) &body body)
  `(invoke-with-file-from-www ,filename ,url (lambda () ,@body)))

;;; gittage itself
(defvar *http-proxy* nil)

(define-executable (%git git)
    :may-want-display t
    :fixed-environment ("HOME=/tmp" "PAGER=/bin/cat" (when *http-proxy*
                                                       (strconcat* "http_proxy=" *http-proxy*))))

(defun git (work-tree &rest args)
  (declare (type (or string pathname) work-tree))
  (set-posix-working-directory work-tree)
  (apply #'%git args))

(define-condition vcs-condition ()
  ((vcs :reader condition-vcs :initarg :vcs)))

(define-condition git-condition (vcs-condition)
  ()
  (:default-initargs :vcs 'git))

(define-condition repository-condition (vcs-condition)
  ((pathname :reader condition-pathname :initarg :pathname)))

(define-condition git-error (error git-condition)  ())
(define-condition repository-error (git-error repository-condition) ())

(define-simple-error git-error)
(define-simple-error repository-error :object-initarg :pathname)

(define-reported-condition repository-not-clean-during-fetch (repository-error executable-failure) ()
  (:report (pathname)
           "~@<Repository in ~S has uncommitted changes during fetch.~:@>" pathname))
(define-reported-condition dirt-files-in-repository (repository-error)
  ((dirt-files :accessor condition-dirt-files :initarg :dirt-files))
  (:report (dirt-files pathname)
           "~@<Dirt files ~S obstructing operation in ~S.~@:>" dirt-files pathname))
(define-reported-condition empty-repository (repository-error)
  ()
  (:report (pathname) "~@<Repository in ~S has no objects.~:@>" pathname))
(define-reported-condition patch-failure (repository-error)
  ((output :reader condition-output :initarg :output))
  (:report (pathname output)
           "~@<Failed to apply patch in ~S. The output was:~%~A~%~:@>" pathname output))

;;;
(defvar *repository*)
(define-binder with-repository *repository*)

(defun missing-remote-error (name &optional (directory *repository*))
  (repository-error directory "~@<At ~S: remote ~A not found, and no URL was provided.~:@>"
                    directory name))

(defun extort-remote-url (name &optional (directory *repository*))
  (restart-case (missing-remote-error name directory)
    (specify (specified-url)
      :report "Specify URL for the missing remote."
      specified-url)))

;;;
;;; Repositories
;;;
(defun git-predicate (directory explanation-format-control git-arguments)
  (with-executable-options (:explanation `(,explanation-format-control ,directory) :output nil)
    (not (with-shell-predicate (apply #'git directory git-arguments)))))

(defmacro defgitpredicate (name (explanation-format-control) &body git-arguments)
  `(defun ,name (&optional (directory *repository*))
     (git-predicate directory ,explanation-format-control '(,@git-arguments))))

;; directory level
(defun directory-has-objects-p (directory)
  (not (null (or (directory (subfile directory '("objects" "pack" :wild) :type :wild))
                 (find-if (lambda (x) (= 2 (length (lastcar (pathname-directory x)))))
                          (directory (merge-pathnames "objects/*/" directory)))))))

(defun dotgit (directory)
  "Given a repository DIRECTORY, return the path to its git storage
directory."
  (merge-pathnames ".git/" directory))

(defun init-repo (directory)
  (ensure-directories-exist (dotgit directory))
  (git directory "init"))

;; repository level
(defun nonbare-repository-present-p (&optional (directory *repository*) &aux
                                         (dotgit (dotgit directory)))
  "See if repository in DIRECTORY and source code is available at LOCALITY."
  (and (fad:directory-exists-p directory)
       (fad:directory-exists-p dotgit)
       (directory-has-objects-p dotgit)))

(defun repository-bare-p (&optional (directory *repository*))
  "See if the DIRECTORY, which is known to contain a git repository is
bare or not."
  (null (fad:directory-exists-p (dotgit directory))))

(defun (setf repository-bare-p) (val &optional (directory *repository*) &aux
                                     (dotgit (dotgit directory)))
  (when val
    (git-error "~@<Couldn't make git repository at ~S bare: not implemented.~:@>" directory))
  (make-directory dotgit #+unix #o755)
  (dolist (filename (directory (merge-pathnames "*" directory)))
    (rename-to-directory filename dotgit))
  (setf (config-var 'core.bare directory) "false")
  (set-branch-index-tree nil directory)
  nil)

(defun repository-world-readable-p (&optional (directory *repository*))
  "See, whether or not repository within DIRECTORY allows itself to be
exported for the purposes of git-daemon."
  (fad:file-exists-p (merge-pathnames ".git/git-daemon-export-ok" directory)))

(defun (setf repository-world-readable-p) (val &optional (directory *repository*))
  "Change git-daemon's idea about world-readability of DIRECTORY."
  (let ((flag-path (merge-pathnames ".git/git-daemon-export-ok" directory)))
    (if val
        (open flag-path :direction :probe :if-does-not-exist :create)
        (when (fad:file-exists-p flag-path)
          (delete-file flag-path)))
    val))

(defgitpredicate repository-unstaged-changes-p ("determining whether git repository at ~S has unstaged changes")
  "diff" "--exit-code")

(defgitpredicate repository-staged-changes-p ("determining whether git repository at ~S has staged changes")
  "diff" "--exit-code" "--cached")

(defgitpredicate repository-changes-p ("determining whether git repository at ~S has staged or unstaged changes")
  "diff" "--exit-code" "--summary" "HEAD" "--")

(defun repository-status (&optional (directory *repository*))
  "Examine status of the git repository within DIRECTORY and return lists of pathnames as multiple values.
The lists of pathnames returned have following semantics:
    - staged modified,
    - staged deleted,
    - staged new,
    - unstaged modified,
    - unstaged deleted."
  (multiple-value-bind (status output)
      (with-executable-options (:explanation `("determining status of git repository at ~S" ,directory)
                                :output :capture)
        (with-shell-predicate (git directory "status")))
    (declare (ignore status))
    (with-input-from-string (s output)
      (flet ((seek-past-marker ()
               (iter (for line = (read-line s nil nil))
                     (unless line
                       (error "~@<Premature EOF while reading from 'git status'.~:@>"))
                     (while (not (string= line "#")))))
             (collect-prefixed ()
               (iter (for line = (read-line s nil nil))
                     (unless (starts-with-subseq "#	" line)
                       (finish))
                     (unless (starts-with-subseq #(#\# #\Tab) line)
                       (error "~@<Bad constituent ~S while reading from 'git status'.~:@>" line))
                     (collect (subseq line 2))))
             (process-entries (list)
               (iter (for entry in list)
                     (multiple-value-bind (modifid-p mod-suffix) (starts-with-subseq "new file:   " entry :return-suffix t)
                       (multiple-value-bind (deled-p del-suffix) (starts-with-subseq "deleted:    " entry :return-suffix t)
                         (multiple-value-bind (new-p new-suffix) (starts-with-subseq "modified:   " entry :return-suffix t)
                           (cond (modifid-p (collect mod-suffix into new))
                                 (deled-p   (collect del-suffix into deleted))
                                 (new-p     (collect new-suffix into modified))
                                 (t (error "~@<Bad constituent ~S while reading from 'git status'.~:@>" entry))))))
                     (finally (return (values modified deleted new))))))
        (iter outer
              (with staged-modified) (with staged-deleted) (with staged-new)
              (with unstaged-modified) (with unstaged-deleted)
              (with untracked)
              (for line = (read-line s nil nil))
              (while line)
              (switch (line :test #'string=)
                ("# Changes to be committed:"
                 (seek-past-marker)
                 (setf (values staged-modified staged-deleted staged-new)
                       (process-entries (collect-prefixed))))
                ("# Changed but not updated:"
                 (seek-past-marker)
                 (setf (values unstaged-modified unstaged-deleted)
                       (process-entries (collect-prefixed))))
                ("# Untracked files:"
                 (seek-past-marker)
                 (setf untracked (collect-prefixed)))
                (t
                 (unless (or (starts-with-subseq "# On branch " line)
                             (string= line "nothing to commit (working directory clean)")
                             (string= line "nothing added to commit but untracked files present (use \"git add\" to track)"))
                   (error "~@<Unrecognised header ~S while reading from 'git status'.~:@>" line))))
              (finally (return-from outer (values staged-modified staged-deleted staged-new unstaged-modified unstaged-deleted untracked))))))))

(defun update-repository-for-dumb-servers (&optional (directory *repository*))
  (git directory "update-server-info"))

;;;
;;; Policies
;;;
;; policy:
;;   - named
;;   - attribute set
;;
(defclass named ()
  ((name :accessor name :initarg :name)))

(defclass repository-policy (named)
  ((unsaved-changes           :initarg :unsaved-changes           :type (or null (member :error :stash :commit)))
   (unsaved-changes-postwrite :initarg :unsaved-changes-postwrite :type (or null (member :error :stash :reset)))
   (missing-master            :initarg :missing-master            :type (or null (member :error :create-on-head)))
   (preexisting-gitless       :initarg :preexisting-gitless       :type (or null (member :error :take-over)))
   (drive-head                :initarg :drive-head                :type boolean)
   (drive-head-branch         :initarg :drive-head-branch         :type boolean)
   (reapply-stash             :initarg :reapply-stash             :type boolean)))

(defun make-repository-policy (name &rest initargs)
  (apply #'make-instance 'repository-policy :name name (remove-from-plist initargs :name)))

(defvar *repository-policies* (alist-hash-table
                               `((:default .  ,(make-repository-policy
                                                :default
                                                :unsaved-changes           :stash
                                                :unsaved-changes-postwrite :stash
                                                :missing-master            :create-on-head
                                                :preexisting-gitless       :take-over
                                                :drive-head                nil
                                                :drive-head-branch         nil
                                                :reapply-stash             nil))
                                 (:new-repo . ,(make-repository-policy
                                                :new-repo
                                                :unsaved-changes           :error
                                                :unsaved-changes-postwrite :stash
                                                :missing-master            :error
                                                :preexisting-gitless       :error
                                                :drive-head                t
                                                :drive-head-branch         t
                                                :reapply-stash             nil))
                                 (:cautious . ,(make-repository-policy
                                                :cautious
                                                :unsaved-changes           :error
                                                :unsaved-changes-postwrite :stash
                                                :missing-master            :error
                                                :preexisting-gitless       :error
                                                :drive-head                nil
                                                :drive-head-branch         nil
                                                :reapply-stash             t)))
                               :test 'eq))

(define-root-container *repository-policies* repository-policy :key-type symbol :coercer t)

(defvar *default-repository-policy*      (repository-policy :default))
(defvar *repository-policy*              *default-repository-policy*)

(defun invoke-with-repository-policy (policy fn)
  (let ((*repository-policy* (if policy
                                 (coerce-to-repository-policy policy)
                                 (or *repository-policy*
                                     (error "~@<Cannot bind *REPOSITORY-POLICY* to NIL.~:@>")))))
    (funcall fn)))

(defmacro with-repository-policy (policy &body body)
  `(invoke-with-repository-policy ,policy (lambda () ,@body)))

(defun repository-policy-value (value-name &aux (slot (find-symbol (string value-name) :gittage)))
  (unless (and slot (slot-exists-p *default-repository-policy* slot))
    (error "~@<Repository policies do not define policy ~S.~:@>" (string value-name)))
  (or (slot-value *repository-policy*         slot)
      (slot-value *default-repository-policy* slot)))

;;;;
;;;; Commits
;;;;
(defclass commit ()
  ((id :reader commit-id :initarg :id)
   (date :reader commit-date :initarg :date)
   (author :reader commit-author :initarg :author)
   (message :reader commit-message :initarg :message)))

(defclass git-commit (commit)
  ())

(defun make-commit (id date author message)
  (make-instance 'git-commit :id id :date date :author author :message message))

;;;
;;; Config variables
;;;
(defun config-var (var &optional directory)
  (declare (type symbol var))
  (with-executable-options (:explanation `("getting value of git variable ~A" ,(symbol-name var))
                            :output :capture)
    (multiple-value-bind (setp output) (with-shell-predicate (git (or directory ".") "config"
                                                                  (unless directory "--global")
                                                                  (string-downcase (symbol-name var))))
      (when setp
        (string-right-trim '(#\Return #\Newline) output)))))

(defun (setf config-var) (val var &optional directory)
  (declare (type symbol var) (type string val))
  (with-explanation ("setting git variable ~A to ~A" (symbol-name var) val)
    (git (or directory ".") "config"
         (unless directory "--global") "--replace-all"
         (string-downcase (symbol-name var)) val)))

;;;
;;; Remotes
;;;
(defun remotes (&optional (directory *repository*))
  (multiple-value-bind (status output)
      (with-executable-options (:explanation `("listing git remotes in ~S" ,directory)
                                :output :capture)
        (git directory "remote" "-v"))
    (declare (ignore status))
    (iter (for line in (split-sequence #\Newline (string-right-trim '(#\Return #\Newline) output)))
          (while (plusp (length line)))
          (destructuring-bind (name url &optional fetch-or-push)
              (split-sequence-if (eq-member-of* #\Space #\Tab) line :remove-empty-subseqs t)
            (when (or (not fetch-or-push)
                      (string= "(fetch)" fetch-or-push))
              (collect (make-keyword (string-upcase name)) into names)
              (collect url into urls)))
          (finally (return (values names urls))))))

(defun find-remote (name &optional (directory *repository*))
  (multiple-value-bind (remote-names urls) (remotes directory)
    (iter (for remote-name in remote-names)
          (for remote-url in urls)
          (finding remote-url such-that (string= name remote-name)))))

(defun add-remote (name url &optional (directory *repository*))
  (with-explanation ("adding a git remote ~A (~A) in ~S" name url directory)
    (git directory "remote" "add" (down-case-string name) url)))

(defun remove-remote (name &optional (directory *repository*))
  (with-explanation ("removing git remote ~A in ~S" name directory)
    (git directory "remote" "rm" (down-case-string name))))

(defun ensure-remote (name &optional url (directory *repository*))
  (let* ((found-url (find-remote name directory))
         (found-good-url-p (and found-url (if url
                                              (string/= url found-url)
                                              t))))
    (unless (or found-good-url-p url)
      (setf url (extort-remote-url name directory)))
    (when (and found-url (not found-good-url-p))
      (remove-remote name directory))
    (unless found-good-url-p
      (add-remote (down-case-string name) url directory))))

(defun fetch-remote (remote-name &optional (directory *repository*))
  (with-explanation ("fetching from git remote ~A in ~S" remote-name directory)
    (git directory "fetch" (down-case-string remote-name))))

;;;
;;; Ref names
;;;
(defun ref-shortp (ref)
  (endp (cdr ref)))

(defun canonicalise-ref (ref)
  (declare (type (or string cons) ref))
  (let ((ref (ensure-cons ref)))
    (if (ref-shortp ref)
        (cons "heads" ref)
        ref)))

(defun ref-headp (ref)
  (or (ref-shortp ref) (string= "heads" (first ref))))

(defun ref-remotep (ref)
  (and (not (ref-shortp ref)) (string= "remotes" (first ref))))

(defun make-remote-ref (remote-name branch)
  `("remotes" ,(down-case-string remote-name) ,(down-case-string branch)))

;;;
;;; Ref content
;;;
(defun parse-commit-id (string)
  (parse-integer string :radix #x10))

(defun parse-ref-value (string)
  (if (equalp (subseq string 0 5) "ref: ")
      (values (split-sequence #\/ (subseq string 5)))
      (values nil (parse-commit-id  string))))

(defun cook-ref-value (refvalue &optional prepend-refs)
  (etypecase refvalue
    (integer (format nil "~40,'0X" refvalue))
    (list (flatten-path-list (xform prepend-refs (curry #'cons "refs") refvalue)))))

;;;
;;; Ref files
;;;
(defun file-path-ref (pathname &aux (dir (pathname-directory pathname)))
  (destructuring-bind (ref repo-dir-head) (split-sequence ".git" (rest dir) :test #'equal)
    (values ref
            (make-pathname :directory (list* (first dir) repo-dir-head)))))

(defun ref-file-path (ref &optional (directory *repository*))
  (subfile directory (list* ".git" "refs" (canonicalise-ref ref))))

(defun ref-file-present-p (name &optional (directory *repository*))
  (probe-file (ref-file-path (xform-if-not #'listp #'down-case-string name) directory)))

(defun ref-file-value (pathname)
  (lret ((refval (parse-commit-id (file-as-string pathname))))
    (unless (not (minusp refval))
      (git-error "~@<Bad value in ref ~S: ~S.~:@>" pathname refval))))

(defun set-ref-file-value (ref directory value)
  (let ((path (ref-file-path ref directory)))
    (ensure-directories-exist path)
    (with-output-to-file (s path)
      (write-string (cook-ref-value value t) s))))

(defun head-pathnames (directory)
  (remove nil (directory (subwild directory `(".git" "refs" "heads") :name :wild :type :wild)
                         #+sbcl :resolve-symlinks #+sbcl nil)
          :key #'pathname-name))

(defun all-remote-head-pathnames (directory)
  (remove-if (lambda (p &aux (name (pathname-name p))) (or (null name) (string= name "HEAD")))
             (directory (subwild directory `(".git" "refs" "remotes") :name :wild :type :wild)
                        #+sbcl :resolve-symlinks #+sbcl nil)))

(defun remote-head-pathnames (remote directory)
  (remove-if (lambda (p &aux (name (pathname-name p))) (or (null name) (string= name "HEAD")))
             (directory (subwild directory `(".git" "refs" "remotes" ,(down-case-string remote)) :name :wild :type :wild)
                        #+sbcl :resolve-symlinks #+sbcl nil)))
;;;
;;; Raw refs & iteration
;;;
(defmacro do-packed-refs ((ref refval directory) &body body)
  (with-gensyms (packed-refs-path s line 1read-offt)
    `(let ((,packed-refs-path (subfile ,directory '(".git" "packed-refs"))))
       (when (probe-file ,packed-refs-path)
         (with-open-file (,s ,packed-refs-path)
           (let ((*read-eval* nil)
                 (*read-base* #x10))
             (iter (for ,line = (read-line ,s nil nil))
                   (while ,line)
                   (when (char= #\# (aref ,line 0)) (next-iteration))
                   (for (values ,refval ,1read-offt) = (read-from-string ,line nil nil))
                   (unless ,refval (next-iteration))
                   (for ,ref = (cdr (split-sequence #\/ (down-case-string (read-from-string ,line nil nil :start ,1read-offt)))))
                   ,@body)))))))

(defun map-packed-refs (predicate fn directory)
  (do-packed-refs (ref refval directory)
    (when (funcall predicate ref refval)
      (collect (funcall fn ref refval)))))

(defun map-pathnames-full-refs (fn pathnames)
  (iter (for pathname in pathnames)
        (collect (funcall fn (file-path-ref pathname) (ref-file-value pathname)))))

(defun map-heads (fn directory)
  (append (map-pathnames-full-refs fn (head-pathnames directory))
          (map-packed-refs (lambda (r v) (declare (ignore v)) (string= (first r) "heads")) fn directory)))

(defun map-all-remote-heads (fn directory)
  (append (map-pathnames-full-refs fn (all-remote-head-pathnames directory))
          (map-packed-refs (lambda (r v) (declare (ignore v)) (string= (first r) "remotes")) fn directory)))

(defun map-remote-heads (fn remote directory)
  (append (map-pathnames-full-refs fn (remote-head-pathnames remote directory))
          (map-packed-refs (lambda (r v) (declare (ignore v)) (and (string= (first r) "remotes") (string= (second r) remote))) fn directory)))

(defun refs-by-value (refval directory)
  (flet ((ref-if-= (r v) (when (= v refval) r)))
    (remove nil (append (map-heads #'ref-if-= directory)
                        (map-all-remote-heads #'ref-if-= directory)))))
;;;
;;; Refs
;;;
(defun ref-value (ref directory &key (if-does-not-exist :error))
  (let* ((ref (canonicalise-ref ref))
         (path (ref-file-path ref directory)))
    (if (probe-file path)
        (ref-file-value path)
        (or (car (remove nil (map-packed-refs (lambda (r v) (declare (ignore v)) (equal ref r))
                                              (lambda (r v) (declare (ignore r)) v)
                                              directory)))
            (ecase if-does-not-exist
              (:error (git-error "~@<Ref named ~S doesn't exist in git repository at ~S.~:@>"
                                 ref (or directory *repository*)))
              (:continue nil))))))

(defun ref-coerce-to-value (ref-or-value &optional (directory *repository*))
  (if (integerp ref-or-value)
      ref-or-value
      (ref-value ref-or-value directory)))

(defgeneric ref= (ref-x ref-y &optional directory)
  (:method (x y &optional (directory *repository*))
    (ref= (ref-coerce-to-value x directory) (ref-coerce-to-value y directory)))
  (:method ((x integer) (y integer) &optional (directory *repository*))
    (declare (ignore directory))
    (= x y)))

(defun symbolic-reffile-value (pathname &optional dereference)
  (multiple-value-bind (ref directory) (file-path-ref pathname)
    (declare (ignore ref))
    (multiple-value-bind (ref refval) (parse-ref-value (file-line pathname))
      (let ((normalised-ref (rest ref))) ; strip the "refs" component
        (if dereference
            (values (or refval
                        (ref-value normalised-ref directory :if-does-not-exist :continue)
                        (repository-error directory "~@<Reffile ~S references a missing ref ~S.~:@>"
                                          pathname normalised-ref))
                    normalised-ref)
            (or normalised-ref refval))))))

(defun set-symbolic-reffile-value (pathname value)
  (with-output-to-file (reffile pathname)
    (when (consp value)
      (write-string "ref: " reffile))
    (write-string (cook-ref-value value t) reffile)))

(defsetf symbolic-reffile-value set-symbolic-reffile-value)
;;;
;;; HEAD operation
;;;
(defun head-pathname (&optional (directory *repository*) remote)
  (subfile directory `(".git" ,@(when remote `("refs" "remotes" ,(down-case-string remote))) "HEAD")))

(defun get-head (&optional dereference (directory *repository*) remote)
  (symbolic-reffile-value (head-pathname directory remote) dereference))

(defun set-head (new-value &optional (directory *repository*) remote &aux
                 (path (head-pathname directory remote)))
  (declare (type (or cons (integer 0)) new-value))
  (prog1 (symbolic-reffile-value path nil)
    (set-symbolic-reffile-value path new-value)))

(defun detach-head (&optional (directory *repository*))
  (set-head (get-head t directory) directory))

(defun head-detached-p (&optional (directory *repository*) remote)
  (let ((head (get-head nil directory remote)))
    (not (or (integerp head)
             (ref-value head directory :if-does-not-exist :continue)))))

(defun invoke-with-maybe-detached-head (directory detachp fn)
  (if detachp
      (let ((current-head (get-head t directory)))
        (unwind-protect (progn (detach-head directory)
                               (funcall fn))
          (when current-head
            (set-head current-head directory))))
      (funcall fn)))

(defmacro with-maybe-detached-head ((&optional (directory '*repository) detachp) &body body)
  `(invoke-with-maybe-detached-head ,directory ,detachp  (lambda () ,@body)))

;;;
;;; Branches
;;;
(defun branches (&optional (directory *repository*))
  (multiple-value-bind (status output)
      (with-executable-options (:explanation `("listing git branches in ~S" ,directory)
                                :output :capture)
        (git directory "branch"))
    (declare (ignore status))
    (mapcar (compose #'make-keyword #'string-upcase)
            (remove-if
             (lambda (x) (or (zerop (length x)) (and (= 1 (length x)) (char= #\* (schar x 0)))))
             (mapcan (curry #'split-sequence #\Space)
                     (split-sequence #\Newline (string-right-trim '(#\Return #\Newline) output)))))))

(defun branch-present-p (name &optional (directory *repository*))
  (declare (symbol name))
  (or (ref-file-present-p name directory)
      (member name (branches directory) :test #'string=)))

(defun remove-branch (name &optional (directory *repository*))
  (with-explanation ("removing git branch ~A in ~S" name directory)
    (nth-value 0 (git directory "branch" "-d" (down-case-string name)))))

(defun set-noncurrent-branch (branchname &optional (directory *repository*) (refvalue (get-head t directory)))
  (declare (type (or list (integer 0)) refvalue))
  (with-explanation ("moving non-current ref ~A to ~:[~40,'0X~;~A~] in ~S"
                     branchname (consp refvalue) refvalue directory)
    (nth-value 0 (git directory "branch" "-f" (down-case-string branchname) (cook-ref-value refvalue)))))

(defun set-branch (name &optional (directory *repository*) (refvalue (get-head t directory)) possibly-current-p)
  (with-maybe-detached-head (directory possibly-current-p)
    (set-noncurrent-branch name directory refvalue)))

(defun set-branch-index-tree (&optional ref (directory *repository*))
  "Set the current branch, the index and the working tree to the state
at REF.  When REF is NIL, set the index and the working tree to the
ref of the current branch."
  (with-explanation ("hard-resetting repository in ~S~:[~; to ~:*~S~]" directory ref)
    (apply #'git directory "reset" "--hard" (when ref (list (flatten-path-list ref) "--")))))

(defun stash (&optional (directory *repository*))
  "Same as SET-BRANCH-INDEX-TREE with no arguments, but saves changes
in a temporary pseudo-commit."
  (with-explanation ("stashing changes in ~S" directory)
    (git directory "stash")))

(defun apply-stash (&optional (directory *repository*))
  "Apply stashed changes."
  (with-explanation ("applying stashed changes in ~S" directory)
    (git directory "stash" "apply")))

(defun ensure-clean-repository (if-changes &optional (directory *repository*))
  (with-retry-restarts ((hardreset-repository ()
                          :report "Clear all uncommitted changes, both staged and unstaged."
                          (set-branch-index-tree nil directory))
                        (stash-changes ()
                          :report "Stash all uncommitted changes, both staged and unstaged."
                          (stash directory)))
    (when (repository-changes-p directory)
      (ecase if-changes
        (:stash (stash directory))
        (:reset (set-branch-index-tree nil directory))
        (:error (git-error "~@<~:[Uns~;S~]taged changes in git repository ~S.~:@>"
                           (repository-staged-changes-p directory) directory))))))

(defun set-head-index-tree (ref &optional (if-changes :stash) (directory *repository*))
  (let ((ref (mapcar #'down-case-string (ensure-cons ref))))
    (ref-value ref directory)
    (ensure-clean-repository if-changes directory)
    (with-explanation ("checking out ~S in ~S" ref directory)
      (git directory "checkout" (flatten-path-list ref)))))

(defun invoke-with-branch-change (directory before-branch after-branch if-changes fn)
  (unwind-protect (progn
                    (set-head-index-tree before-branch if-changes directory)
                    (funcall fn))
    (set-head-index-tree after-branch if-changes directory)))

(defmacro with-branch-change ((before-branch-form after-branch-form &key (if-changes :stash) (directory '*repository*))
                              &body body)
  `(invoke-with-branch-change ,directory ,before-branch-form ,after-branch-form ,if-changes
                              (lambda () ,@body)))

(defun invoke-with-remote (remote-name fn
                              &key url (if-remote-does-not-exist :create) (directory *repository*))
  (unless (find-remote remote-name directory)
    (ecase if-remote-does-not-exist
      (:create (ensure-remote remote-name url directory))
      (:error  (missing-remote-error remote-name directory))))
  (funcall fn))

(defmacro with-remote ((remote &key (url nil urlp) (if-remote-does-not-exist nil if-remote-does-not-exist-p)
                                  (directory nil directoryp))
                          &body body)
  "Must be used with *REPOSITORY* bound."
  `(invoke-with-remote ,remote (lambda () ,@body) ,@(pass-&key* url if-remote-does-not-exist directory)))

;;;;
;;;; Repository-level operations
;;;;
(defun invoke-with-repository-write-access (path if-repository-does-not-exist if-gitless-nonempty
                                                fn &aux
                                                (dotgit (dotgit path))
                                                (*repository* path))
  (with-directory (path :if-does-not-exist :create)
    (let ((effectively-new (or (directory-created-p)
                               (directory-empty-p path))))
      (with-repository-policy (when (directory-created-p)
                                :new-repo)
        (handler-bind ((error (lambda (c)
                                (declare (ignore c))
                                ;; Maintain the no-useless-directories-added invariant.
                                ;; non-negotiable
                                (when (directory-created-p)
                                  (fad:delete-directory-and-files path))
                                #| Continue signalling. |#)))
          (cond
            (effectively-new
             (ecase if-repository-does-not-exist
               (:create    (git path "init"))
               (:continue  nil)))
            ((not (directory-has-objects-p dotgit))
             (ecase if-gitless-nonempty
               (:error     (error 'empty-repository :pathname path))
               (:take-over nil)))
            (t
             (ensure-clean-repository (repository-policy-value :unsaved-changes))))
          (funcall fn effectively-new))))))

(defmacro with-repository-write-access ((new-repo-p &key
                                                        (if-repository-does-not-exist :create)
                                                        (if-gitless-nonempty '(repository-policy-value :preexisting-gitless)))
                                            path &body body)
  (with-ignored-names ignores (_ new-repo-p)
    `(invoke-with-repository-write-access ,path ,if-repository-does-not-exist ,if-gitless-nonempty
                                              (lambda (,new-repo-p)
                                                ,@ignores
                                                ,@body))))

;;;;
;;;; Queries
;;;;
(defun commit-log (ref &optional (directory *repository*))
  "Given a REF, return the commit id, author, date and commit message
of the corresponding commit as multiple values."
  (multiple-value-bind (status output)
      (with-executable-options (:explanation `("querying commit log of ~S at ~S" ,ref ,directory)
                                :output :capture)
        (git directory "log" "-1" (cook-ref-value ref)))
    (declare (ignore status))
    (with-input-from-string (s output)
      (let (commit-id author date (posn 0))
        (iter (for line = (read-line s nil nil))
              (while (and line (plusp (length line))))
              (incf posn (1+ (length line)))
              (cond-let
                ((suffix (prefixp "commit " line))  (setf commit-id suffix))
                ((suffix (prefixp "Author: " line)) (setf author suffix))
                ((suffix (prefixp "Date:   " line)) (setf date suffix))))
        (unless (and commit-id author date)
          (git-error "~@<Error parsing commit log of ~X at ~S.~:@>" ref directory))
        (let ((message (string-right-trim '(#\Newline)
                                          (subseq output (+ 5 posn)))))
          (make-commit (parse-commit-id commit-id) date author message))))))

;;;;
;;;; Import
;;;;
(defun apply-diff (filename &optional (directory *repository*) (add-to-index t) (error-on-failure t))
  (multiple-value-bind (successp output) (with-explanation ("applying gitdiff ~S in ~S" filename directory)
                                           (with-shell-predicate
                                               (apply #'git directory "apply"
                                                      filename (append (when add-to-index '("--index"))))))
    (cond (successp)
          (t
           (set-branch-index-tree nil directory)
           (if error-on-failure
               (error 'patch-failure :pathname directory :output output)
               (values nil output))))))

(define-executable darcs)
(define-executable darcs-fast-export)
(define-executable hg)
(define-executable hg-fast-export)
(define-executable python)            ; this is for hg-to-git.py
(define-executable rsync)
(define-executable cvs)
(define-executable wget)

(defvar *supported-import-types* '(hg darcs cvs svn tarball))

(defun locate-vcs-import-executables (vcs-type)
  (every #'find-executable (ecase vcs-type
                             (hg      '(hg python hg-fast-export))
                             (darcs   '(darcs darcs-fast-export wget))
                             (cvs     '(rsync cvs)) ; git-cvsimport
                             (svn     '(rsync))     ; git-svn
                             (tarball '()))))

(defun locate-import-executables ()
  (mapc #'locate-vcs-import-executables *supported-import-types*))

(defun touch-www-file (url)
  (with-valid-exit-codes ((1 nil) (2 nil) (3 nil) (4 nil) (5 nil) (6 nil) (7 nil) (8 nil)) (wget "--spider" url)))

;; Untested!
;; PIPE!
;; TODO: clear up branch handling: -o/-M
(defun indirect-import-mercurial (url to-repo target-ref transit-repo &aux (*repository* to-repo))
  (unless (fad:directory-exists-p transit-repo)
    (hg "clone" url transit-repo))
  (hg "pull" "-R" transit-repo)
  (let ((*executable-standard-output* nil)
        (*executable-error-output* nil))
    (pipe (hg-fast-export "-r" transit-repo ;; "-M" (cook-ref-value target-ref)
                          )
          (git *repository* "fast-import"))))

;; PIPE!
;; TODO: speed up incremental import: --{import,export}-marks
(defun indirect-import-darcs (url to-repo target-ref transit-repo &aux (*repository* to-repo))
  (unless (fad:directory-exists-p transit-repo)
    (darcs "get" url transit-repo))
  (darcs "pull" "--all" "--repodir" transit-repo url)
  (when (nonbare-repository-present-p to-repo)
    (multiple-value-bind (staged-mod staged-del staged-new unstaged-mod unstaged-del untracked) (repository-status)
      (when untracked
        (format t "~@<;;; ~@;before conversion ~S -> ~S: untracked files ~A in the target repository.  Purging.~:@>~%"
                transit-repo *repository* untracked)
        (mapc #'delete-file untracked))
      (when (or staged-mod staged-del staged-new unstaged-mod unstaged-del)
        (ensure-clean-repository :error))))
  ;; We ignore exit status, as, sadly, it's not informative.
  ;; Thankfully, git-fast-import is pretty reliable.
  (let ((*executable-standard-output* nil)
        (*executable-error-output* nil))
    (pipe (darcs-fast-export transit-repo ;; (strconcat* "--git-branch=" (cook-ref-value target-ref))
                             )
          (git *repository* "fast-import"))))

(defun direct-import-cvs (url to-repo target-ref cvs-module-name &aux (*repository* to-repo))
  ;; -o ref -r remotes/ref/master
  (git *repository* "cvsimport" "-d" url cvs-module-name))

(defun direct-import-svn (url to-repo target-ref svn-module new-p &aux (*repository* to-repo))
  (when new-p
    (with-executable-options (:explanation `("setting up svn to git conversion: ~S => ~S" ,url ,to-repo))
      (git to-repo "svn" "init" url svn-module)))
  (git to-repo "svn" "fetch"))

(defun direct-import-tarball (url-template to-repo target-ref temp-dir &optional tarball-version &aux
                              (*repository* to-repo))
  (flet ((repository-last-version-from-tag (&optional (directory *repository*))
           (multiple-value-bind (successp output)
               (with-explanation ("determining version last recorded in ~S" directory)
                 (git directory "log" "-1" "--tags" "--decorate=short"))
             (declare (ignore successp))
             (when-let ((version (first (extract-delimited-substrings output "tag: upstream/" #\,))))
               (mapcar #'parse-integer (split-sequence #\. version))))))
    (iter (with last-version = (or tarball-version (repository-last-version-from-tag)))
          (for (values url next-version) = (determine-available-module-tarball-version-starting-after url-template last-version))
          (setf last-version next-version)
          (while url)
          (let* ((slash-pos (or (position #\/ url :from-end t)
                                (git-error "~@<Error while calculating tarball URL from template ~A: ~S has no slashes.~:@>"
                                           url-template url)))
                 (localised-tarball (merge-pathnames (subseq url (1+ slash-pos)) temp-dir)))
            ;; inlined WITH-FILE-FROM-WWW from desire
            (with-file-from-www (localised-tarball url)
              (with-executable-options (:explanation `("in repository ~A: importing tarball version ~A"
                                                       ,to-repo ,(princ-version-to-string next-version)))
                (git *repository* "import-orig" localised-tarball)))))))

(defun indirect-import-cvs (url to-repo target-ref transit-repo cvs-module-name lockdir &aux (*repository* to-repo))
  (rsync "-ravPz" url transit-repo)
  (with-output-to-file (stream (subfile* transit-repo "CVSROOT" "config") :if-exists :supersede)
    (format stream "LockDir=~A~%" lockdir))
  (unless (fad:directory-exists-p (subdirectory* transit-repo cvs-module-name))
    ;; caller didn't guess the CVS module right, try to fix up..
    (iter (for guess in '("src"))
          (when (fad:directory-exists-p (subdirectory* transit-repo guess))
            (format t "~@<;;; ~@;During import from ~S: CVS module ~S does not exist, guessed an alternative: ~S.  Recording that as the new wrinkle.~:@>~%"
                    transit-repo cvs-module-name guess)
            (setf cvs-module-name guess)
            ;; remnants of stateful import:
            ;; (set-remote-module-wrinkle *source-remote* name guess)
            (leave))
          (finally
           (git-error "~@<During import from ~S: CVS module ~S does not exist, and it's name couldn't be guessed.~:@>" transit-repo cvs-module-name))))
  (with-exit-code-to-error-translation ((9 'git-error :format-control "~@<Repository ~S not clean during fetch.~:@>"
                                           :format-arguments (list to-repo)))
    (git to-repo "cvsimport" "-v" "-C" to-repo ;; "-o" (cook-ref-value target-ref)
         "-d" (format nil ":local:~A" (string-right-trim "/" (namestring transit-repo))) cvs-module-name)))

(defun indirect-import-svn (url to-repo target-ref transit-repo svn-module new-p &aux (*repository* to-repo))
  (rsync "-ravPz" url transit-repo)
  (when new-p
    (with-executable-options (:explanation `("setting up svn to git conversion: ~S => ~S" ,transit-repo ,to-repo))
      (git to-repo "svn" "init" `("file://" ,transit-repo ,svn-module)))) ;; 'file://' -- gratuitious SVN complication
  (git to-repo "svn" "fetch"))

;;; Utility
(defun determine-available-module-tarball-version-starting-after (url-template version &optional (search-depth 3))
  ;; XXX: security implications: URL-TEMPLATE comes from DEFINITIONS
  (iter (for depth below search-depth)
        (with current-depth-version = version)
        (for current-depth-variants = (next-version-variants current-depth-version))
        (iter (for next-version-variant in current-depth-variants)
              (for url = (string-right-trim '(#\/) (format nil url-template (princ-version-to-string next-version-variant))))
              (when (with-explanation ("touching URL ~S" url)
                      (touch-www-file url))
                (return-from determine-available-module-tarball-version-starting-after (values url next-version-variant))))
        (setf current-depth-version (first current-depth-variants))))

;;;
;;; Touching
;;;
(defun touch-repository (url)
  (with-valid-exit-codes ((128 nil)) (git "." "peek-remote" url)))

(defun touch-rsync-repository (url)
  (with-valid-exit-codes ((23 nil)) (rsync "--list-only" url)))

(defun touch-cvs-repository (url)
  (with-valid-exit-codes ((1 nil)) (cvs "-d" url "history")))