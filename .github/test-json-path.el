;;; test-json-path.el --- Test for fate/json-get-path and fate/json-print-path-js functions  -*- lexical-binding: t; -*-

;; Test script for JSON path functions

(require 'ert)
(require 'fate-web)
(require 'json-ts-mode)
(require 'treesit)

(defun test-json-path--get-path-at-string (content search-str)
  "Get JSON path at position of SEARCH-STR in CONTENT using tree-sitter."
  (with-temp-buffer
    (insert content)
    (json-ts-mode)
    (treesit-parser-create 'json)
    (goto-char (point-min))
    (search-forward search-str)
    (backward-char)
    (fate/json-get-path (treesit-node-at (point)))))

;;; Tests for fate/json-get-path - Tree-sitter node path parsing

(ert-deftest test-json-get-path-simple-object ()
  "Test path parsing for a simple object property."
  (let* ((json "{\"name\": \"John\"}")
         (path (test-json-path--get-path-at-string json "John")))
    (should (equal path '("\"name\"")))))

(ert-deftest test-json-get-path-nested-object ()
  "Test path parsing for nested object properties."
  (let* ((json "{\"user\": {\"name\": \"John\"}}")
         (path (test-json-path--get-path-at-string json "John")))
    (should (equal path '("\"user\"" "\"name\"")))))

(ert-deftest test-json-get-path-array-element ()
  "Test path parsing for array element."
  (let* ((json "{\"items\": [1, 2, 3]}")
         (path (test-json-path--get-path-at-string json "2")))
    (should (equal path '("\"items\"" 1)))))

(ert-deftest test-json-get-path-first-array-element ()
  "Test path parsing for first array element."
  (let* ((json "{\"items\": [\"first\", \"second\"]}")
         (path (test-json-path--get-path-at-string json "first")))
    (should (equal path '("\"items\"" 0)))))

(ert-deftest test-json-get-path-nested-array ()
  "Test path parsing for nested array element."
  (let* ((json "{\"matrix\": [[1, 2], [3, 4]]}")
         (path (test-json-path--get-path-at-string json "4")))
    (should (equal path '("\"matrix\"" 1 1)))))

(ert-deftest test-json-get-path-object-in-array ()
  "Test path parsing for object property inside array."
  (let* ((json "{\"users\": [{\"name\": \"Alice\"}, {\"name\": \"Bob\"}]}")
         (path (test-json-path--get-path-at-string json "Bob")))
    (should (equal path '("\"users\"" 1 "\"name\"")))))

(ert-deftest test-json-get-path-deep-nesting ()
  "Test path parsing for deeply nested structure."
  (let* ((json "{\"a\": {\"b\": {\"c\": {\"d\": \"value\"}}}}")
         (path (test-json-path--get-path-at-string json "value")))
    (should (equal path '("\"a\"" "\"b\"" "\"c\"" "\"d\"")))))

(ert-deftest test-json-get-path-complex-array-object ()
  "Test path parsing for complex array and object mix."
  (let* ((json "{\"data\": [{\"items\": [1, 2]}, {\"items\": [3, 4]}]}")
         (path (test-json-path--get-path-at-string json "4")))
    (should (equal path '("\"data\"" 1 "\"items\"" 1)))))

(ert-deftest test-json-get-path-special-key-names ()
  "Test path parsing for keys with special characters."
  (let* ((json "{\"user-name\": \"test\", \"email@domain\": \"test@example.com\"}")
         (path (test-json-path--get-path-at-string json "test@example.com")))
    (should (equal path '("\"email@domain\"")))))

;;; Tests for fate/json-print-path-js - String serialization

(defun test-json-format-path (path-list)
  "Format PATH-LIST using the same logic as fate/json-print-path-js."
  (mapconcat
   (lambda (elt)
     (cond
      ((numberp elt) (format "[%d]" elt))
      ((stringp elt)
       (let ((trimmed (string-trim elt "\"" "\"")))
         (if (string-match-p "[^[:word:]]" trimmed)
             (format "[%s]" elt)
           (format ".%s" trimmed))))))
   path-list ""))

(ert-deftest test-json-print-path-js-simple-key ()
  "Test serialization of simple object key."
  (let ((result (test-json-format-path '("\"name\""))))
    (should (string= result ".name"))))

(ert-deftest test-json-print-path-js-nested-keys ()
  "Test serialization of nested object keys."
  (let ((result (test-json-format-path '("\"user\"" "\"name\""))))
    (should (string= result ".user.name"))))

(ert-deftest test-json-print-path-js-single-array-index ()
  "Test serialization of single array index."
  (let ((result (test-json-format-path '("\"items\"" 1))))
    (should (string= result ".items[1]"))))

(ert-deftest test-json-print-path-js-zero-index ()
  "Test serialization of zero array index."
  (let ((result (test-json-format-path '("\"items\"" 0))))
    (should (string= result ".items[0]"))))

(ert-deftest test-json-print-path-js-multiple-array-indices ()
  "Test serialization of multiple array indices."
  (let ((result (test-json-format-path '("\"matrix\"" 1 1))))
    (should (string= result ".matrix[1][1]"))))

(ert-deftest test-json-print-path-js-mixed-path ()
  "Test serialization of mixed object and array path."
  (let ((result (test-json-format-path '("\"users\"" 1 "\"name\""))))
    (should (string= result ".users[1].name"))))

(ert-deftest test-json-print-path-js-hyphen-key ()
  "Test serialization of key with hyphen."
  (let ((result (test-json-format-path '("\"user-name\""))))
    (should (string= result "[\"user-name\"]"))))

(ert-deftest test-json-print-path-js-at-sign-key ()
  "Test serialization of key with @ symbol."
  (let ((result (test-json-format-path '("\"email@domain\""))))
    (should (string= result "[\"email@domain\"]"))))

(ert-deftest test-json-print-path-js-space-key ()
  "Test serialization of key with space."
  (let ((result (test-json-format-path '("\"first name\""))))
    (should (string= result "[\"first name\"]"))))

(ert-deftest test-json-print-path-js-dot-key ()
  "Test serialization of key with dot."
  (let ((result (test-json-format-path '("\"file.ext\""))))
    (should (string= result "[\"file.ext\"]"))))

(ert-deftest test-json-print-path-js-complex-path ()
  "Test serialization of complex path with mixed elements."
  (let ((result (test-json-format-path '("\"data\"" "\"users\"" 0 "\"profile\"" "\"first-name\""))))
    (should (string= result ".data.users[0].profile[\"first-name\"]"))))

(ert-deftest test-json-print-path-js-all-arrays ()
  "Test serialization of path with only array indices."
  (let ((result (test-json-format-path '("\"deep\"" 0 1 2))))
    (should (string= result ".deep[0][1][2]"))))

(ert-deftest test-json-print-path-js-underscore-key ()
  "Test serialization of key with underscore (valid word char)."
  (let ((result (test-json-format-path '("\"user_name\""))))
    (should (string= result "[\"user_name\"]"))))

(ert-deftest test-json-print-path-js-number-key ()
  "Test serialization of numeric key name."
  (let ((result (test-json-format-path '("\"123\""))))
    (should (string= result ".123"))))

;; Run tests if executed as a script
(when noninteractive
  (ert-run-tests-batch-and-exit))

;;; test-json-path.el ends here
