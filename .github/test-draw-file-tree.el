;;; test-draw-file-tree.el --- Test for fate/draw-file-tree function  -*- lexical-binding: t; -*-

;; Test script for fate/draw-file-tree function

(require 'fate-misc)

(defun test-draw-file-tree ()
  "Test the fate/draw-file-tree function with a sample tree structure."
  (let* ((test-tree '("project"
                      ("src"
                       ("components" "Header.js" "Footer.js")
                       ("utils" "helpers.js"))
                      "README.md"
                      "package.json"))
         (expected-output "project\n├── src\n│   ├── components\n│   │   ├── Header.js\n│   │   └── Footer.js\n│   └── utils\n│       └── helpers.js\n├── README.md\n└── package.json\n")
         (actual-output (fate/draw-file-tree test-tree)))

    (unless (string= actual-output expected-output)
      (error "Test failed!\nExpected:\n%s\nActual:\n%s" expected-output actual-output))

    (message "Test passed! Output:\n%s" actual-output)
    t))

(condition-case err
    (progn
      (test-draw-file-tree)
      (message "All tests passed!")
      (kill-emacs 0))
  (error
   (message "Test failed with error: %s" (error-message-string err))
   (kill-emacs 1)))

;;; test-draw-file-tree.el ends here
