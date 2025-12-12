;;; test-draw-file-tree.el --- Test for fate/draw-file-tree function  -*- lexical-binding: t; -*-

;; Test script for fate/draw-file-tree function

(require 'ert)
(require 'fate-misc)

(ert-deftest test-draw-file-tree-basic ()
  "Test a basic project structure."
  (let* ((tree '("project"
                 ("src"
                  ("components" "Header.js" "Footer.js")
                  ("utils" "helpers.js"))
                 "README.md"
                 "package.json"))
         (expected "project
├── src
│   ├── components
│   │   ├── Header.js
│   │   └── Footer.js
│   └── utils
│       └── helpers.js
├── README.md
└── package.json
"))
    (should (string= (fate/draw-file-tree tree) expected))))

(ert-deftest test-draw-file-tree-flat ()
  "Test a flat directory structure."
  (let* ((tree '("docs" "manual.md" "setup.md" "api.md"))
         (expected "docs
├── manual.md
├── setup.md
└── api.md
"))
    (should (string= (fate/draw-file-tree tree) expected))))

(ert-deftest test-draw-file-tree-deep-nesting ()
  "Test deep nesting."
  (let* ((tree '("root"
                 ("level1"
                  ("level2"
                   ("level3" "file.txt")))))
         (expected "root
└── level1
    └── level2
        └── level3
            └── file.txt
"))
    (should (string= (fate/draw-file-tree tree) expected))))

(ert-deftest test-draw-file-tree-single-file ()
  "Test a directory with a single file."
  (let* ((tree '("config" "settings.json"))
         (expected "config
└── settings.json
"))
    (should (string= (fate/draw-file-tree tree) expected))))

(ert-deftest test-draw-file-tree-complex ()
  "Test a complex mixed structure."
  (let* ((tree '("app"
                 "Makefile"
                 ("src" "main.c" "utils.c")
                 ("include" "utils.h")
                 "LICENSE"))
         (expected "app
├── Makefile
├── src
│   ├── main.c
│   └── utils.c
├── include
│   └── utils.h
└── LICENSE
"))
    (should (string= (fate/draw-file-tree tree) expected))))

;; Run tests if executed as a script
(when noninteractive
  (ert-run-tests-batch-and-exit))

;;; test-draw-file-tree.el ends here
