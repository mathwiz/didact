(defvar *node-list*)


(defstruct node
  (name nil)
  (question nil)
  (yes-case nil)
  (no-case nil))


(defun init ()
  (setf *node-list* nil))


;; Add and return name of added
(defun add-node (name question yes-case no-case)
  (let ((node (make-node :name name
                         :question question
                         :yes-case yes-case
                         :no-case no-case)))
    (progn
      (setf *node-list* (cons node *node-list*))
      (node-name node))))


;; find by name or return nil
(defun find-node (name)
  (labels ((rec (nodes)
                (cond ((null nodes) nil)
                      (t (if (equal (node-name (car nodes)) name)
                             (car nodes)
                             (rec (cdr nodes)))))))
    (rec *node-list*)))


;; take user input
(defun process-node (name)
  nil)


;;
(defun run ()
  nil)


;; add a node through prompts
(defun user-add ()
  nil)


;; testing
(init)
(add-node 'start
          "Does the engine turn over?"
          'engine=turns-over
          'engine-wont-turn-over)

(add-node 'engine-turns-over
          "Will the engine run for any period of time?"
          'engine-will-run-briefly
          'engine-wont-run)

(add-node 'engine-wont-run
          "Is there gas in the tank?"
          'gas-in-tank
          "Fill the tank and try starting the engine again.")

(add-node 'engine-wont-turn-over
          "Do you hear any sound when you turn the key?"
          'sound-when-turn-key
          'no-sound-when-turn-key)

(add-node 'no-sound-when-turn-key
          "Is the battery voltage low?"
          "Replace the battery"
          'battery-voltage-ok)

(add-node 'battery-voltage-ok
          "Are the battery cables dirty or loose?"
          "Clean the cables and tighten the connections."
          'battery-cables-good)

(add-node 'battery-cables-good
          "?"
          "."
          "You are screwed.")

