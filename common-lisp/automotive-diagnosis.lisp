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
  (let ((node (find-node name)))
    (cond ((null node) (progn (format t "Node ~S not found" name) nil))
          (t (let* ((q (node-question node))
                    (a (progn (format t "~S " q) (read))))
               (cond ((equal a 'y) (node-yes-case node))
                     (t (node-no-case node))))))))


;; main program loop
(defun run ()
  (labels ((rec (current-node)
                (let ((response (process-node current-node)))
                  (cond ((null response) nil)
                        ((stringp response) (progn (format t "~S" response) nil))
                        (t (rec response))))))
    (rec 'start)))
  
  

;; add a node through prompts
(defun user-add ()
  (let ((name (progn (format t "Enter the name: ") (read)))
        (quest (progn (format t "Enter the question (enclose in double quotes): ") (read)))
        (yes (progn (format t "Enter the yes case (double quote if string): ") (read)))
        (no (progn (format t "Enter the no case (double quote if string): ") (read))))
    (add-node name quest yes no)))


;; testing
(init)
(add-node 'start
          "Does the engine turn over?"
          'engine-turns-over
          'engine-wont-turn-over)

(add-node 'engine-turns-over
          "Will the engine run for any period of time?"
          'engine-will-run-briefly
          'engine-wont-run)

(add-node 'engine-will-run-briefly
          "Does it stall when cold but not when warm?"
          'idle-rpm-speed
          "Take your car to your dealer.")

(add-node 'idle-rpm-speed
          "Is the idle speed at least 700 rpm?"
          'out-of-ideas
          "Adjust idle speed")

(add-node 'engine-wont-run
          "Is there gas in the tank?"
          'gas-in-tank
          "Fill the tank and try starting the engine again.")

(add-node 'engine-wont-turn-over
          "Do you hear any sound when you turn the key?"
          'sound-when-turn-key
          'no-sound-when-turn-key)

(add-node 'sound-when-turn-key
          "Have you ever serviced the alternator?"
          "Take your car to the dealer."
          "Test your alternator.")

(add-node 'no-sound-when-turn-key
          "Is the battery voltage low?"
          "Replace the battery"
          'battery-voltage-ok)

(add-node 'battery-voltage-ok
          "Are the battery cables dirty or loose?"
          "Clean the cables and tighten the connections."
          'battery-cables-good)

(add-node 'battery-cables-good
          "Do you have an idea?"
          "Check your best idea."
          'out-of-ideas)

(add-node 'out-of-ideas
          "Are you out of ideas?"
          "Take your car to your dealer."
          "Think some more.")
