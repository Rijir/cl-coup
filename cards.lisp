(in-package :coup)

(defvar *assassin* 'assassin)
(defvar *contessa* 'contessa)
(defvar *ambassador* 'ambassador)
(defvar *sargent* 'sargent)
(defvar *duke* 'duke)

(defvar *all-cards* (list *assassin*
                          *contessa*
                          *ambassador*
                          *sargent*
                          *duke*))

(defun cardp (c)
  (if (find c *all-cards*)
      t
      nil))

(defun card< (c1 c2)
  (string< (symbol-name c1)
           (symbol-name c2)))

(defun make-deck (&optional (num-copies 3))
  (loop repeat num-copies
        with c = (loop for c in *all-cards* collect c)
        append c))

(defun make-deck-shuffled (&optional (num-copies 3))
  (alexandria:shuffle (make-deck num-copies)))
