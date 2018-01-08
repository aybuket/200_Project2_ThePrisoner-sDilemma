;;; aakgul13@ku.edu.tr    Mon Oct 26 13:35:16 2015
;;;    	  	   	 
;;; Comp200 Project 2    	  	   	 
;;;    	  	   	 
;;; Due Nov 1, 2015    	  	   	 
;;;    	  	   	 
;;; Before you start:    	  	   	 
;;;    	  	   	 
;;; * Please read the detailed instructions for this project from the
;;; file project2.pdf available in the Assignments section of the
;;; course website.    	  	   	 
;;;    	  	   	 
;;; * Please read "Project Submission Instructions" carefully and make
;;; sure you understand everything before you start working on your
;;; project in order to avoid problems.
;;;    	  	   	 
;;; While you are working:    	  	   	 
;;; * Type all your work and notes in the appropriate sections of this file.
;;; * Please do not delete any of the existing lines.
;;; * Use the procedure names given in the instructions.
;;; * Remember to frequently save your file.
;;; * Use semicolon (;) to comment out text, tests and unused code.
;;; * Remember to document your code.
;;; * Remember our collaboration policy: you can discuss with your friends but
;;;    	  	   	 
;;;   *** NOTHING WRITTEN GETS EXCHANGED ***
;;;    	  	   	 
;;; When you are done:    	  	   	 
;;; * Perform a final save and check-in.
;;; * Please do not make any modifications after midnight on the due date.
;;; * Please send an email comp200help@ku.edu.tr if you have any questions.
;;; * Make sure your file loads and runs without errors.
;;;    	  	   	 
;;;   *** IF (load "project2.scm") GIVES ERRORS OR WE CANNOT RUN YOUR PROJECT, IT WILL NOT BE GRADED ***
;;;    	  	   	 
    	  	   	 
;;; DO NOT CHANGE FOLLOWING LINES, THEY ARE NECESSARY FOR THE WHOLE PROJECT.
;;; WHILE SOLVING PROBLEMS YOU CAN USE THE PROCEDURES DEFINED HERE, IT WILL EASE YOUR WORK.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang racket    	  	   	 
(require math/bigfloat)    	  	   	 
; Ignore the following. This is necessary so the file loads without errors initially:
(define your-answer-here #f)    	  	   	 
    	  	   	 
; make-play will make a list then given two strings
; e.g. (make-play "c" "c") => ("c" "c")
(define make-play list)    	  	   	 
    	  	   	 
; The empty history when noone    	  	   	 
; has started to played yet    	  	   	 
(define the-empty-history '())    	  	   	 
    	  	   	 
; extend-history adds the new element
; to the history when someone plays
(define extend-history cons)    	  	   	 
    	  	   	 
; empty-history? is a procedure that
; returns boolean true if the history is empty
(define empty-history? null?)    	  	   	 
    	  	   	 
; History of players is kept in a list.
; The last action is found by most-recent-play procedure.
(define most-recent-play car)    	  	   	 
    	  	   	 
; All the actions except the most recent
; one are found with rest-of-plays procedure.
(define rest-of-plays cdr)    	  	   	 
    	  	   	 
; The play-loop procedure takes as its  arguments two prisoner's
; dilemma strategies, and plays an iterated game of approximately
; one hundred rounds.  A strategy is a procedure that takes
; two arguments: a history of the player's previous plays and
; a history of the other player's previous plays.  The procedure
; returns either a "c" for cooperate or a "d" for defect.
(define (play-loop strat0 strat1)
  (define (play-loop-iter strat0 strat1 count history0 history1 limit)
    (cond ((= count limit) (print-out-results history0 history1 limit))
	    (else (let ((result0 (strat0 history0 history1))
			      (result1 (strat1 history1 history0)))
		      (play-loop-iter strat0 strat1 (+ count 1)
				        (extend-history result0 history0)
					  (extend-history result1 history1)
					    limit)))))
  (play-loop-iter strat0 strat1 0 the-empty-history the-empty-history
		    (+ 90 (random 21))))
    	  	   	 
; The following procedures are used to compute and print
; out the players' scores at the end of an iterated game
(define (print-out-results history0 history1 number-of-games)
  (let ((scores (get-scores history0 history1)))
    (newline)    	  	   	 
    (display "Player 1 Score:  ")
    (display (* 1.0 (/ (car scores) number-of-games)))
    (newline)    	  	   	 
    (display "Player 2 Score:  ")
    (display (* 1.0 (/ (cadr scores) number-of-games)))
    (newline)))    	  	   	 
    	  	   	 
(define (get-scores history0 history1)
  (define (get-scores-helper history0 history1 score0 score1)
    (cond ((empty-history? history0)
	      (list score0 score1))
	    (else (let ((game (make-play (most-recent-play history0)
					        (most-recent-play history1))))
		      (get-scores-helper (rest-of-plays history0)
					      (rest-of-plays history1)
					           (+ (get-player-points 0 game) score0)
						        (+ (get-player-points 1 game) score1))))))
  (get-scores-helper history0 history1 0 0))
    	  	   	 
  (define (get-player-points num game)
  (list-ref (get-point-list game) num))
    	  	   	 
(define *game-association-list*
  ;; format is that first sublist identifies the players' choices
  ;; with "c" for cooperate and "d" for defect; and that second sublist
  ;; specifies payout for each player
  '((("c" "c") (3 3))    	  	   	 
    (("c" "d") (0 5))    	  	   	 
    (("d" "c") (5 0))    	  	   	 
    (("d" "d") (1 1))))    	  	   	 
    	  	   	 
; Note that you will need to write extract-entry in Problem 1
(define (get-point-list game)    	  	   	 
  (cadr (extract-entry game *game-association-list*)))
    	  	   	 
; A sampler of strategies    	  	   	 
    	  	   	 
(define (NASTY my-history other-history)
  "d")    	  	   	 
    	  	   	 
(define (PATSY my-history other-history)
  "c")    	  	   	 
    	  	   	 
(define (SPASTIC my-history other-history)
  (if (= (random 2) 0)    	  	   	 
      "c"    	  	   	 
      "d"))    	  	   	 
    	  	   	 
(define (EGALITARIAN  my-history other-history)
  (define (count-instances-of test hist)
    (cond ((empty-history? hist) 0)
	  ((string=? (most-recent-play hist) test)
	   (+ (count-instances-of test (rest-of-plays hist)) 1))
	  (else (count-instances-of test (rest-of-plays hist)))))
  (let ((ds (count-instances-of "d" other-history))
	(cs (count-instances-of "c" other-history)))
    (if (> ds cs) "d" "c")))    	  	   	 
    	  	   	 
(define (EYE-FOR-EYE my-history other-history)
  (if (empty-history? my-history)
      "c"    	  	   	 
      (most-recent-play other-history)))
;;; DO NOT CHANGE THE ABOVE LINES, THEY ARE NECESSARY FOR THE WHOLE PROJECT.
;;; WHILE SOLVING PROBLEMS YOU CAN USE THE PROCEDURES DEFINED HERE, IT WILL EASE YOUR WORK.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    	  	   	 
;;;;;;;::;;;;:::;;;;:::;:;::::;;::::::;:;:::;::;;;::;;;:;::;;::;::;;:;;::;;::
;;; Problem 1    	  	   	 
    	  	   	 
; Description for extract-entry: (before the definition of each procedure,
; please write a description about what the procedure does and what
; its input and output should be, making sure the lines are commented
; out with semi-colons)    	  	   	 
    	  	   	 
; HINT: You can make use of list-ref
; (list-ref *game-association-list* 0) ==> (("c" "c") (3 3))
; (list-ref *game-association-list* 1) ==> (("c" "d") (0 5))

; ASNWER: extract-entry takes a play which shows the preferences (cooperate or defect) and a list
; which shows the possibities with scores as inputs. enxtract-entry-helper looks the first
; element of the list and compare with the play. If there is a match, it returns the play
; with its scores. If there is not a match, it returns the empty list.

(define (extract-entry play *game-association-list*)
(extract-entry-helper 0 play *game-association-list*))
(define extract-entry-helper
    (lambda (count play *game-association-list*)
      (if (< count (length *game-association-list*))
          (if (equal? play (car (list-ref *game-association-list* count)))
              (list-ref *game-association-list* count)
              (extract-entry-helper (+ count 1) play *game-association-list*))
          '()
          )))   	  	   	 
    	  	   	 
; Test cases for extract-entry: (When you write your procedure you can
; test it with the below test cases. Initially it returns #f since
; your-answer-here is defined as #f. But when you complete the procedure
; It should return the correct answers below.)
(display "--------Problem 1--------")
(newline)    	  	   	 
(define a-play (make-play "c" "c"))
(extract-entry a-play *game-association-list*) ; ANSWER => '(("c" "c") (3 3))
    	  	   	 
(define a2-play (make-play "c" "d"))
(extract-entry a2-play *game-association-list*) ; ANSWER => '(("c" "d") (0 5))
    	  	   	 
(define a3-play (make-play "d" "c"))
(extract-entry a3-play *game-association-list*) ; ANSWER => '(("d" "c") (5 0))
    	  	   	 
(define a4-play (make-play "d" "d"))
(extract-entry a4-play *game-association-list*) ; ANSWER => '(("d" "d") (1 1))
    	  	   	 
(define a5-play (make-play "x" "x"))
(extract-entry a5-play *game-association-list*) ; ANSWER => '()
(display "-----End of Problem 1-----")
(newline)    	  	   	 
    	  	   	 
;;;;;;;::;;;;:::;;;;:::;:;::::;;::::::;:;:::;::;;;::;;;:;::;;::;::;;:;;::;:;;
;;; Problem 2    	  	   	 
    	  	   	 
; Use play-loop to play games among the five defined strategies
; Create a matrix in which you show the average score for
; tournaments pitting all possible pairings of the five
; different strategies: Nasty, Patsy, Eye-for-Eye, Spastic, Egalitarian.
; Describe the behavior you observe for the different strategies.
    	  	   	 
; To test the strategies against each other => e.g. (play-loop NASTY PATSY)
; Fill in the ? part in below matrices when you get the result from play-loop procedure.
    	  	   	 
; Strategy2 =>      NASTY       ||       PATSY       ||      SPASTIC      ||     EGALITARIAN   ||    EYE-FOR-EYE    ||
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Strategy1 | Player1 | Player2 || Player1 | Player2 || Player1 | Player2 || Player1 | Player2 || Player1 | Player2 ||
;           | points  | points  || points  | points  || points  | points  || points  | points  || points  | points  ||
;  NASTY    |   1.0   |   1.0   ||  5.0    |   0     ||  2.85   |  0.54   ||  1.04   |  0.99   ||  1.04   |  0.99   ||
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; ANSWER:
; NASTY against NASTY: Both sides will always defect and win same points in every round. If both sides defect, sides gaim
; least points in the game. None of both sides never lose but also they will never have high points.

; NASTY against PATSY: p1 will always defect and p2 will always cooperate. In the game, if both sides don't do the same,
; the side which defects gains all points and other side gains none. Then, NASTY always win against PATSY with max high score.

; NASTY against: SPASTIC: p1 will always defect and p2 will defect or cooperate randomly. When p2 defects,
; both sides will gain 1 point, but when p2 cooperate, p1 will gain 5 points and p2 will gain nothing. Hence, NASTY will
; always win against SPASTIC with high score.

; NASTY against EGALITARIAN: p1 will always defect and p2 will cooperate at first round, then looks the other's history
; and if other defects more than cooperate, p2 will defect too. That means p2 will cooperate at first round,
; and at the other rounds, p2 will defect. First round, p1 will gain 5 pts and p2 will gain none but all the other
; rounds both sides will gain 1 pt. NASTY will always win against EGALITARIAN with very small pts difference.

; NASTY against EYE-FOR-EYE: p1 will always defect and p2 will cooperate at first round, then mimics the other's last move.
; In other words, p2 will cooperate at first round, and defect at other rounds. That means, EGALITARIAN and EYE-FOR-EYE
; have same moves against NASTY and both of them will lose against NASTY with a little pts difference.

; SHORTLY: NASTY will always win against other strategies except of itself. Playing NASTY seems a logical movement for winning.
    	  	   	 
; Strategy2 =>      NASTY       ||       PATSY       ||      SPASTIC      ||     EGALITARIAN   ||    EYE-FOR-EYE    ||
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Strategy1 | Player1 | Player2 || Player1 | Player2 || Player1 | Player2 || Player1 | Player2 || Player1 | Player2 ||
;           | points  | points  || points  | points  || points  | points  || points  | points  || points  | points  ||
;  PATSY    |   0     |   5.0   ||  3.0    |   3.0   ||   1.33  |  4.12   ||   3.0   |   3.0   ||   3.0   |   3.0   ||
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; ANSWER:
; PATSY against PATSY / EGALITARIAN / EYE-FOR-EYE: Because p1 will always cooperate, EGALITARIAN and EYE-FOR-EYE will always
; cooperate too. Then both of the strategies acts like PATSY. This case is the case which both sides have the most and balaced
; pts in the game.

; PATSY against SPASTIC: p1 will always cooperate and p2 will defect or cooperate randomly. When p2 cooperates, both sides will
; gain 3 pts, but when p2 defects, p1 will gain none and p2 will gain 5 pts. Then, PATSY will always lose against SPASTIC with
; lowest score after against NASTY's score.

; SHORTLY: PATSY has lowest or balanced score. If other side is obseessed with winning, it does not seem a good strategy because
; it cannot help to gain pts; but if other side thinks that pts must be fair, then it may be a good strategy to be balanced. 
    	  	   	 
; Strategy2 =>      NASTY       ||       PATSY       ||      SPASTIC      ||     EGALITARIAN   ||    EYE-FOR-EYE    ||
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Strategy1 | Player1 | Player2 || Player1 | Player2 || Player1 | Player2 || Player1 | Player2 || Player1 | Player2 ||
;           | points  | points  || points  | points  || points  | points  || points  | points  || points  | points  ||
; SPASTIC   |   0.51  |  2.96   ||   3.9   |  1.65   ||   2.42  |  2.09   ||   2.7   |  2.11   ||   2.08  |  2.08   ||
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; ANSWER:
; SPASTIC against SPASTIC: Both sides will defect or cooperate randomly. Which side will win is uncertain, but both sides
; will have pts between 2 and 2,5 in average.
; We can assume both sides will cooperate one time and defect on time in 2 games. Possibilities:
;                   1) c,c -> 3,3 and d,d -> 1,1. Then both sides will have 2 pts in average.
;                   2) c,d -> 0,5 and d,c -> 5,0. Then both sides will have 2,5 pts in average.
; Assuming there are 4 games, possibilities:
;                   1) c,c -> 3,3 | c,d -> 0,5 | d,c -> 5,0 | d,d -> 1,1 Then both sides will have 2,25 in average.
;                   2) c,d -> 0,5 | c,d -> 0,5 | d,c -> 5,0 | d,c -> 5,0 Then both sides will have 2,5 in average.
;                   3) c,c -> 3,3 | c,c -> 3,3 | d,d -> 1,1 | d,d -> 1,1 Then both sides will have 2 in average.
; That means, they may have same pts or pts with small difference.

; SPASTIC against EGALITARIAN: p1 will defect or cooperate randomly, p2 will cooperate at first round and then looks p1's history
; and cooperate if number of p1's defects are not bigger than cooperates'. It does not have a common pattern. At the first try,
; it gives the results as above, and it looks like scores are closely; but at other tries, there are results such that
; "p1's score is 3.98 and p2's score is 1.53" or "p1's score is 0.58 and p2's score 2.81." It is really big difference.

; SPASTIC against EYE-FOR-EYE: p1 will defect or cooperate randomly, p2 will cooperate at first and then mimics p1's last move.
; At first play-loop, the result was balanced. Then I tried 30 more play-loop and I had results such that 12 SPASTIC wins, and 18 balanced.
; EYE-FOR-EYE is shifted version of other strategy. Then it either neutralizes the other one or gains less than other but realy close pts.

; SHORTLY: SPASTIC depends on chance and strategy of the other one. It may wins with high point or it may lose with low point or it may be
; very close to other's score. 
    	  	   	 
; Strategy2 =>      NASTY       ||       PATSY       ||      SPASTIC      ||     EGALITARIAN   ||    EYE-FOR-EYE    ||
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Strategy1 | Player1 | Player2 || Player1 | Player2 || Player1 | Player2 || Player1 | Player2 || Player1 | Player2 ||
;           | points  | points  || points  | points  || points  | points  || points  | points  || points  | points  ||
;EGALITARIAN|  0.99   |  1.04   ||   3.0   |   3.0   ||   2.14  |   2.14  ||   3.0   |   3.0   ||   3.0   |   3.0   ||
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; ANSWER:
; EGALITARIAN against EGALITARIAN: Both look other's history and cooperate if other has not defects more than cooperates. Because
; both will cooperate at first round, they will always with cooperate and act like PATSY. 

; EGALITARIAN against EYE-FOR-EYE: Both look other's history. One of them cooperates if other has not defects more than cooperates,
; other cooperates if other's last move is cooperating. Then both act like PATSY.

; SHORTLY: It is logical to use this strategy against others because in three cases, results will be balanced; and one case will lose with
; very little difference. There is only one case whose results are uncertain.
    	  	   	 
; Strategy2 =>      NASTY       ||       PATSY       ||      SPASTIC      ||     EGALITARIAN   ||    EYE-FOR-EYE    ||
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Strategy1 | Player1 | Player2 || Player1 | Player2 || Player1 | Player2 || Player1 | Player2 || Player1 | Player2 ||
;           | points  | points  || points  | points  || points  | points  || points  | points  || points  | points  ||
;EYE-FOR-EYE|  0.99   |   1.04  ||   3.0   |   3.0   ||   2.29  |   2.34  ||   3.0   |   3.0   ||   3.0   |   3.0   ||
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; ANSWER:
; EYE-FOR-EYE against EYE-FOR-EYE: Both cooperate at first round, then mimic each other's last move. This means both act like PATSY in that case.

; SHORTLY: It is also logical to use against other strategies because it has 3 balanced result, 1 losing result but with very little difference and
; 1 balanced or losing result with very little difference.

; GENERAL SUMMARY: NASTY is for winning, either highest score or lowest score. PATSY is for winning with average score or losing with lowest score.
; EYE-FOR-EYE and EGALITARIAN are more balanced, they win with average score and lose with close pts to winner's pts. SPASTIC's results are uncertain.
; It can win with high pts or lose with low pts. I think using EYE-FOR-EYE is most logical move, because there even it won't win every game, it won't
; lose badly either. 
    	  	   	 
;;;;;;;::;;;;:::;;;;:::;:;::::;;::::::;:;:::;::;;;::;;;:;::;;::;::;;:;;::;:;:
;;; Problem 3    	  	   	 
    	  	   	 
; Games involving Egalitarian tend to be slower than other games.
; Why is that so? Use order-of-growth notation to explain your answer.
; Alyssa P. Hacker, upon seeing the code for Egalitarian,
; suggested the following iterative version of the procedure:
;(define (Egalitarian my-history other-history)
;  (define (majority-loop cs ds hist)
;    (cond ((empty-history? hist) (if (> ds cs) “d” “c”))
;          ((string=? (most-recent-play hist) “c”)
;           (majority-loop (+ 1 cs) ds (rest-of-plays hist)))
;          (else    	  	   	 
;           (majority-loop cs (+ 1 ds) (rest-of-plays hist)))))
;  (majority-loop 0 0 other-history))
; Compare this procedure with the original version.
; Do the orders of growth (in time) for the two procedures differ?
; Is the newer version faster?

; ANSWER: First one's the order of growth is theta(2n), second one's the order of growth is theta(n).
; In lectures, we say that 2n or n there is no difference, they both linear and we prefer not to keep constant
; inside. Then, both of them becomes theta(n).
; First one has a recursive procedure inside and the procedure runs twice because there are two case for
; "test" input. Second one has a recursive procedure inside too, but the procedure runs once because it controls
; in very step if it is "c" or not. 
;    	  	   	 
    	  	   	 
;;;;;;;::;;;;:::;;;;:::;:;::::;;::::::;:;:::;::;;;::;;;:;::;;::;::;;:;;::;::;
;;; Problem 4    	  	   	 
    	  	   	 
; Write a new strategy eye-for-two-eyes

; EYE-FOR-TWO-EYES takes two inputs, my-history and other-history. It returns "d" if both of the last two
; elements are "d". Procedure looks the other-history. First it controls if the history is empty or has
; one element, then it returns "c". Then it looks if last two elements are equal. If they are equal, procedure
; returns one of the last two elements. In other words, if both are "c", it returns "c" and if both are "d",
; it returns "d". If they are not equal, it returns "c" because that means one of them is "c" and one of them
; is "d".

; EYE-FOR-TWO-EYES against other strategies: The results of the play-loops is not different much than EYE-FOR-EYE
; against other strategies. EYE-FOR-TWO-EYES avts is same as EYE-FOR-EYE against PATSY, EGALITARIAN and EYE-FOR-EYE.
; There is very little difference between EYE-FOR-EYE against NASTY and EYE-FOR-TWO-EYES against NASTY. EYE-FOR-TWO-EYES's
; score decreases about 0.002 and NASTY's score increases about 0.05. EYE-FOR-TWO-EYES fails against SPASTIC. It is not
; bad as PATSY against SPASTIC, but it is worse than EYE-FOR-EYE against SPASTIC. EYE-FOR-EYE against SPASTIC has equal or
; really close scores; but EYE-FOR-TWO-EYES loses against SPASTIC, and has not close score. Differences between score of SPASTIC
; and score of EYE-FOR-TWO-EYES is around 1 and 2. 

(define (EYE-FOR-TWO-EYES my-history other-history)
(cond ((empty-history? other-history) "c")
      ((empty-history? (rest-of-plays other-history)) "c")
      (else (let ((mr (most-recent-play other-history))
         (sr (most-recent-play (rest-of-plays other-history))))
      (if (equal? mr sr) mr "c"))))	  	   	 
  )    	  	   	 
    	  	   	 
; Test cases for EYE-FOR-TWO-EYES: (When you write your procedure you can
; test it with the below test cases. Initially it returns #f since
; your-answer-here is defined as #f. But when you complete the procedure
; It should return the correct answers below.)
(display "--------Problem 4--------")
(newline)    	  	   	 
    	  	   	 
; Strategy depends only to the other-history, so dont case my-history
(EYE-FOR-TWO-EYES the-empty-history the-empty-history) ; ANSWER => "c"
(EYE-FOR-TWO-EYES the-empty-history (list "c")) ; ANSWER => "c"
(EYE-FOR-TWO-EYES the-empty-history (list "d")) ; ANSWER => "c"
(EYE-FOR-TWO-EYES the-empty-history (list "c" "c")) ; ANSWER => "c"
(EYE-FOR-TWO-EYES the-empty-history (list "c" "d")) ; ANSWER => "c"
(EYE-FOR-TWO-EYES the-empty-history (list "d" "c")) ; ANSWER => "c"
(EYE-FOR-TWO-EYES the-empty-history (list "d" "d")) ; ANSWER => "d"
(EYE-FOR-TWO-EYES the-empty-history (list "c" "d" "c")) ; ANSWER => "c"
(EYE-FOR-TWO-EYES the-empty-history (list "d" "d" "c")) ; ANSWER => "d"
(EYE-FOR-TWO-EYES the-empty-history (list "d" "c" "d" "d")) ; ANSWER => "c"

; 

(display "-----End of Problem 4-----")
(newline)    	  	   	 
    	  	   	 
;;;;;;;::;;;;:::;;;;:::;:;::::;;::::::;:;:::;::;;;::;;;:;::;;::;::;;:;;::;:::
;;; Problem 5    	  	   	 
    	  	   	 
; Write a procedure make-eye-for-n-eyes.

; ANSWER:
; make-eye-for-n-eye takes a number and returns "d" if all of the last n elements are "d".

; ANSWER:
; There is no difference between EYE-FOR-EYE and make-eye-for-n-eye when they are against PATSY, EGALITARIAN or
; another eye-for-?-eye(s) strategies. The score difference decreases very little with increaing n, when it is
; against NASTY. When it is against SPASTIC score is always change because SPASTIC has no common pattern; but
; score differences increases with increaing n, becacuse eye-for-n-eye(s) is getting closer to PATSY with larger n.
; In other words, make-eye-for-n-eyes strategy is getting closer to PATSY strategy with larger n. 
    	  	   	 
(define (make-eye-for-n-eyes n)
  ; You need to return a two-argument (my-history other-history) procedure.
  (lambda (my-history other-history)
       	(if (> n 0)
      (cond ((empty-history? other-history) "c")
            ((equal? (most-recent-play other-history) "c") "c")
            (else ((make-eye-for-n-eyes (- n 1)) my-history (rest-of-plays other-history))))
      "d")  	   	 
    )    	  	   	 
  )    	  	   	 
    	  	   	 
; Test cases for make-eye-for-n-eyes: (When you write your procedure you can
; test it with the below test cases. Initially it returns #f since
; your-answer-here is defined as #f. But when you complete the procedure
; It should return the correct answers below.)
(display "--------Problem 5--------")
(newline)    	  	   	 
    	  	   	 
(define one-eye (make-eye-for-n-eyes 1)) ; equivalent to EYE-FOR-EYE
(define two-eye (make-eye-for-n-eyes 2)) ; equivalent to EYE-FOR-TWO-EYE
    	  	   	 
; Similar to one-eye and two-eye.
; Defects only if three recent plays in the history1 is "d"
; Cooperates if any of the three recent plays in hsitory1 is "c"
(define three-eye (make-eye-for-n-eyes 3))
    	  	   	 
; Strategy depends only to the other-history, so dont case my-history
(display "Testing (make-eye-for-n-eyes 1)")
(newline)    	  	   	 
(one-eye the-empty-history (list "d")) ; ANSWER => "d"
(one-eye the-empty-history (list "d" "c" "c")) ; ANSWER => "d"
    	  	   	 
(display "Testing (make-eye-for-n-eyes 2)")
(newline)    	  	   	 
(two-eye the-empty-history (list "d" "c")) ; ANSWER => "c"
(two-eye the-empty-history (list "d" "d" "c")) ; ANSWER => "d"
    	  	   	 
(display "Testing (make-eye-for-n-eyes 3)")
(newline)    	  	   	 
(three-eye the-empty-history (list "d" "d" "d")) ; ANSWER => "d"
(three-eye the-empty-history (list "d" "d" "c")) ; ANSWER => "c"
    	  	   	 
(display "-----End of Problem 5-----")
(newline)    	  	   	 
    	  	   	 
;;;;;;;::;;;;:::;;;;:::;:;::::;;::::::;:;:::;::;;;::;;;:;::;;::;::;;:;;:::;;;
;;; Problem 6    	  	   	 
    	  	   	 
; Write a procedure make-rotating-strategy

; ANSWER: 
; make-rotating-strategy takes four inputs. Two of them is strategies that we want to return due to frequencies, other
; two inputs. First we need to counter to count how many times we use this strategy. So, we must define a counter.
; I prefer -1, because procedure uses the modulo to control the frequencies after setting the counter. Now, we need to
; define a new procedure because we want to return a precedure due to our counter. Our strategies takes two inputs, then
; inner procedure must take two inputs. Scheme procedures return the last operation, then count must be setted before the
; comparison with frequencies. I defined a new constant c as (modulo count (+ freq0 freq1)). If freq0 is 2 and freq1 is 3,
; then after 5 using, the procedure must start to return strat0 again. 0-1-2-3-4 are the values that c can be equal.
; Then, procedure must return strat0 when c is less than freq0, otherwise it must return strat1; and bigger procedure
; (make-rotating-strategy) must return helper procedure.

; ANSWER:
; rotating-1 against other strategies: It wins against PATSY and eye-for-n-eyes with high score, because eye-for-n-eyes
; act like PATSY and say "c" in every round. It is also similar with SPASTIC against PATSY. It may be balaced with EGALITARIAN,
; because they may neutralise each other, or rotating-1 wins due to how many games played. In this case EYE-FOR-EYE and
; EGALITARIAN act same. There is no common pattern with SPASTIC as before, their scores were between 2 and 2,5 in my tries.
; That means, one of them may win with little difference or they may be balaced. If we try rotating-1 against rotating-1,
; first one always say "d", and second on always say "c" because we use twice in one game and, it rotate again twice.
; That means, if we use rotating-1 against itself, first side acts like NASTY and second side acts like PATSY.

; ANSWER:
; rotating-2 against other strategies: It wins against PATSY and loses against NASTY. When it is against SPASTIC, their
; scores were between 2 and 3 in my tries. rotating-2 wins generally against EGALITARIAN; but sometimes scores reverse and
; rotating-2 loses. Generally, rotating-2 playes "d"-"d"-"c"-"c" and EGALITARIAN "c"-"d"-"d"-"d"-"c"-"d"-"d"-"d"
; EX:
; (play-loop rotating-2 EGALITARIAN)
; Player 1 Score:  3.978723404255319
; Player 2 Score:  1.5319148936170213
; (play-loop rotating-2 EGALITARIAN)
; Player 1 Score:  1.5157894736842106
; Player 2 Score:  2.7263157894736842
; (play-loop rotating-2 EGALITARIAN)
; Player 1 Score:  3.2803738317757007
; Player 2 Score:  1.97196261682243
; (play-loop rotating-2 EGALITARIAN)
; Player 1 Score:  4.0
; Player 2 Score:  1.5
; (play-loop rotating-2 EGALITARIAN)
; Player 1 Score:  3.9894736842105263
; Player 2 Score:  1.5157894736842106
; (play-loop rotating-2 EGALITARIAN)
; Player 1 Score:  3.230769230769231
; Player 2 Score:  2.021978021978022
; (play-loop rotating-2 EGALITARIAN)
; Player 1 Score:  1.528301886792453
; Player 2 Score:  2.707547169811321
; rotating-2 may win or be balaced with EYE-FOR-EYE, it depends have many game played. rotating-2 may win with little difference
; or may be balaced with rotating-1. It will balaced when it played against itself.

(define (make-rotating-strategy strat0 strat1 freq0 freq1)
  (let ((count -1))
    (define (mrs-helper m0 m1)
            (set! count (+ count 1))
            (define c (modulo count (+ freq0 freq1)))
            (if (< c freq0) (strat0 m0 m1) (strat1 m0 m1)))
    mrs-helper)
 )    	  	   	 
    	  	   	 
; Test cases for make-rotating-strategy: (When you write your procedure you can
; test it with the below test cases. Initially it returns #f since
; your-answer-here is defined as #f. But when you complete the procedure
; It should return the correct answers below.)
(display "--------Problem 6--------")
(newline)    	  	   	 
    	  	   	 
(define rotating-1 (make-rotating-strategy NASTY PATSY 1 1))
; 1 times NASTY 1 times PATSY    	  	   	 
(define (testing rt)    	  	   	 
  (if (eq? rt #f) empty    	  	   	 
      (rt the-empty-history the-empty-history)
     )    	  	   	 
  )    	  	   	 
    	  	   	 
(display "Testing (make-rotating-strategy NASTY PATSY 1 1)")
(newline)    	  	   	 
(testing rotating-1) ; ANSWER => "d"
(testing rotating-1) ; ANSWER => "c"
(testing rotating-1) ; ANSWER => "d"
(testing rotating-1) ; ANSWER => "c"
    	  	   	 
(define rotating-2 (make-rotating-strategy NASTY PATSY 2 2))
; 2 times NASTY 2 times PATSY    	  	   	 
(display "Testing (make-rotating-strategy NASTY PATSY 2 2)")
(newline)    	  	   	 
(testing rotating-2) ; ANSWER => "d"
(testing rotating-2) ; ANSWER => "d"
(testing rotating-2) ; ANSWER => "c"
(testing rotating-2) ; ANSWER => "c"
(testing rotating-2) ; ANSWER => "d"
(testing rotating-2) ; ANSWER => "d"
(testing rotating-2) ; ANSWER => "c"
(testing rotating-2) ; ANSWER => "c"
    	  	   	 
(display "-----End of Problem 6-----")
(newline)    	  	   	 
    	  	   	 
;;;;;;;::;;;;:::;;;;:::;:;::::;;::::::;:;:::;::;;;::;;;:;::;;::;::;;:;;:::;;:
;;; Problem 7    	  	   	 
; Write a new strategy, make-higher-order-spastic, which takes a list of strategies as input.

; ANSWER:
; make-higher-order-spastic takes a list input, and count have many times it played. Due to number of plays,
; it returns next strategy in the list. Procedure uses a helper which will be returned from make-higher-order-spastic.
; mhos-helper takes 2 inputs because strategies take 2 histories. mhos-helper sets the counter, lookes its modulo and
; finds the nth (mod counter list-length) element. 

; NASTY-PASTY strategy is the same strategy with rotating-1. 


(define (make-higher-order-spastic strategies)
  (let ((count-spastic -1) (l1 (length strategies)))
    (define (mhos-helper m0 m1)
      (set! count-spastic (+ count-spastic 1))
      (define cs (modulo count-spastic l1))
      ((list-ref strategies cs) m0 m1))
    mhos-helper)

  )    	  	   	 
    	  	   	 
    	  	   	 
; Test cases for make-higher-order-spastic: (When you write your procedure you can
; test it with the below test cases. Initially it returns #f since
; your-answer-here is defined as #f. But when you complete the procedure
; It should return the correct answers below.)
(display "--------Problem 7--------")
(newline)    	  	   	 
    	  	   	 
(define NASTY-PATSY (make-higher-order-spastic (list NASTY PATSY)))
    	  	   	 
(display "Testing NASTY-PASTY")
(newline)    	  	   	 
(testing NASTY-PATSY) ; ANSWER =>  "d"
(testing NASTY-PATSY) ; ANSWER => "c"
(testing NASTY-PATSY) ; ANSWER => "d"
(testing NASTY-PATSY) ; ANSWER => "c"
    	  	   	 
(display "-----End of Problem 7-----")
(newline)    	  	   	 
    	  	   	 
;;;;;;;::;;;;:::;;;;:::;:;::::;;::::::;:;:::;::;;;::;;;:;::;;::;::;;::;::;;;;
;;; Problem 8    	  	   	 
; Write a procedure gentle, which takes as input
; a strategy (say strat) and a number
; between 0 and 1 (call it gentleness-factor).
    	  	   	 
; HINT : You can use (bigfloat->flonum (bfrandom))
; to generate random numbers between 0 and 1

; ANSWER:
; Procedure takes strategy and number, then it looks that if strategy returns "c",
; if it does not return "c", then it picks a random number between 0 and 1. If the random number is bigger
; than gentless-factor, then it returns "d", but if it is not, it returns "c".

(define (gentle strat gentleness-factor)
  (lambda (my-history other-history)
    (if (equal? (strat my-history other-history) "c") "c"
        (if (> (bigfloat->flonum (bfrandom)) gentleness-factor) "d" "c"))
    )    	  	   	 
  )    	  	   	 
    	  	   	 
; Test cases for gentle: (When you write your procedure you can
; test it with the below test cases. Initially it returns #f since
; your-answer-here is defined as #f. But when you complete the procedure
; It should return the correct answers below.)
(display "--------Problem 8--------")
(newline)    	  	   	 
    	  	   	 
(display "Testing (gentle NASTY 0.0)")
(newline)    	  	   	 
(testing (gentle NASTY 0.0)) ; ANSWER =>  "d"
(testing (gentle NASTY 0.0)) ; ANSWER =>  "d"
    	  	   	 
(display "Testing (gentle NASTY 1.0)")
(newline)    	  	   	 
(testing (gentle NASTY 1.0)) ; ANSWER =>  "c"
(testing (gentle NASTY 1.0)) ; ANSWER => "c"
    	  	   	 
(define SLIGHTLY-GENTLE-NASTY (gentle NASTY 0.1))
(define SLIGHTLY-GENTLE-EYE-FOR-EYE (gentle EYE-FOR-EYE 0.1))
    	  	   	 
(display "-----End of Problem 8-----")
(newline)    	  	   	 
    	  	   	 
    	  	   	 
;;; DO NOT CHANGE FOLLOWING LINES, THEY ARE NECESSARY FOR 3 PLAYER GAME.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define *game-association-list3*
  (list (list (list "c" "c" "c") (list 4 4 4))
        (list (list "c" "c" "d") (list 2 2 5))
        (list (list "c" "d" "c") (list 2 5 2))
        (list (list "d" "c" "c") (list 5 2 2))
        (list (list "c" "d" "d") (list 0 3 3))
        (list (list "d" "c" "d") (list 3 0 3))
        (list (list "d" "d" "c") (list 3 3 0))
        (list (list "d" "d" "d") (list 1 1 1))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    	  	   	 
    	  	   	 
;;;;;;;::;;;;:::;;;;:::;:;::::;;::::::;:;:::;::;;;::;;;:;::;;::;::;;::;::;;;:
;;; Problem 9    	  	   	 
;Revise the Scheme code for the two-player game to make a three-player iterated game.

; ANSWER: 
; play-loop3 takes three strategy and takes results from them due to number of games which is genereted by
; random variable between 0 and 20 plus 90. 
(define (play-loop3 strat0 strat1 strat2)
 (define (play-loop3-iter count history0 history1 history2 limit)
    (cond ((= count limit) (print-out-results3 history0 history1 history2 limit))
          (else (let ((result0 (strat0 history0 history1 history2))
                      (result1 (strat1 history0 history1 history2))
                      (result2 (strat2 history0 history1 history2)))
                  (play-loop3-iter (+ count 1) (extend-history result0 history0)
                                  (extend-history result1 history1)
                                  (extend-history result2 history2)
                                  limit)))))
        	  	   	 
  (play-loop3-iter 0 the-empty-history the-empty-history the-empty-history (+ 90 (random 21))))
    	  	   	 
; Define "print-out-results" for handling three strategies:
; print-out-results takes 3 histories and the number of games, and calculates the average of points
; and returs the result with explanation. 
(define (print-out-results3 history0 history1 history2 number-of-games)
  (let ((scores (get-scores3 history0 history1 history2)))
    (newline)
    (display "Player 1 Score:  ")
    (display (* 1.0 (/ (car scores) number-of-games)))
    (newline)
    (display "Player 2 Score:  ")
    (display (* 1.0 (/ (cadr scores) number-of-games)))
    (newline)
    (display "Player 3 Score:  ")
    (display (* 1.0 (/ (car (cddr scores)) number-of-games)))
    (newline)
    ))
      	  	   	 
    	  	   	 
; Define "get-scores" for handling three strategies:
; get-scores takes three histories and calculates the scores due to points list and list them.
(define (get-scores3 history0 history1 history2)
  (define (get-scores3-helper history0 history1 history2 score0 score1 score2)
    (cond ((empty-history? history0)
           (list score0 score1 score2))
          (else (let ((game (make-play (most-recent-play history0)
                                       (most-recent-play history1)
                                       (most-recent-play history2))))
                  (get-scores3-helper (rest-of-plays history0)
                                      (rest-of-plays history1)
                                      (rest-of-plays history2)
                                      (+ (get-player-points3 0 game) score0)
                                      (+ (get-player-points3 1 game) score1)
                                      (+ (get-player-points3 2 game) score2))))))
  
(get-scores3-helper history0 history1 history2 0 0 0))

 (define (get-point-list3 game)    	  	   	 
  (cadr (extract-entry3 game *game-association-list3*)))

(define (get-player-points3 num game)
  (list-ref (get-point-list3 game) num))

; Define "extract-entry" for handling three strategies:
; extract-entry3 takes a play and points list and returns the value of the play. 
(define (extract-entry3 play *list*)
  (extract-entry3-helper 0 play *list*))
(define extract-entry3-helper
    (lambda (count play *list*)
      (if (< count (length *list*))
          (if (equal? play (car (list-ref *list* count)))
              (list-ref *list* count)
              (extract-entry3-helper (+ count 1) play *list*))
          '()
          ))    	  	   	 
  )    	  	   	 
    	  	   	 
; Test cases for extract-entry3: (When you write your procedure you can
; test it with the below test cases. Initially it returns #f since
; your-answer-here is defined as #f. But when you complete the procedure
; It should return the correct answers below.)
(display "--------Problem 9--------")
(newline)    	  	   	 
(display "Testing extract-entry-3:")
(newline)    	  	   	 
(extract-entry3 (make-play "c" "c" "c") *game-association-list3*)
; ANSWER =>  (("c" "c" "c") (4 4 4))
(extract-entry3 (make-play "c" "c" "d") *game-association-list3*)
; ANSWER => (("c" "c" "d") (2 2 5))
(extract-entry3 (make-play "c" "d" "c") *game-association-list3*)
; ANSWER =>  (("c" "d" "c") (2 5 2))
(extract-entry3 (make-play "c" "d" "d") *game-association-list3*)
; ANSWER =>  (("c" "d" "d") (0 3 3))
(extract-entry3 (make-play "d" "c" "c") *game-association-list3*)
; ANSWER =>  (("d" "c" "c") (5 2 2))
(extract-entry3 (make-play "d" "c" "d") *game-association-list3*)
; ANSWER => (("d" "c" "d") (3 0 3))
(extract-entry3 (make-play "d" "d" "c") *game-association-list3*)
; ANSWER =>  (("d" "d" "c") (3 3 0))
(extract-entry3 (make-play "d" "d" "d") *game-association-list3*)
; ANSWER => (("d" "d" "d") (1 1 1))
(extract-entry3 (make-play "x" "x" "x") *game-association-list3*)
; ANSWER =>  ()    	  	   	 
    	  	   	 
(display "-----End of Problem 9-----")
(newline)    	  	   	 
    	  	   	 
;;;;;;;::;;;;:::;;;;:::;:;::::;;::::::;:;:::;::;;;::;;;:;::;;::;::;;::;::;;:;
;;; Problem 10    	  	   	 
; Write strategies Patsy-3, Nasty-3, and spastic-3 that will work in a three-player game.
; Hint: They are same as NASTY, PATSY, and SPASTIC.

; ANSWER: PATSY-3 always returns "c" in three-player game.
(define (PATSY-3 my-history other-history-1 other-history-2)
  "c"    	  	   	 
  )    	  	   	 

; ANSWER: NASTY-3 always returns "d" in three-layer game.
(define (NASTY-3 my-history other-history-1 other-history-2)
  "d"    	  	   	 
  )    	  	   	 

; ASNWER: SPASTIC-3 returns "c" or "d" randomly.
(define (SPASTIC-3 my-history other-history-1 other-history-2)
  (if (= (random 2) 0)    	  	   	 
      "c"    	  	   	 
      "d"))      	  	   	 

; ANSWER: TOUGH-EYE-FOR-EYE returns "d" unless both of the other players say "c" in last game
; or if it is not a first round.
(define (TOUGH-EYE-FOR-EYE my-history other-history-1 other-history-2)
  (cond ((empty-history? my-history) "c")    	  	   	 
        ((equal? (most-recent-play other-history-1) "d") "d")
        ((equal? (most-recent-play other-history-2) "d") "d")
        (else "c"))
  )    	  	   	 

; ANSWER: SOFT-EYE-FOR-EYE returns "c" unless both of the other players say "d" in last game.
(define (SOFT-EYE-FOR-EYE my-history other-history-1 other-history-2)
  (cond ((empty-history? my-history) "c")    	  	   	 
        ((equal? (most-recent-play other-history-1) "c") "c")
        ((equal? (most-recent-play other-history-2) "c") "c")
        (else "d"))    	  	   	 
  )    	  	   	 
    	  	   	 
; Test cases for strategies: (When you write your procedure you can
; test it with the below test cases. Initially it returns #f since
; your-answer-here is defined as #f. But when you complete the procedure
; It should return the correct answers below.)
(display "--------Problem 10--------")
(newline)    	  	   	 
    	  	   	 
(display "Testing NASTY-3:")    	  	   	 
(newline)    	  	   	 
(NASTY-3 (list "c") (list "c") (list "c")) ; ANSWER => "d"
(NASTY-3 (list "c") (list "d") (list "c")) ; ANSWER => "d"
(NASTY-3 (list "d") (list "d") (list "d")) ; ANSWER => "d"

; ANSWER: NASTY-3 is not much different than NASTY. It will always win and gain high points unless both
; of the opponents are NASTY-3.
    	  	   	 
(display "Testing PATSY-3:")    	  	   	 
(newline)    	  	   	 
(PATSY-3 (list "c") (list "c") (list "c")) ; ANSWER => "c"
(PATSY-3 (list "d") (list "c") (list "c")) ; ANSWER => "c"
(PATSY-3 (list "d") (list "d") (list "d")) ; ANSWER => "c"

; ANSWER: PATSY-3 is not much different than PATSY. It will always loses and gain
; least points unless others say "c". If others say "c", it is the most useful strategy.
    	  	   	 
(display "Testing SPASTIC-3:")    	  	   	 
(newline)    	  	   	 
(SPASTIC-3 (list "c") (list "c") (list "c")) ; ANSWER => 50% of time "d", 50% of time "c"

; ANSWER: It is also not much different from 2 player ones. It loses against TOUGH-EYE-FOR-EYE every turn,
; and win against SOFT-EYE-FOR-EYE every turn because SOFT-EYE-FOR-EYE acts like PATSY generally. 

    	  	   	 
(display "Testing TOUGH-EYE-FOR-EYE:")
(newline)    	  	   	 
(TOUGH-EYE-FOR-EYE (list "d") (list "c") (list "c")) ; ANSWER => "c"
(TOUGH-EYE-FOR-EYE (list "c") (list "c") (list "d")) ; ANSWER => "d"
(TOUGH-EYE-FOR-EYE (list "c") (list "d") (list "c")) ; ANSWER => "d"
(TOUGH-EYE-FOR-EYE (list "c") (list "d") (list "d")) ; ANSWER => "d"

; ANSWER: TOUGH-EYE-FOR-EYE is the most useful strategy against others. It returns "d", if one of the
; opponents say "d", and other opponents say "c", it returns "c" and all of them gain highest point.
    	  	   	 
(display "Testing SOFT-EYE-FOR-EYE:")
(newline)    	  	   	 
(SOFT-EYE-FOR-EYE (list "c") (list "c") (list "d")) ; ANSWER => "c"
(SOFT-EYE-FOR-EYE (list "c") (list "d") (list "c")) ; ANSWER => "c"
(SOFT-EYE-FOR-EYE (list "c") (list "d") (list "d")) ; ANSWER => "d"
(SOFT-EYE-FOR-EYE (list "d") (list "c") (list "c")) ; ANSWER => "c"

; ANSWER: If one of the opponents is PATSY, SOFT-EYE-FOR-EYE acts like PATSY. Unless all of the strategies say "c",
; it become useful. 
    	  	   	 
(display "-----End of Problem 10-----")
(newline)    	  	   	 
    	  	   	 
    	  	   	 
;;;;;;;::;;;;:::;;;;:::;:;::::;;::::::;:;:::;::;;;::;;;:;::;;::;::;;::;::;;::
; END OF PROJECT    	  	   	 
    	  	   	 
    	  	   	 
