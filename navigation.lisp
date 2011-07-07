;Gruppe X keine Ahnung
; Franz
; Jonathan
; Tarek
; Markus

; Starten des Experiments mit(s)
; Status: Malt sichtbare Teile des Labyrinths.
; In den Konstanten kann die Startposition angepasst werden.


;The Maze
(setf *maze* '(
("x" "x" "x" "x" "x" "x" "x" "x" "x" "x" "x" "x" "x" "x" "x" "x" "x" "x" "x" "x")
("x" "*" "o" "o" "o" "o" "*" "o" "*" "x" "*" "o" "*" "o" "o" "x" "o" "o" "*" "x")
("x" "o" "x" "x" "x" "x" "o" "x" "*" "o" "*" "x" "o" "x" "x" "x" "x" "x" "o" "x")
("x" "o" "x" "*" "o" "o" "*" "x" "x" "x" "o" "x" "*" "*" "x" "*" "o" "o" "*" "x")
("x" "o" "x" "o" "x" "x" "x" "x" "*" "o" "*" "x" "x" "o" "x" "o" "x" "x" "x" "x")
("x" "o" "x" "o" "x" "*" "o" "x" "o" "x" "x" "x" "*" "*" "x" "*" "o" "o" "*" "x")
("x" "*" "o" "*" "o" "*" "x" "x" "o" "x" "*" "o" "*" "x" "x" "x" "x" "x" "o" "x")
("x" "x" "x" "x" "x" "o" "x" "*" "o" "x" "o" "x" "*" "o" "o" "E" "o" "o" "*" "x")
("x" "*" "o" "o" "o" "*" "x" "o" "x" "x" "x" "x" "o" "x" "x" "x" "x" "x" "o" "x")
("x" "o" "x" "x" "x" "o" "x" "*" "D" "x" "o" "o" "*" "x" "o" "x" "o" "x" "o" "x")
("x" "o" "x" "o" "o" "*" "x" "x" "x" "x" "x" "x" "o" "x" "o" "*" "*" "o" "*" "x")
("x" "o" "x" "x" "x" "*" "o" "o" "o" "*" "o" "o" "*" "x" "x" "o" "x" "x" "x" "x")
("x" "o" "x" "o" "x" "o" "x" "x" "x" "o" "x" "x" "o" "x" "o" "*" "o" "*" "o" "x")
("x" "*" "A" "*" "x" "*" "o" "*" "x" "*" "o" "o" "*" "x" "x" "x" "x" "o" "x" "x")
("x" "o" "x" "o" "x" "x" "x" "o" "x" "x" "x" "x" "x" "x" "x" "*" "o" "*" "o" "x")
("x" "x" "x" "o" "x" "*" "o" "*" "x" "*" "o" "o" "o" "*" "x" "o" "x" "x" "x" "x")
("x" "*" "o" "*" "x" "o" "x" "x" "x" "o" "x" "x" "x" "o" "x" "*" "o" "o" "*" "x")
("x" "o" "x" "o" "x" "o" "x" "o" "x" "o" "x" "x" "x" "o" "x" "x" "x" "x" "o" "x")
("x" "*" "o" "*" "o" "*" "o" "*" "B" "*" "x" "o" "o" "*" "o" "o" "o" "C" "*" "x")
("x" "x" "x" "x" "x" "x" "x" "x" "x" "x" "x" "x" "x" "x" "x" "x" "x" "x" "x" "x")
))

;Constants
(setf *wall* "x")
(setf *crossing* "*")
(setf *buddy* "@")
(setf *start-pos* '(6 3))
;Window and maze properties
(setf *size* (length *maze*))
(setf *windowSize* 600)
(setf *space* (/ *windowSize* *size*))
(setf *offset* (/ *space* 2))

;Werden im Laufe des Programms geändert
(setf *position* *start-pos*)


(defun s ()

		; Oeffnet ein Fenster für das Experiment. Auf das Fenster kann über lokale Variable window zugegriffen werden.
		(let ((window (open-exp-window "Navigation"									  
						:visible t						;Das Fenster soll angezeigt werden.
						:width *windowSize*						;Breite des Fensters.
						:height *windowSize*						;Höhe des Fensters.
						:x 0
						:y 0)))
				;(draw-text)
				;(draw-lab)
								(print *start-pos*)
								(setq *position* *start-pos*)
								(draw-visible *position*)
				;Fuehrt das ACT-R Modell aus, wenn die globale Variable *actr-enabled-p* auf t gesetzt wurde
				(when *actr-enabled-p*						 
						(reset)												
						(install-device window)								;Zeige ACT-R was das Experimenten Fenster ist.
						(proc-display)												;Verarbeite alle visuellen Objekte im Experiment so, dass ACT-R sie versteht und darauf zugreifen kann.
						(run 2 :real-time t)))								;Starte das ACT-R-Modell und lasse es maximal 30 Sekunden lang laufen
				
		
)

; Draw the element at position x y
(defun draw-elem (x y)
		(add-text-to-exp-window
				:text (nth x (nth y *maze*))
				:width 10
				:x (+ *offset* (* *space* x))
				:y (+ *offset* (* *space* y))
		)
)

;Draw complete maze (for debugging)
(defun draw-lab ()
		;Draw the *maze*
		
		(dotimes (i *size* nil) (dotimes (j *size* nil)(draw-elem i j)))
)

;Draw the visible part of the maze.
(defun draw-visible (pos)
		(clear-exp-window)
		; Male Rechts
		(dotimes (j *size* nil)
				(let* (( i (+ j (first pos) 1)))
					(if (not (equal (nth i (nth (second pos) *maze*)) *wall*))
						(draw-elem i (second pos))
						(progn	(setf *FreiNachRechts* j)
								(return)
						)
					)
				)
		)
		; Male Links
		(dotimes (j *size* nil)
				(let* (( i (+ (* -1 j) (first pos) -1)))
					(if (not (equal (nth i (nth (second pos) *maze*)) *wall*))
						(draw-elem i (second pos))
						(progn	(setf *FreiNachLinks* j)
								(return)
						)
					)
				)
		)
		; Male Oben
		(dotimes (j *size* nil)
			(let* (( i (+ (* -1 j) (second pos) -1)))
				(if (not (equal (nth (first pos) (nth i *maze*)) *wall*))
					(draw-elem (first pos) i)
					(progn	(setf *FreiNachOben* j)
							(return)
					)
				)
			)
		)
		; Male Unten
		(dotimes (j *size* nil)
			(let* (( i (+ j (second pos))))
				(if (not (equal (nth (first pos) (nth i *maze*)) *wall*))
					(draw-elem (first pos) i)
					(progn	(setf *FreiNachUnten* j)
							(return)
					)
				)
			)
		)
		
		;Draw Buddy			  
		(add-button-to-exp-window 
				:x (+ *offset* (* *space* (first pos)))								;X-Koordinate an der der Button angezeigt werden soll.
				:y (+ *offset* (* *space* (second pos)))							;Y-Koordinate an der der Text angezeigt werden soll.
				:height 20										 					;Höhe desd Button.
				:width 20															;Breite des Button.
				:text (nth (first pos) (nth (second pos) *maze*))					;Text der auf dem Button angezeigt werden soll.
				:action nil															;Wenn dieser Button gedrückt wird, rufe die Funktion "button-pressed" auf.
		)
)

; Funktion um einen key-press zu verarbeiten. Diese Funktion wird automatisch aufgerufen sobald eine Taste gedrueckt wird. Innerhalb der Funktion kann die gewünschte Reaktion auf den Tastendruck definiert werden.
(defmethod rpm-window-key-event-handler ((win rpm-window) key)
	(let ((oldPos *position*))
		(case key
			((#\w)					 
				(progn
					(setq *position* (list (first *position*) (+ (second *position*) -1)))
					))
			((#\a)					 
				(progn
					(setq *position* (list (+ (first *position*) -1) (second *position*)))
					))
			((#\s)					   
				(progn
					(setq *position* (list (first *position*) (+ (second *position*) 1)))
					))
			((#\d)							
				(progn
					(setq *position* (list (+ (first *position*) 1) (second *position*)))
					))
				)
		(if (equal (nth (first *position*) (nth (second *position*) *maze*)) *wall*)
			(setq *position* oldPos)
			(draw-visible *position*)
		)
	)
)  
		
(clear-all)

(define-model Navigation

		; Die Einstellungen wurden aus dem Beispiel übernommen!
		; Zusätzliche Einstellungen werden in der Folgenden Liste aufgelistet und Argumentiert.
		; Optionen:
		; 	* :auto-attend - Das visuelle modul soll die Aufmerksamkeit nach einer
		;					 Positionsanfrage automatisch zu dieser bewegen.
		(sgp :v t :esc t :rt -2 :lf 0.4 :ans 0.5 :bll 0.5 :act nil :ncnar nil :trace-detail low :show-focus t :auto-attend t) 
		
		(chunk-type Abschnitt Wegpunkt1 Wegpunkt2 Entfernung)
		(chunk-type Wegpunkt Name)
		(chunk-type Status Hauptstatus Unterstatus Blickrichtung Oben Unten Rechts Links)
		
		; Das Gedächtniss
		(add-dm
				(goal isa Status Hauptstatus Bewegung Unterstatus BeginneOrientierungsPhase)			; Setze den Status auf Orientierung
				(gray isa chunk)														; Wird im Button verwendet
		)
		
		; Beginne eine Orientierungsphase damit, die entsprechenden Felder zu leeren, und die Aufmerksamkeit auf
		; das Ich zu setzen.
		(P BeginneOrientierungsPhase
			=goal>
				isa				Status
				HauptStatus		Bewegung
				Unterstatus		BeginneOrientierungsPhase
				Blickrichtung	=blickrichtung
			?visual>
				state			free
			==>
			=goal>																	; Der Goalchunk wird zurück gesetzt
				HauptStatus		Bewegung
				Unterstatus		Orientierung
				Blickrichtung	Hoch
				Oben			nil
				Unten			nil
				Rechts			nil
				Links			nil
			+visual-location>														; Die Position des Ich's(Button) finden
				isa				visual-location
				kind			Button
				:auto-attend	t
		)
		
		; Der Blick nach "Oben" um zu checken, wie weit das Ich in dieser Richtung gehen kann!
		(P OrientierungHoch
			=goal>
				isa				Status
				Hauptstatus		Bewegung
				Unterstatus		Orientierung
				Blickrichtung	Hoch
				Oben			nil
			?visual>
				state			free
			=visual>
				isa
			=visual-location>
				isa				visual-location
				kind			Button
				screen-x		=x
				screen-y		=y
			==>
			=goal>
			+visual-location>
				isa				visual-location
				kind			text
				< screen-x		=x
				screen-y		=y
		)
		; Analysiere Das Feld oberhalb des Ich's.
		(P OrientierungHoch2
			=goal>
				isa				Status
				Hauptstatus		Bewegung
				Unterstatus		Orientierung
				Blickrichtung	Hoch
				Oben			nil
			?visual>
				state			free
			!bind!				=wall				(*wall*)				; Hole die globale Variable "wall".
			=visual> ;TODO visual not correct here!
				isa				visual
				kind			Text
				- value			=wall
			=visual-location>
				isa				visual-location
				screen-x		=x
				screen-y		=y
			!bind!				=entfernung				(*FreiNachOben*)	; Hole die Länge des Koridors nach oben.
			==>
			=goal>
				Blickrichtung	Links
				Oben			=entfernung
			+visual-location>
				isa				visual-location
				kind			Text
				:nearest-loc	current
				< screen-x		=x
			;visual wird automatisch geleert?
		)
				
			
			
		
		(P SearchCurrent
				=goal>
						isa				Status
						Status			SearchCurrent
				?visual>
						state free
		==>
				+visual-location>
						isa				visual-location
						kind			Button
				=goal>
						Status			LookAtCurrent
		)
		
		(P Kreuzung
				=goal>
						isa				Status
						name			=curRichtung
						Status			LookAtCurrent
				!bind!	=crossing		(*crossing*) ; Hole die globale Variable.
				=visual>
						ISA				oval
						value			"*"
				+visual-location>
						isa				visual-location
						screen-x		=x
				==>
				+goal>
						Status			KreuzungLookUp
				+visual-location>
						isa				visual-location
						kind			text
						< screen-x		=x
		)
		
		(P KreuzungLookUp
				=goal>
						isa				Status
						Status			Kreuz
						
				!bind! 	=wall			(*wall*) ; Hole die globale Variable.
				!bind!
				=visual>
						isa				text
						- value			=wall
				==>
						
						
						
						
						
						
		(goal-focus goal)
	(setf *actr-enabled-p* t)
)
