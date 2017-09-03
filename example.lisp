
;;Automatic reloader engine

;;new window called 'win1' with a button. Prints hello window on terminal when clicked
(window "win1" :pos '(0 0) :size '(100 100)
	(button :click (lambda () (print "Hello window"))))

;; win loaded on first load. Content reloaded every time.
(defvar win (window "win2" :pos '(100 0) :size '(100 100)))
(set-window-content win (button :click (lambda () (print "Hello window 2"))))

(defvar audio (rt-audio :outputs 2 :inputs 0)) ;;Initialize audio
(clear audio) ;;remove all connections
(connect audio 0 (osc 440)) ;;Connect oscillator 440hz to out channel 1
(connect audio 1 (osc 450)) ;;-¦¦- 450 hz channel 2
 
(sleep 0.1)
(clear audio)
(defvar asdrosc)
(setf asdrosc (asdr 1.0 0.1 0.5 0.5))
(connect audio 0 (audio-mult (osc 440) asdrosc)) 
(connect audio 1 (audio-mult (osc 450) asdrosc))
(bang asdrosc) ;;Invoke asdr envelope alt: 
;;Bang asdr rutine on button click
(clear win)
(set-window-content win 
   (button :click (lambda () (bang asdrosc))))

(sleep 5.0)
;;Graphics
(defvar flat-shader (load-shader 'flat2d))
(defvar tex (load-texture-from-file "test1.png"))
(defvar verts (vertexes (list 0 0 1 0 1 1 0 1) :dim 2))

(clear win)
(set-window-content 
 win (canvas :draw (lambda () (draw vert flat-shader verts))))


;;;;Implementation examples
(defvar flat-shader-vert (vert-shader :file "flat.vert"))
(defvar flat-shader-frag (frag-shader :file "flat.frag")) 
(defvar arr (make-gl-array :d2 0 0 0 1 1 1 1 0)) 
(defvar tex (make-gl-texture :file "test.png"))
(set-uniform flat-shader-vert "color" 1.0 0.0 0.0 1.0)
(render flat-shader-vert flat-shader-frag arr tex)

;;Inside render
(check-context)
(load-array arr) ;checks if arr is loaded  (by checksum of values)
(load-texture tex) ;checks if tex is loaded. compares loaded with new version
(load-shader flat-shader)
(update-uniforms flat-shader)
;;

;window context classes
texture (/ array, path-string)
array-texture
file-texture
array (/list, array)
shader
string-shader
file-shader
gl-context-resource-manager
events
window-class 
glfwWindow instance
window redraw management

option: each resource class has a get and set method, so it is possible to signal when change has happened each set method should calculate which parts of the resource is changed so that only changes are loaded to gpu.

when working with path-based resources, monitor file.
prioritize ease to use over efficiency.

dont upload right away. Upload when needed or when told to
resource management:
1. array (done)
2. shader (done)
3. uniforms (done) 
   uniform vector 1,2,3,4d   (set-uniform 1.0 2.0 3.0 4.0)  
   uniform vectors 1,2,3,4d  (set-uniform #(1.0 2.0 3.0) #(1.0 2.0 3.0))
   uniform matrixes 2,3,4d   (set-uniform #(#(1 2 3) #(4 5 6) #(7 8 9)) #(#(1 2 3) #(4 5 6) #(7 8 9)))
4. resource manager (for array and shader)
   use one resource manager for each context
   two different concepts exists: context owned objects and context state.
   Two contexts can share objects but not state
   Hence shared vbos,textures and programs.
   State needs to be controlled for each context.
   Uniforms are shared too. (makes sense since program is)
   -> programs can control if uniforms are loaded

final result should look like this:

(set-uniform program "color" 0.0 0.0 0.2 1.0)
(set-uniform program "position" 0.0 0.0 0.2 1.0)
(gpu-array array '(0.0 0.0 1.0 0.0 1.0 1.0 0.0 1.0))
(render array program)

;;;alt (render array shader (uniform "color" 1.0 0.0 0.0 1.0) (uniform "pos" 1.0 2.0))
array should have revision incremented if data, kind or dimension is changed
texture: data, height, width or format
framebuffer: width,height,format
program revision should follow vertex and fragment object revisions (+)
 
It occurs that there are different kinds of behaviour between the graphics objects

1. Only one shader program can be loaded at a time
2. Several arrays can be loaded at a time, but only one for each program attribute. Hence some enumeration is needed.
      The standard way of doing this is by index.
3. Several Textures can be loaded. Only one per Sampler2D same applies for uniforms

Maybe the simplest is then to do 
(set-uniform program "color" 0.0 0.0 0.2 1.0) ;same thing with uniforms
(use-program program win) ;refer to context
(bind-array vbo1 0 win)
(bind-array vbo1 0 win2) ;auto-switch context
(draw-array 0 4 :lines win)
(swap-buffers win)

use-program 
    make context current
    finds the program inside the context. delete If not existant or out of date try compiling If out of date and compiles delete old
    bind program


;;Library precedence 
context-manager - window
glfw          -/
events       -/

next:
  load textures
  load shaders from files
  live code reloading
  monitor changes in files to upload new resources
  sounds

****  USER INTERFACE ****
  events - need a nice way of making event handlers. Maybe it should be treated like the graphics..
the problem isnt the window events, they can be handled by functions.
example:
(defun mouse-move(x y)
  (print (format nil "mouse moved ~a ~a" x y)))
(defvar bang (make-event))
(defvar b1 (make-button ))
(defvar b2 (make-button ))
(make-param *bg-color* 0xaaaaaaff)

(make-ui-graph ui-graph 
  (make-ui :content 
	   (make-grid :columns 2 :rows 2
		      (in-row 0 (make-label :text "hello"))
		      (in-row 1 (in-column 0 (make-button b1 :click bang)))
		      (in-row 1 (in-column 1 (make-button b2 :click bang))))))

(draw-ui-graph ui-graph window)

I dont want to redraw the ui-graph each time something changes (each load). So the make-ui-graph macro specifies a ui graph called ui-graph. each time this macro expansion is called, it will use graph theory to detect changes in the graph. So its not simply a defvar. In case the current ui-graph is nil, it wil just make the whole thing. Else, it will make a copy and compare the nodes of the graph. The graph itself is then marked as changed.

Chaining graphs before a "button" element was used below is the definition
(make-ui-graph-template button
   (click text width height) ;locals for button. May be dynamically changed during runtime.
   (make-grid :columns 1 :rows 1
	      (in-row 0
		      (rectangle :name rect1 
				 :width width :height height :left-mouse-up click)
		      (gradient :start *bg-color* ;grad start color. Dynamically updated during runtime.  
				:end 0x00000000 :orientation '(0 -1) :enabled (prop rect1 hover))
		      (gradient :start *bg-color* :end 0x00000000 :orientation '(0 1))
		      (text-box :text text))))

** properties
(make-prop bg-color 0xffffffff)   ;;Define property bg-color as 0xffffffff
(set-prop bg-color 0xaaffffff)    ;;Set property and invoke property changed
(get-prop-evt bg-color)           ;;Returns the event property

draw iterates the tree to see if any node has changed. Or the inverse: A node signals change and causes redrawing of it. 

One can bind to a symbol, a property or an event. Properties has the value-changed-evt event.

(defclass property ()
  ((value :initarg :value)
   (value-changed-evt :initform (make-instance 'event)))) ;;But this is behind the scenes.

(defclass A ()
  ((a (make-instance 'property :value 5))
   (a2 (binding self 'a))))

(defclass B ()
  ((a3 (make-instance 'A))
   (b :initform (binding self a3 a2))))

(defclass C ()
  ((b :initform (make-instance 'B))
   (c :iniform (binding self b b))))

(defvar c2 (make-instance 'C))
(get-prop c2 c) ;; c -> b -> a -> 5
(set-prop (path to a ...) 6) -> a(ivk) -> b(ivk) -> c(ivk)

Above, the basic idea of properties (kind of like dependency properties

Data graph
(def-data-graph G (make-node 10) (make-node (make-node 10) (make-node 20)))

;;The following will make a data grap called G with two nodes 'a' and one unnamed
(def-data-graph G (make-node :name a :value 15) (make-node 10)) 

;;i can then do the following to modify a's value
(def-data-graph G (make-node :name a :value 20) (make-node 10))

;;or in a simpler way:
(set-value a 25)

;since symbols are globally defined i can make another graph:
(def-data-graph G2 a (make-node :value (binding a value)))
;this graph contains a and has a node that binds its value to a's value. If a's value then change this value will change as well.

;it would also be possible to do this:
(def-data-graph G2 a (make-node :value (get-value a)))
;but that would only set the new value to that of a when G is created.

;it is also possible to bind to the symbol 'a'
(def-data-graph G2 (symbol-bind a))

;hence it is possible to bind to:
- values (rw)
- property objects (read-write)
- active property
- symbols (read-write)
- functions (lambda expressions) r 
- two way functions rw

;two way functions are simply two lambda expression. One for setting and one for getting
;binding in code

;value bindings are arbritary
(defvar b1 5)
(get-value b1)
(set-value b1 10)

(defvar b2 (make-property :value 5))
(set-value b2 15)
(get-value b2)

(defvar _b3 5)
(defvar b3 (symbol-binding _b3)))
(set-value b3 20)
(get-value b3)

(defvar b4 (lambda () 30))
(get-value b4)

(defvar b5 (function-binding (lambda() 40) (lambda(new-value) (print new-value))))
(set-value b5 50)
(get-value b5)

(defvar b6 (active-property (lambda(x) (+ x 5)) (lambda () (+ x 10)))) ;setters and getters
functions can be attached to the setters in b2, b3 and b5. But the place it would make most sense is with b2.



We need to bind to something...
(defvar some-data (...))
(ui-graph (grid :context some-data (row 0 (label :text (binding context name))))) 


;;examples in gui:
(defgraph uigraph1
    (make-grid :rows 2 :columns 2
	       (in-row 0 :rowspan 2 :content (button :text "hej"))
	       (in-row 1 
		       (in-column 1 :content (list 
					      (button :text "hej1")
					      (button :text "hej2"))))))
(defgraph uigraph2
    (make-stack-panel 
     :content (list 
	       (make-button :text "hej1") 
	       (make-button :text "hej2"))))

;;in audio processing:
(defgraph audio-graph (dac :channel 0 :channelspan 2
	  :input (osc :freq 440)))
(defgraph audio-graph (dac :channel 0 :channelspan 0
     :input (mult :input (list 
			  (osc :freq 440) 
			  (adsr 0.1 0.1 0.1 0.1 
				:input (beat :bpm 120)))))) 

;;mixed
(defgraph audio-graph3 
    (dac :channel 0 :channelspan 2
	 :input (mult 
		 :input (list (osc :freq 440)
			      (adsr 0.1 0.1 0.1 0.1 
				    :input (bang :name asdr-bang)))))) 
			      

(defgraph uigraph3
    (text)
    (button :text text 
	    :click (lambda () (bang-on asdr-bang))))

(defgraph uigraph4
    ((inherit framework))
    (grid :rows 2 :columns 2
	  (row 0 :span 2 :input (uigraph3 :text "123")
	  (row 1 :span 2 :input (uigraph3 :text "456")))))

defgraph symb objects body
symb is the symbol of the new graph
objects are the forwarded properties can be bound from inside and outside
body is the actual graph

uigraph objects are objects that refer to other uigraphs like they are templates
the forwarded values are those which are available as keywords-

Lets abandon the ui and audio for a while and go into the symbols

;;A forwards a variable A but does nothing to it.
(defgraph A ;;Create a template for a graph
    (a)
  ())

(defgraph B ;;Another graph template
    (c b)
  (make-A :name a-graph :c a))

(defnode b-graph (make-B :c 10 :b 20)) ;make an instance of a graph
(set-value b-graph c 10)
(get-value b-graph a-graph a)

;;But each object also needs some functionality
;;if the graph objects gets expanded into CLOS objects its possible to define methods
;;and getters and setters
;;Default implementation of set-c:
(defmethod set-c ((self B) value)
  (sub-set c value))


(defmethod b-graph set-a ((self B) value)
  value)

(defmethod b-graph get-a ((self B))
  (after-get-value self a))


What happens in the case of (set-value b-graph c 10)
it should first call the user-defined method for set c before going to a and calling set-a, with the value from set-c. If set-a changes the value, it must notify b through the normal route.
(defgraph C
    (d c)
   (make-graph A :a c :content (make-graph A :a c)))
C forwards 'c' to two instances of A. in this case they will try to synchronize so that if c is set on an instance of C, it will propagate to both instances of A. If 'a' changes on an instance of A, it will propagate back to C and then to the other instance of A. The rule could be that the data is only propagates if it is changed. Giving a small overhead
(defgraph symname forwards body)
(defnode symname body) ;;Defines or updates a graph instance

How are the values bound. The bindings can exhibit a one-to-many behaviour similar to what happens with events. But the updates are both ways. on set-value update if get-value has changed from before.

Events are different from properties. Events can be properties. Events are just one value hence everybody can bind to it. A bigger problem is clearing them. Probably they shouldent be used in this context since they include a lot of state.
(defgraph D
    (bang)
  (event :sink bang))

(defgraph E
    ()
  (D :bang (lambda (b) print b)))

;;a bang event is hence better simulated by a 'pulse'
(set-value obj p 1) ;;on 
(set-value obj p 0) ;;off again
;or pulse revisions
(set-value obj p 1) ;bang
(set-value obj p 2) ;bang
(set-value obj p 3) ;bang


(defgraph A ;name
    (a b c)  ;;forwards
  ()) ;;empty body

(defgraph B
    (d e)
  (A a: d :b 5 :c 5))

(defnode b B)

would expand to
(defclass A (graph-obj)
  ((a :reader get-a :writer set-a) (b :reader get-b :writer set-b) (c :reader get-c :writer set-c)))

*****************
(defgraph A
    (a) ())

(defmethod changed-a ((self A) value)
  (print value))

(defgraph B (a)
    (A a: a))

(defmethod changed-a ((self B) value)
  (print value))

(defnode C (make B :a 20))

(print (get-value C a))
(set-value C a 30)
;;if a has properties too
(set-value C a b 30)
**************
(defmacro defgraph (symname properties body)
  ())
(defmacro defnode (symname body)
  ())





**********
(defgraph A 
    (a)
  ())

(defgraph B
    (c d)
  (A :a c))
