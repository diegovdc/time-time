# time-tiem

A Polychronic library for precisely sequencing events in time.

The aim of `time-tiem` is to develop low-ish level composable functions for the flexible development of higher level sequencers that can do anything from common sequencing tasks to highly coordinated polytemporality. 

The aim includes the development of such higher level constructs. 

It is important to note that these sequencing tools are agnostic of medium, so it is possible to sequence audio, video, text or whichever thing that can be put into a function call.





## Usage

### ref-rain

The focus of `ref-rain` is to provide a good live coding experience by allowing the sequencer to be modified in real time without losing track of the following events in it's timeline. 

``` clojure
(require '[time-time.dynacan.players.gen-poly :as gp])

(gp/ref-rain
    :id :my-sequencer ;; identifier of the sequencer, useful when recompiling (will apply changes on the next event, rather than restarting the sequencer)
    :durs [3 2 3 3 2] ;; a vector of durations, this are durations in beats and will run at the tempo specified below.
    :tempo 120
    :ratio 1 ;; `1/2` will make everything run twice as fast, etc.
    :on-event (fn [{:keys [data]}]
                ;; `data` is the current event data and will print something like:
                #_{:durs [3 2 3 3 2]
                   :index 0
                   :started-at 1660746728661
                   :current-event {:dur 3, :event-dur 1500N}
                   :playing? true
                   :ratio 1
                   :dur 3
                   :on-event (fn [] #_some-function)}
                (println data)))
                
(gp/stop :my-sequencer) ;; stop this sequencer
(gp/stop) ;; stop all sequencers
```

The `on-event` macro is useful for accessing the most commonly required values of an event, and provides the at-index function:

``` clojure
(gp/ref-rain
    :id :my-sequencer-2
    :durs [3 2 3 3 2]
    :tempo 120
    :on-event (gp/on-event
                ;; notice that we just need to pass a function call to execute
                (println index
                         dur    ;; event dur as stated in the durs vector
                         dur-ms ;; duration converted to milliseconds
                         (at-index [:one :two :three]) ; provided by on-event, get the value at index, will wrap using `mod` if index overflows
                         (keys data))))
                         
(gp/stop)
```

<!--
## Time units

Conventions:

`:elapsed` Elapsed time in abstract time units (no real temporal value, can be mapped to milliseconds, seconds, etc.)
`:elapsed-ms` Elapsed time in milliseconds

`:echoic-distance`
`:echoic-distance-event-qty`


;;TODO
:cp-at
:\*-at

;; TODO
sequencing-3 `:current-event` is not clear

### What are Abstract time units?

# WIP

Time management:

tiempo en milisecs:
ej: :elapsed-ms

todo lo demas q sean unidades abstractas de tiempo: Unidades de tiempo. Explicar abstract time units (ATU)

los miliseconds es UTC son utiles para relacinarse con el tiempo de la maquina.

ejs:

:elapsed 1.2
:elapsed-ms 1923873940

---

interval-from-cp (echDist)
events-from-cp (qty)

function to calculate the echoic distance (if needed)

Echoic dist = diferencia entre interval-from-cp-v1 and interval-from-cp-v2

echoic distance son datos q se pueden usar!

//////
event<->cp
-->

## Note
This library is a work in progress and mostly a workshop for myself, so the code is not polished as it should. If you are using this library, feel free to let make me aware of so that I can take more care of the code and the documentation.


## License

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
