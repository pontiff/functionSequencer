(ns animator
  (:require [net.cgrand.enlive-html :as html] )
  (:use seesaw.core seesaw.mig seesaw.dev clojure.java.io clojure.math.numeric-tower))

(defn init []
  
 (defn getTimeline [filePath]
  (if (.exists (as-file filePath))
    (into (sorted-map) (read-string (slurp filePath)))
    (do
      (spit filePath {})
      (into (sorted-map) (read-string (slurp filePath))))))

 (defn truncNum [numVal places]
  (bigdec (/ (round (* numVal (expt 10 places))) (expt 10 places))))
 
 (defn addToTimeline [ timeMap timeFile indx newValue ]
  (let [ newMap (assoc @timeMap indx (concat (@timeMap indx) (list newValue))) ]
    (reset! timeMap newMap)
    (spit timeFile @timeMap)))

 (defn removeFrame [ timeMap timeFile indx ]
   (let [newMap (into (sorted-map) (remove #(= (first %) indx) @timeMap)) ]
     (reset! timeMap newMap)
     (spit timeFile @timeMap)))
 
 (defn updateTimeline [ timeMap timeFile indx newValue ]
   (let [ newMap (assoc @timeMap indx newValue ) ]
     (reset! timeMap
             (into (sorted-map) (for [ [k v] newMap :when (first v) ] [ k v ])))
     (spit timeFile @timeMap)
     @timeMap))

 (defn getStateRef [ stateMap frameNum ]
  (first (for [[k v] @stateMap :when (not (empty? (filter #(= % frameNum) (v :frameRefs)))) ]
           k)))
 
 (defn getKeyFrameRef [ stateMap frameNum ]
   (first (for [[k v] @stateMap :when (= (v :keyFrame) frameNum) ] k)))
 
 (defn timeline->jsArrayFile [ timeline stateMap targetFile attachTo arrNm ]
   (let [arrStr (str attachTo "." arrNm) ]
     (spit targetFile (str arrStr " = [];\n\n"))
     (doseq [ [ k v ] @timeline]
       (let [stateRef (getStateRef stateMap k)
             keyRef (getKeyFrameRef stateMap k) ]
       (cond  
          stateRef
          (do  
            (spit targetFile
               (str
                 arrStr "[" k "]" " = function(){\n"
                 "if ("attachTo".state != "stateRef")\n{\n"
                 attachTo".takeDowns["attachTo".state]();\n"
                 attachTo".setups["stateRef"]();\n"
                 attachTo".state = " stateRef ";\n}\n"
                 (apply str (interpose "\n" v))
                 attachTo".state = "stateRef";\n}\n\n")
               :append true))
          keyRef
           (do
             (spit targetFile
                (str
                  arrStr "[" k "]" " = function(){\n"
                  "if ("attachTo".state != " keyRef ")\n{\n"
                  attachTo".takeDowns["attachTo".state]();\n"
                 (apply str (interpose "\n" v))"\n"
                 attachTo".state = " keyRef ";\n}}\n\n")
               :append true)))))))

  (defn stateMap->jsArrayFile [ stateMap takeDownsFile setupsFile attachTo ]
  (let [takeArr  (str attachTo ".takeDowns") 
        stateArr (str attachTo ".setups") ]
    (spit takeDownsFile
          (str
           takeArr " = [];\n\n"
           takeArr"[0] = function(){\n"
           attachTo ".state = 0;\n}\n\n"))
   (spit setupsFile (str stateArr " = [];\n\n"))
   (doseq [[k v] stateMap]
     (spit takeDownsFile
           (str
              takeArr "[" k "]" " = function(){\n"
              (v :takeDown)
              "\n"attachTo".state = 0;}\n\n")
           :append true)
     (spit setupsFile
           (str
              stateArr "[" k "]" " = function(){\n"
              (v :state)
              "\n}\n\n")
           :append true))))

 
 (defn getFramesOfElement [ eName timeLine ]
  (for [ [k v] timeLine
         :when
         (#(re-find
           (re-pattern (str "(?<![\\w])" eName "(?![\\w])")) (apply str v))) ]
    (list k
          (filter
           #(re-find
             (re-pattern (str "(?<![\\w])" eName "(?![\\w])")) %) v ))))

(defn createElementsMap [ sourceFiles classNameKey ]
   (let [ sourceRefs
         (for [ x sourceFiles ]
           (html/select (html/html-resource (java.io.File. x)) [(keyword (str "."classNameKey))])) ]
     (apply concat (for [ y sourceRefs ] (for [ x y ] ((x :attrs):id))))))
 
 (defn createElementsFile [ targetFile sourceFile classNameVector ]
   (if (.exists (as-file targetFile)) nil (spit targetFile  ""))
   (doseq [ v classNameVector ]
     (let [ sourceMap (html/select (html/html-resource (java.io.File. sourceFile)) [(keyword (str "."v))])]
       (doseq [ x sourceMap ]
         (spit targetFile
               (str "elements."
                    ((x :attrs):id)
                    " = document.getElementById(\""
                    ((x :attrs):id)"\");\n")  :append true)))))

 (defn getFrameContents [ frameMap idx ]
         (apply str
            (interpose "\n"
                       (@frameMap idx))))

 (defn display [frame content]
   (config! frame :content content)
   content)
 
 (defn getStateMap [filePath]
  (if (.exists (as-file filePath))
    (into (sorted-map) (read-string (slurp filePath)))
    (do
      (spit filePath {})
      (into (sorted-map) (read-string (slurp filePath))))))
  
(defn addFramesToState [ stateMap stateFile indx newState newTakeDown frameRange]
   (let [  newMap (into (sorted-map) 
                  (assoc @stateMap indx
                                      {
                                       :state newState
                                       :keyFrame ((@stateMap indx):keyFrame)
                                       :takeDown newTakeDown
                                       :frameRefs (list* frameRange)})) ]
    (reset! stateMap newMap)
    (spit stateFile @stateMap)))


 (defn addStateToMap [ stateMap stateFile indx newState newTakeDown keyFrame]
   (let [  newMap (into (sorted-map) 
                  (assoc @stateMap indx
                                      {
                                       :state newState
                                       :takeDown newTakeDown
                                       :keyFrame keyFrame
                                       :frameRefs '()})) ]
    (reset! stateMap newMap)
    (spit stateFile @stateMap)))



 (defn removeFromStateMap [ stateMap stateFile indx ]
   (let [ newMap (into (sorted-map) (dissoc @stateMap indx))  ]
    (reset! stateMap newMap)
    (spit stateFile @stateMap)))
 
(comment

 (defn switch2test []
   (def TIMELINE-FILE "pub/test.clj")
   (def ELEMENTS (createElementsMap ["/home/user/code/svg-lab/pub/index.html"] "mainVidAnim"))
   (def TIMELINE-MAP (atom (getTimeline TIMELINE-FILE ) ))
   (def STATEMAP-FILE "pub/testStateMap.clj")
   (def STATEMAP (atom (getStateMap STATEMAP-FILE)))
   (defn writeJS []
     (timeline->jsArrayFile TIMELINE-MAP STATEMAP "pub/testTimeline.js" "elements.mainVideo" "timeline")
     (stateMap->jsArrayFile @STATEMAP "pub/testTakeDowns.js"
                                      "pub/testSetups.js"
                                      "elements.mainVideo")
     )) (switch2test) (writeJS)


  
 (defn switch2main []
   (def TIMELINE-FILE "pub/tMain.clj")
   (def ELEMENTS (createElementsMap ["/home/user/code/svg-lab/pub/index.html"] "mainVidAnim"))
   (def TIMELINE-MAP (atom (getTimeline TIMELINE-FILE ) ))
   (def STATEMAP-FILE "pub/mainStateMap.clj")
   (def STATEMAP (atom (getStateMap STATEMAP-FILE)))
   (defn writeJS []
     (timeline->jsArrayFile TIMELINE-MAP STATEMAP "/home/user/code/svg-lab/pub/js/mainVideo/timeline.js" "elements.mainVideo" "timeline")
     (stateMap->jsArrayFile @STATEMAP "/home/user/code/svg-lab/pub/js/mainVideo/takeDowns.js"
                                      "/home/user/code/svg-lab/pub/js/mainVideo/setups.js"
                                      "elements.mainVideo")
     )) (switch2main) (writeJS)

 (defn switch2narrator []
   (def TIMELINE-FILE "pub/tNarrator.clj")
   (def ELEMENTS (createElementsMap
                  ["/home/user/code/svg-lab/pub/index.html" "/home/user/code/svg-lab/pub/svg/narrator.svg"]
                  "narratorVidAnim"))
   (def TIMELINE-MAP (atom (getTimeline TIMELINE-FILE)))
   (def STATEMAP-FILE "pub/narratorStateMap.clj")
   (def STATEMAP (atom (getStateMap STATEMAP-FILE)))

   (defn writeJS []
    (timeline->jsArrayFile TIMELINE-MAP STATEMAP "/home/user/code/svg-lab/pub/js/narratorVideo/timeline.js" "elements.narratorVideo" "timeline")
    (stateMap->jsArrayFile @STATEMAP "/home/user/code/svg-lab/pub/js/narratorVideo/takeDowns.js"
                                      "/home/user/code/svg-lab/pub/js/narratorVideo/setups.js"
                                      "elements.narratorVideo")))
    

(createElementsFile "/home/user/code/svg-lab/pub/js/elements.js" "/home/user/code/svg-lab/pub/index.html" ["mainVidAnim"])


)


) (init) 


;;;;;;ELEMENTS->FRAMES->CODE PANEL

(defn initEFCP []
 (def efclb1 (listbox :model []))
 (def efclb2 (listbox :model []))
 (def efcb1 (button :text "Delete"))
 (def efcarea (text :multi-line? true :editable? false))
 (def efcsplit2 (left-right-split (scrollable efclb2) (scrollable efcarea)))
 (def efcsplit1 (left-right-split (scrollable efclb1) efcsplit2 :divider-location 1/6))
 (def efc-ff (frame :minimum-size [600 :by 100] :size [600 :by 100] :title "Javscript Animator"))
 (def efcbp (border-panel
           :north (horizontal-panel :items [efcb1])
           :center efcsplit1
           :vgap 5 :hgap 5 :border 5))

 (display efc-ff efcbp)
 (-> efc-ff pack! show!)
 
 (config! efclb1 :model ELEMENTS )

 (listen efclb1 :selection   
        (fn [e]
               (config! efclb2 :model
                   (for [x (getFramesOfElement (selection e) @TIMELINE-MAP) ] (first x)))
               (.setSelectedIndex efclb2 (.getFirstVisibleIndex efclb2))))

 (listen efclb2 :selection (fn [e]
                         (let [ select (selection e) ]
                         (if (= nil select)
                           nil
                           (.setText efcarea  
                                     (apply str
                                            (interpose "\n"
                                                       (first (for [ v (getFramesOfElement (selection efclb1) @TIMELINE-MAP)
                                                                 :when (= (first v) (int select)) ]
                                                                (second v))))))))))
(show-options (text))

 (listen efcb1 :action
         (fn [ e ]
           (let [frame (Integer. (selection efclb2)) ]
           (removeFrame TIMELINE-MAP TIMELINE-FILE frame)
           (selection! efclb1 (selection efclb1))
           (selection! fclb2 (selection fclb2)))))

 
 ) (initEFCP)

;;;;FRAME->CODE

(defn initFC []

 (def gap [:fill-h 5])
 (def fclb1 (listbox :model (list* "M" (keys @STATEMAP)) :layout-orientation :horizontal-wrap :fixed-cell-height 20))
   (.setVisibleRowCount fclb1 1)
   (.setFixedCellWidth fclb1 40)
   (.setHorizontalAlignment (.getCellRenderer fclb1) javax.swing.JLabel/CENTER)

 (def fclb2 (listbox :model (range 10000)  :layout-orientation :horizontal-wrap :fixed-cell-height 20 ))
   (.setVisibleRowCount fclb2 1)
   (.setFixedCellWidth fclb2 40)
   (.setHorizontalAlignment (.getCellRenderer fclb2) javax.swing.JLabel/CENTER)

 (def fcb1 (button :text "Save"))
 (def fcb2 (button :text "Delete"))

 (def fcarea1 (text :multi-line? true  ))
 (def fcarea2 (text :multi-line? true  ))
 (def fcarea3 (text :multi-line? true  ))

 (def fcarea4 (text :multi-line? false :editable?  true )) 
 (def fcarea5 (text :multi-line? false :editable?  true ))
 (config! fcarea4 :size [50 :by 25])
 (config! fcarea5 :size [50 :by 25])
 
 (def fcsplit1 (top-bottom-split (scrollable fcarea2) (scrollable fclb1)))
 (def fcsplit2 (left-right-split (scrollable fcarea1) fcsplit1))
 (def fcsplit3 (left-right-split fcsplit2 (scrollable fcarea3)))
 (def fcsplit4 (top-bottom-split fcsplit3 (scrollable fclb2)))

 (def fcbp1 (border-panel
         :north (horizontal-panel :items [fcb1 gap fcb2 gap fcarea4 gap fcarea5])
         :center fcsplit4
         :vgap 5 :hgap 5 :border 5))

 (def fcf (frame :minimum-size [200 :by 150] :content fcbp1 :size [200 :by 150] :title "Javscript Animator"))
 (-> fcf pack! show!)

 (listen fcb1 :action
      (fn [e] 
        (if (not (number? (selection fclb1)))
          (let [ newIdx (Integer. (.getSize (.getModel fclb1)))
                 mapUpdate  (list (value fcarea2) ) ]

            (addStateToMap STATEMAP STATEMAP-FILE
                          newIdx          
                          (value fcarea1)
                          (value fcarea3)
                          (Integer. (selection fclb2)))
            
            (config! fclb1 :model (list* "M" (keys @STATEMAP)))
            (selection! fclb1 newIdx)
            
            (updateTimeline
                 TIMELINE-MAP
                 TIMELINE-FILE
                 (Integer. (selection fclb2))
                  mapUpdate)
            (selection! fclb1 newIdx))
          
          (let [ mapUpdate  (list (value fcarea2)) ]

            (if (and (re-find #"\d+" (value fcarea4))
                     (re-find #"\d+" (value fcarea5)))
              (do
                (addFramesToState  STATEMAP STATEMAP-FILE
                          (Integer. (selection fclb1))          
                          (value fcarea1)
                          (value fcarea3)
                          (range (Integer. (value fcarea4)) (+ 1 (Integer. (value fcarea5)))))
            
                (updateTimeline
                   TIMELINE-MAP
                   TIMELINE-FILE
                   (Integer. (selection fclb2))
                    mapUpdate)))))))

 (listen fcb2 :action
      (fn [e] 
        (if (= (selection fclb1) "M") nil
           (do
             (removeFromStateMap STATEMAP STATEMAP-FILE
                                 (Integer. (selection fclb1)))
            (config! fclb1 :model (list* "M" (keys @STATEMAP)))
            (selection! fclb1 "M")))))

 (listen fclb2 :selection
         (fn [e]
           (if (selection e) 
             (do   
               (let [ state (getStateRef STATEMAP (Integer. (selection fclb2)))
                      keyframe (getKeyFrameRef STATEMAP (Integer. (selection fclb2)))
                      contents (getFrameContents TIMELINE-MAP (int (selection fclb2))) ]

                (cond
                  state
                   (do  
                     (selection! fclb1 state)
                     (value! fcarea4 (first ((@STATEMAP (getStateRef STATEMAP (int (selection fclb2)))):frameRefs)))
                     (value! fcarea5 (last ((@STATEMAP (getStateRef STATEMAP (int (selection fclb2)))):frameRefs))))
                  keyframe
                   (do
                     (selection! fclb1 keyframe)
                     (value! fcarea4 "")
                     (value! fcarea5 ""))
                  :else
                   (do
                     (selection! fclb1 "M")
                     (.setText fcarea4 "")
                     (.setText fcarea5 "")))
                 (if contents
                   (.setText fcarea2 contents)
                   (.setText fcarea2 "")))))))
 
 (listen fclb1 :selection
        (fn [e]
          (if (number? (selection e)) 
            (do
              (config! fcarea1 :text ((@STATEMAP (Integer. (selection e))):state) )
              (config! fcarea3 :text ((@STATEMAP (Integer. (selection e))):takeDown)))
            (do
              (config! fcarea1 :text "")
              (config! fcarea3 :text "")))))

;;;;NEW KEYFRAMES

 )                            
(initFC) 

(defn initNCF []

 (def ncf (frame :minimum-size [550 :by 150] :size [550 :by 200] :title "Javscript Animator"))
 (-> ncf pack! show!)

 (def nclbl0 (label :text "Code" :maximum-size [400 :by 20]))
 (def nclbl1 (label :text "VAR" :size [60 :by 20]))
 (def nclbl2 (label :text "sVal" :size [60 :by 20]))
 (def nclbl3 (label :text "eVal" :size [60 :by 20]))
 (def nclbl4 (label :text "sFrame" :size [60 :by 20]))
 (def nclbl5 (label :text "eFrame" :size [60 :by 20]))
 (def nclbl6 (label :text "Easing" :size [60 :by 20]))

 (def ncfield0 (text :multi-line? true :size [800 :by 400]))
 (def ncfield1 (text :size [100 :by 20]))
 (def ncfield2 (text :size [100 :by 20]))
 (def ncfield3 (text :size [100 :by 20]))
 (def ncfield4 (text :size [100 :by 20]))
 (def ncfield5 (text :size [100 :by 20]))
 (def ncfield6 (text :size [100 :by 20]))

 
 (def ncb (button :text "Create"))

(def ncpnl (mig-panel :constraints ["wrap 2"] :items [ [ncfield1][nclbl1]
                                                       [ncfield2][nclbl2]
                                                       [ncfield3][nclbl3]
                                                       [ncfield4][nclbl4]
                                                       [ncfield5][nclbl5]
                                                       [ncfield6][nclbl6] ]))
                                                           

 (def ncsplit (left-right-split (scrollable ncfield0) ncpnl))

 (display
  ncf
 (border-panel
  :north (horizontal-panel :items [ ncb ])
  :center ncsplit
           :vgap 5 :hgap 5 :border 5))

 (listen ncb :action
         (fn [e]
           (if (= "" (value ncfield1))
             (do
               (addToTimeline TIMELINE-MAP
                            TIMELINE-FILE
                            (Integer. (value ncfield4))
                            (clojure.string/replace (value ncfield0) #"ELEM" (selection efclb1))))

             (do
               (let [ baseRange (range 0 (+ 1 (- (Integer. (value ncfield5)) (Integer. (value ncfield4)))))
                      step  (* (/ 1 (last baseRange))
                               (- (rationalize (Float/parseFloat (value ncfield3))) (rationalize (Float/parseFloat (value ncfield2)))))
                      start (rationalize (Float/parseFloat (value ncfield2))) ]

                (doseq [[k v] (partition 2
                                         (interleave
                                          (range (Integer. (value ncfield4)) (+ 1 (Integer. (value ncfield5))))
                                          (for [ x baseRange  ] (truncNum (+ start (* x step))3)))) ]
                (addToTimeline
                  TIMELINE-MAP
                  TIMELINE-FILE
                  k
                  (clojure.string/replace
                   (clojure.string/replace (value ncfield0) (re-pattern(value ncfield1)) (str v))
                   #"ELEM" (selection efclb1)))))))

           (selection! efclb1 (value efclb1))
           (selection! fclb2 (selection fclb2))))
             
  )


(init)
(switch2narrator)
(initEFCP)
(initFC)
(initNCF)
(switch2main)
(writeJS)


