(load-file "/home/user/code/animator/src/animator/utl.clj")

(ns narrator
  (:require [net.cgrand.enlive-html :as html] )
  (:use seesaw.core seesaw.mig seesaw.dev clojure.java.io clojure.math.numeric-tower animator.utl))

  (def TIMELINE-FILE "pub/tNarrator.clj")
  (def ELEMENTS (createElementsMap
                  ["/home/user/code/svg-lab/pub/index.html" "/home/user/code/svg-lab/pub/svg/narrator.svg"]
                  "narratorVidAnim"))
  (def TIMELINE-MAP (atom (getTimeline TIMELINE-FILE)))
  (def STATEMAP-FILE "pub/narratorStateMap.clj")
  (def STATEMAP (atom (getStateMap STATEMAP-FILE)))

 (defn writeJS []
     (map->timeline TIMELINE-MAP STATEMAP "/home/user/code/svg-lab/pub/js/narratorVideo/timeline.js" "elements.narratorVideo")
     (map->statefile STATEMAP  "/home/user/code/svg-lab/pub/js/narratorVideo/stateMap.js"  "elements.narratorVideo"))

; (createElementsFile "/home/user/code/svg-lab/pub/js/elements.js" "/home/user/code/svg-lab/pub/index.html" ["mainVidAnim"])

;;;;FRAME->CODE
(comment
(load-file "src/animator/narrator.clj")
(writeJS)



)


;;;;FRAME->CODE

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
 (def fcb3 (radio))
 
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
         :north (horizontal-panel :items [fcb1 gap fcb2 gap fcarea4 gap fcarea5 gap fcb3])
         :center fcsplit4
         :vgap 5 :hgap 5 :border 5))

 (def fcf (frame :minimum-size [200 :by 150] :content fcbp1 :size [200 :by 150] :title "Javscript Animator"))
 (-> fcf pack! show!)


(for [[k v] @STATEMAP :when (= 0 (v :keyFrame))] k)


  (listen fcb1 :action
      (fn [e]
            (if (not (number? (selection fclb1)))
               (let [ newIdx (Integer. (.getSize (.getModel fclb1)))
                      mapUpdate  (list (value fcarea2)) ]
                   (updateStateMap STATEMAP STATEMAP-FILE
                          newIdx          
                          (value fcarea1)
                          (value fcarea3)
                          (Integer. (selection fclb2))
                          (range (Integer. (value fcarea4)) (+ 1 (Integer. (value fcarea5)))))
                 
                   (config! fclb1 :model (list* "M" (keys @STATEMAP)))
                   (selection! fclb1 newIdx)
                   (updateTimeline
                        TIMELINE-MAP
                        TIMELINE-FILE
                        (Integer. (selection fclb2))
                        mapUpdate)
                   (writeJS))

               (let [ mapUpdate  (list (value fcarea2)) ]
;                      existingIdx (first (for [[k v] @STATEMAP :when
;                                               (= (Integer. (selection fclb2))(v :keyFrame))] k)) ]
;                 (if existingIdx
;                      (updateStateMap STATEMAP STATEMAP-FILE
;                           existingIdx          
;                          (value fcarea1)
;                          (value fcarea3)
;                          (Integer. (selection fclb2))
;                          (range (Integer. (value fcarea4)) (+ 1 (Integer. (value fcarea5)))))
                  (updateStateMap STATEMAP STATEMAP-FILE
                          (Integer. (selection fclb1))          
                          (value fcarea1)
                          (value fcarea3)
                          (Integer. (selection fclb2))
                          (range (Integer. (value fcarea4)) (+ 1 (Integer. (value fcarea5)))))
            
                  (updateTimeline
                     TIMELINE-MAP
                     TIMELINE-FILE
                     (Integer. (selection fclb2))
                     mapUpdate)
                 (writeJS)))))

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
               (let [ keyFrame (first (for [[k v] @STATEMAP :when (= (Integer. (selection fclb2))(v :keyFrame))] (v :keyFrame))) 
                      state (getStateRef STATEMAP (Integer. (selection fclb2)))
                       keyRef (getKeyFrameRef STATEMAP (Integer. (selection fclb2)))
                      contents (getFrameContents TIMELINE-MAP (int (selection fclb2))) ]
                 (if ( = (Integer. (selection fclb2)) keyFrame) (selection! fcb3 true) (selection! fcb3 false)) 
                 (cond
                   state
                   (do  
                     (selection! fclb1 state)
                     (value! fcarea4 (first ((@STATEMAP (getStateRef STATEMAP (int (selection fclb2)))):frameRefs)))
                     (value! fcarea5 (last ((@STATEMAP (getStateRef STATEMAP (int (selection fclb2)))):frameRefs))))
                   keyRef
                   (do
                     (selection! fclb1 keyRef)
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

;;;;;;ELEMENTS->FRAMES->CODE PANEL

 (def efclb1 (listbox :model []))
 (def efclb2 (listbox :selection-mode :multi-interval :model []))
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
 (config! efclb1 :model (list* "null" ELEMENTS ))

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

(listen efcb1 :action
         (fn [ e ]
           (let [frames (selection efclb2 {:multi? true}) ]
             (doseq [v frames]
               (removeFrame TIMELINE-MAP TIMELINE-FILE (Integer. v)))
           (selection! efclb1 (selection efclb1))
           (selection! fclb2 (selection fclb2)))))

;;;;NEW KEYFRAMES

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
         
