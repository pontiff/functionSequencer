(ns animator.utl
  (:require [net.cgrand.enlive-html :as html] )
  (:use seesaw.core seesaw.mig seesaw.dev clojure.java.io clojure.math.numeric-tower))

(defn map->statefile [ stateMap stateFile attachTo ]
     (spit stateFile (str attachTo".state = ["(apply str (interpose ", " (repeat (count @stateMap) 0)))"];\n"))
     (spit stateFile (str attachTo".stateMap = [\n") :append true)
     (spit stateFile
           (str (apply str (for [ [k v]  @stateMap]
                        (str "[\nfunction() {\n"
                             (v :takeDown)"},\n"
                             "function() {\n"
                             (v :state) "}\n"
                             "],\n")))
                "];")
           :append true))

(defn map->timeline [ timeline stateMap targetFile attachTo ]
  (let [ frameKeys (for [[k v] @timeline]
                     (list k (for [[ x y] @stateMap ]
                               (if (some #(= k %) (y :frameRefs))
                                   1
                                   0)))) ]
    (spit targetFile (str attachTo".timeline = [];\n\n"))
    (doseq [[ k v ] frameKeys]
            (spit targetFile
               (str
                attachTo".timeline[" k "] = function(){\n"
                "var state = [" (apply str (interpose ", " v)) "];\n"
                "for ( var i = 0; i < state.length; i++) {\n"
                   "if ( state[i] != " attachTo".state[i])"
                       attachTo".stateMap[i][state[i]]();}\n"
                attachTo".state = state;\n"   
               (apply str (@timeline k))"\n"
               "}\n\n")
               :append true))))
                    
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
                 (apply str (interpose "\n" v))"\n"
                 attachTo".state = "stateRef";\n}\n\n")
               :append true))
          keyRef
           (do
             (spit targetFile
               (str
                arrStr "[" k "]" " = function(){\n"
                "if (!"attachTo".paused)\n{\n"
                   "if ("attachTo".state != " keyRef ")\n{\n"
                         attachTo".takeDowns["attachTo".state]();\n"
                         (apply str (interpose "\n" v))"\n"
                         attachTo".state = " keyRef ";\n}}\n"
                "else if ("attachTo".state != " keyRef "){\n"
                      attachTo".takeDowns["attachTo".state]();\n"
                      attachTo".setups["keyRef"]();\n"
                      attachTo".state = " keyRef ";\n"
                      "}}\n\n")
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
           (re-pattern (str "(?<![\\w])" eName "(?![\\w])")) (apply str (interpose "\n" v)))) ]
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


 (defn updateStateMap [ stateMap stateFile indx newState newTakeDown keyFrame frameRange]
   (let [  newMap (into (sorted-map) 
                  (assoc @stateMap indx
                                      {
                                       :state newState
                                       :takeDown newTakeDown
                                       :keyFrame keyFrame
                                       :frameRefs frameRange})) ]
    (reset! stateMap newMap)
    (spit stateFile @stateMap)))



 (defn removeFromStateMap [ stateMap stateFile indx ]
   (let [ newMap (into (sorted-map) (dissoc @stateMap indx))  ]
    (reset! stateMap newMap)
    (spit stateFile @stateMap)))




