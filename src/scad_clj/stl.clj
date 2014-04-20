(ns scad-clj.stl
	(:use [clojure.core.match :only (match)])
	(:use [clojure.pprint])
	(:use [scad-clj.model])
	(:use [scad-clj.scad])
	(:require [nio.core :as nio])
	(:use [gloss core io]))

(defn cadr [x] ((x 1) 0))

;; Thanks jackrusher:
;; https://gist.github.com/jackrusher/8958272
(defcodec stl-codec
					{:header (string :ascii :length 80)
					 :shape  (repeated (ordered-map :normal [:float32-le :float32-le :float32-le]
																					:v1 [:float32-le :float32-le :float32-le]
																					:v2 [:float32-le :float32-le :float32-le]
																					:v3 [:float32-le :float32-le :float32-le]
																					:attr-count :uint16)
														 :prefix :uint32-le)})

(defn import-stl [filename]
	(decode stl-codec (to-buf-seq (nio/mmap filename))))

(defn get-shapes [stl]
	(stl :shape))


(defn point-array [shapes]
	(map-indexed
		(fn [index shape]
			[[(get shape :v1)
				 (get shape :v2)
				 (get shape :v3)]
				[vector index]])
		shapes))


(defn polyhedron-from-point-array [point-array]
	(let [points (doall (partition 3 (flatten (map first point-array))))
				faces (map identity
									 (map (fn [x]
													(let [i (* 3 x)]
														[i
														 (+ 1 i)
														 (+ 2 i)]))
												(map cadr point-array)))]
		(polyhedron points faces)))

(defn polyhedron-from-stl [filename]
	(polyhedron-from-point-array
		(point-array
			(get-shapes
				(import-stl filename)))))
