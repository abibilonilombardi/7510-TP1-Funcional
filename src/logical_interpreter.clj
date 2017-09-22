(ns logical-interpreter)
(require '[clojure.string :as str])

(defn fact? [cadena-a-analizar]
  "Retorna true o false si la cadena-a-analizar cumple con la sintaxis de fact"
    (if (nil? (re-matches #"^[a-zA-Z\s]*\([a-zA-Z\s,]*\)\s*\.\s*$" cadena-a-analizar))
        false true)
)

(defn rule? [cadena-a-analizar]
  "Retorna true o false si la cadena-a-analizar cumple con la sintaxis de rule"
    (if (nil? (re-matches #"^[a-zA-Z\s]*\([a-zA-Z\s,]*\)\s*\:\-[a-zA-Z,\s\(\)]*\.\s*$" cadena-a-analizar))
        false true)
)

(defn fact-o-rule? [cadena-a-analizar]
    (if (or (rule? cadena-a-analizar) (fact? cadena-a-analizar))
        true false)
)

(defn preprocesar-base-de-datos [base-de-datos]
  "Retorna una lista de facts/rules ingresados"
  (map #(str/trim %) (filter #(not (str/blank? %)) (str/split base-de-datos  #"\n")))
)

(defn procesar-bdd [base-de-datos]
  "Retorna una lista de la las rules/fact cuando cumplen con la sintaxis y nil en caso 
  contrario"
  (map #(if (fact-o-rule? %) % nil) (preprocesar-base-de-datos base-de-datos))
)

; //CREAR EL MAP DE DEFINICIONES

(defn parsear-definicion [cadena-a-parsear]  
  "Parser de las definiciones de fact/rule 'definicion(val1,...,valn)'"
	     (rest (re-find #"^([a-zA-Z\s]*)\(([a-zA-Z\s,]*)\)\s*[\.\:]?" cadena-a-parsear))
	 )


;FIJARSE SI DE LA PARTE DE HASHMAP DA HACER UNA FUNCION PORQUE LO USO EN LOS DOS
(defn crear-map-definiciones-existentes [bdd]
  "crea un map con todos las definiciniones ingresadas en la base
  CLAVES: nombre de fact/rule
  VALOR: lista de todos los parametros del fact/rule"
  (apply merge-with into (map  #(hash-map (keyword (first %)) (list (second %)))  
      (map #(parsear-definicion %) bdd)))
)

; //CREAR EL MAP DE RULES
(defn parsear-rule [cadena-a-parsear]  
  "Parser de las definiciones de fact/rule 'definicion(val1,...,valn)'"
	     (rest (re-find #"^([a-zA-Z\s]*)\([a-zA-Z\s,]*\)\s*\:\-([a-zA-Z,()\s]*\))\.$" cadena-a-parsear))
	 )

(defn crear-map-rules-existentes [bdd]
  "crea un map con todos las definiciniones ingresadas en la base
  CLAVES: nombre de fact/rule
  VALOR: lista de todos los parametros del fact/rule"
  (apply merge-with into (map  #(hash-map (keyword (first %)) (list (second %)))  
      (map #(parsear-rule %) (filter #(rule? %) bdd ))))
)

; /**********************
; esta seria la generica mandar esta funcion en las otras qu lo usan
(defn procesar-query [query]
  "crea un map de la query"
  (let [query-lista (parsear-definicion query)]
       (hash-map (keyword (str/trim (first query-lista))) (second query-lista))
       )
)

(defn crear-lista-tuplas [una-lista otra-lista]
  "Retorna una lista con los tuplas con los valores de las listas en orden de los indices"
  ; QUEDARME CON EL DE LOGITUD MIN Y DEVOLVER ESO O IF SI NO SON IGUALES NIL
  (for [x (range 0 (alength (into-array una-lista)) )] (list (nth una-lista x) (nth otra-lista x))))

(defn reemplazar [cadena lista-valor-reemplazo]
    (reduce #(str/replace %1 (re-pattern(first %2)) (second %2)  ) cadena lista-valor-reemplazo)
)

(defn evaluar-rule [argumento-rule valores-rule cadena-facts]
  (let [argumento-valor (crear-lista-tuplas(str/split (first argumento-rule)  #"\,\s*") (str/split (first valores-rule)  #"\,\s*"))]
       (re-seq #"[a-zA-Z\s]*\([a-zA-Z\s,]*\)\s*" (reemplazar cadena-facts argumento-valor))
  )
)

(defn evaluar-fact [map-bdd query]
  (if (not-any? #(= (first (map val query)) %) (some map-bdd (map key query)))
      false
      true
  )
)
  
(defn evaluar-consulta [bdd query]
  (let [map-bdd (crear-map-definiciones-existentes bdd)
        map-rules (crear-map-rules-existentes bdd)
        valores-a-matchear (some map-bdd (map key query))]
      (if (nil? (some map-rules (map key query))) (evaluar-fact map-bdd query)
                (every? #(true? %) (map #(evaluar-fact map-bdd (procesar-query %)) (evaluar-rule valores-a-matchear (map val query) (first (some map-rules (map key query)))) ))
      )
  )
)

(defn evaluate-query [base-de-datos query-ingresada]
  "Devuelve true o false a la consulta ingresada sobre la base de datos. Retorna nil si la base de datos o 
  la query no tienen formato correcto
  FORMATO DE LA BASE DE DATOS: una fact/rule por linea, puede haber lineas en blanco entre datos
  FORMATO DE FACTS/RULES: "
  (let [bdd (procesar-bdd base-de-datos)
        query (procesar-query query-ingresada)] 
    (if (contains? (into #{} bdd) nil)  nil (evaluar-consulta bdd query)))
)



