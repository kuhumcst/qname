(ns dk.cst.qname
  "A QName record and conversions between QNames, Keywords, and IRI strings."
  (:require [ont-app.vocabulary.core :as voc]
            [ont-app.vocabulary.format :as fmt]
            [clojure.string :as str])
  (:import [clojure.lang Named Keyword]))

(defrecord QName [prefix identifier]
  Object
  (toString [_] (str prefix ":" identifier))

  Named
  (getNamespace [_] prefix)
  (getName [_] identifier))

(defprotocol RDFResource
  "Conversions between different ways of representing RDF resources."
  (->qname [this] "Convert an RDF Resource into a QName.")
  (->keyword [this] "Convert an RDF Resource into an encoded keyword.")
  (->iri [this] "Convert an RDF Resource into a Resource IRI."))

(defn qname
  "Create a QName record from a `qname-str`, e.g. prefix:identifier."
  [qname-str]
  (apply ->QName (str/split qname-str #":")))

(defn iri-parts
  "Return [prefix-iri identifier] for a normalized `iri`."
  [iri]
  (next (re-matches (voc/namespace-re) iri)))

(defn resource-iri?
  [iri]
  (and (str/starts-with? iri "<")
       (str/ends-with? iri ">")))

(extend-protocol RDFResource
  QName
  (->qname [this] this)
  (->keyword [this] (keyword (fmt/encode-kw-ns (namespace this))
                             (fmt/encode-kw-name (name this))))
  (->iri [this] (let [ns-uri (voc/prefix-to-namespace-uri (namespace this))]
                  (str "<" ns-uri (name this) ">")))

  Keyword
  (->qname [this] (->QName (fmt/decode-kw-ns (namespace this))
                           (fmt/decode-kw-name (name this))))
  (->keyword [this] this)
  (->iri [this] (let [ns-uri (-> (namespace this)
                                 (fmt/decode-kw-ns)
                                 (voc/prefix-to-namespace-uri))]
                  (str "<" ns-uri (fmt/decode-kw-name (name this)) ">")))

  String
  (->qname [this] (let [iri (if (resource-iri? this)
                              (subs this 1 (dec (count this)))
                              this)]
                    (when-let [[prefix-iri identifier] (iri-parts iri)]
                      (->QName (-> (voc/namespace-to-ns)
                                   (get prefix-iri)
                                   (voc/ns-to-prefix))
                               identifier))))
  (->keyword [this] (->keyword (->qname this)))
  (->iri [this] (if (resource-iri? this)
                  this
                  (str "<" this ">"))))

(comment
  (qname "rdf:1234/foobar")
  (qname ":1234/foobar")
  (iri-parts "http://www.w3.org/2000/01/rdf-schema#123foo/bar")

  ;; Calling functions on the record itself
  (str (QName. "dn" "synset/1234"))
  (namespace (QName. "dn" "synset/1234"))
  (name (QName. "dn" "synset/1234"))

  ;; ->keyword examples
  (->keyword (QName. "dn" "synset/1234"))
  (->keyword (QName. "example" "1234"))
  (->keyword :rdf/foobar)
  (->keyword "<http://www.w3.org/2000/01/rdf-schema#123foo/bar>")

  ;; ->iri examples
  (->iri (QName. "rdf" "1234"))
  (->iri :rdf/foobar)
  (->iri "http://www.w3.org/2000/01/rdf-schema#123foo/bar")
  (->iri "<http://www.w3.org/2000/01/rdf-schema#123foo/bar>")

  ;; ->qname examples
  (->qname (QName. "dn" "synset/1234"))
  (->qname :rdf/foobar)
  (->qname "http://www.w3.org/2000/01/rdf-schema#foobar")

  ;; The grand test
  (= "<http://www.w3.org/2000/01/rdf-schema#123foo/bar>"
     (-> "<http://www.w3.org/2000/01/rdf-schema#123foo/bar>"
         ->iri
         ->qname
         ->keyword
         ->iri
         ->keyword
         ->qname
         ->iri))
  #_.)
