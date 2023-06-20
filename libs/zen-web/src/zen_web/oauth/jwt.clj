(ns zen-web.oauth.jwt
  (:require
   [cheshire.core :as json]
   [clojure.string :as str])
  (:import [java.util Base64]
           [java.security Signature SecureRandom]
           [javax.crypto.spec SecretKeySpec]
           [javax.crypto Mac]))

(def ^:private algs
  {:RS256 "SHA256withRSA"
   :RS384 "SHA384withRSA"
   :RS512 "SHA512withRSA"
   :HS256 "HmacSHA256"
   :HS384 "HmacSHA384"
   :HS512 "HmacSHA512"})

(defn decode64 [str]
  (.decode (java.util.Base64/getDecoder) str))

(defn encode64 [str]
  (.encodeToString (java.util.Base64/getEncoder) (if (string? str) (.getBytes str) str)))

(defn safe-encode [s]
  (-> (encode64 s)
      (str/replace #"\s" "")
      (str/replace "=" "")
      (str/replace "+" "-")
      (str/replace "/" "_")))

(defn safe-decode [s]
  (-> (case (mod (count s) 4)
        2 (str s "==")
        3 (str s "=")
        s)
      (str/replace "-" "+")
      (str/replace "_" "/")
      decode64))

(defn rsa-verify
  "Function to verify data and signature with RSA algorithm."
  [alg public-key body signature]
  (let [sig (doto (java.security.Signature/getInstance alg)
              (.initVerify public-key)
              (.update (.getBytes body)))]
    (.verify sig (safe-decode signature))))

(defn rsa-sign
  "Function to sign data with RSA algorithm."
  [key body & [alg]]
  (let [sig (doto (java.security.Signature/getInstance alg)
              (.initSign key (java.security.SecureRandom.))
              (.update (.getBytes body "UTF-8")))]
    (safe-encode (.sign sig))))

(defn crypto-eq?
  "Test whether two sequences of characters or bytes are equal in a way that
  protects against timing attacks. Note that this does not prevent an attacker
  from discovering the *length* of the data being compared."
  [a b]
  (let [a (map int a), b (map int b)]
    (if (and a b (= (count a) (count b)))
      (zero? (reduce bit-or (map bit-xor a b)))
      false)))

(defn hmac-sign
  "Function to sign data with HMAC algorithm."
  [key body alg]
  (let [hmac-key (javax.crypto.spec.SecretKeySpec. (.getBytes key "UTF-8") alg)
        hmac     (doto (javax.crypto.Mac/getInstance alg)
                   (.init hmac-key))]
    (safe-encode (.doFinal hmac (.getBytes body "UTF-8")))))

(defn hmac-verify
  "Function to verify data and signature with HMAC algorithm."
  [alg key body signature]
  (crypto-eq? signature (hmac-sign key body alg)))

(defn verify [jwt public-key]
  (let [alg (keyword (get-in jwt [:header :alg]))]
    (if-let [alg-name (get algs alg)]
      (case alg
        :RS256 (rsa-verify alg-name public-key (:body jwt) (:signature jwt))
        :HS256 (hmac-verify alg-name public-key (:body jwt) (:signature jwt)))
      (throw (Exception. (str "Unknown alg: " (pr-str (:header jwt))))))))

(defn parse [jwt-str]
  (let [parts (str/split jwt-str #"\.")]
    (when (= 3 (count parts))
      (let [[header claims signature] parts]
        {:header  (json/parse-string (String. (safe-decode header)) keyword)
         :claims  (json/parse-string (String. (safe-decode claims)) keyword)
         :body    (str header "." claims)
         :signature signature}))))

(defn sign [private-key claims & [alg]]
  (let [alg (keyword (or alg :RS256))
        header (safe-encode (json/generate-string {:alg (name alg) :typ "JWT"}))
        claims (safe-encode (json/generate-string claims))
        data (str header "." claims)
        alg-key (get algs alg)
        sig (case alg
              :RS256 (rsa-sign private-key data alg-key)
              :HS256 (hmac-sign private-key data alg-key))]
    (str data "." sig)))
