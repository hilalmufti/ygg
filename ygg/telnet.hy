(require
  hyrule [-> loop])

(import 
 socket
 toolz [first drop count cons assoc]
 hy
 hyrule [recur])


(setv *newline* "\r\n")
(setv *http-port* 80)

(defn rest [xs]
  (drop 1 xs))

(defn second [xs]
  (get xs 1))

(defn reverse [xs]
  (list (reversed (list xs))))

(setv make-symbol hy.models.Symbol)

; TODO: write my own split
(defn split [s sep]
  [])

(defn newline? [s]
  (= s *newline*))

(defn split-scheme [s]
  (.split s "://" 1))

(defn split-host [s]
  (.split
   (+ s (if (in "/" s) "" "/"))
   "/"
   1))

(defn parse-url [s]
  (let [[scheme rest] (split-scheme s)
        [host path] (split-host rest)]
    [scheme host (+ "/" path)]))

(defn make-url [scheme host path]
  (assert (= scheme "http"))
  ['url scheme host path])


(setv make-socket socket.socket)


(defn utf8 [s]
  (.encode s "utf8"))


(defn get? [method]
  (= method 'get))


(defn method->str [method]
  (cond
    (get? method) "GET"
    :else (raise (NotImplementedError))))


(defn parse-version [s]
  (let [[_ version] (.split s "/")
        [major minor] (.split version ".")]
    [(int major) (int minor)]))


(defn make-version [major minor]
  ['version major minor])


(defn version->str [version]
    (let [[_ major minor] version]
        (+ "HTTP/" (str major) "." (str minor))))

; TODO: add invariant checking for everything
(defn make-request-line [method path version]
  ['request-line method path version])


(defn request-line->str [request-line]
  (let [[_ method path version] request-line]
    (+ (method->str method) " " path " " (version->str version) *newline*)))


(defn parse-header-line [s]
  (let [[header value] (.split s ": " 1)]
    [(parse-header header) (.strip value)]))

; TODO: check what header can be
(defn make-header-line [header value]
  ['header-line header value])


(defn header-line->str [header-line]
  (let [[_ header value] header-line]
    (+ header ": " value *newline*)))


(defn make-request [request-line header-line]
    ['request request-line header-line])


(defn request->str [request]
  (let [[_ request-line header-line] request]
    (+ (request-line->str request-line) (header-line->str header-line) *newline*)))


(defn request->utf8 [r]
  (utf8 (request->str r)))


(defn send-request [s r] ; socket request
    (.send s (request->utf8 r)))


(defn split-status-line [s]
  (.split s " " 2))


(defn parse-header [s]
  (-> s .strip .casefold make-symbol))

(defn parse-status-line [s]
  (let [[version status explanation] (split-status-line s)]
    [(parse-version version) (int status) (parse-header explanation)]))


(defn make-status-line [version status explanation]
  ['status-line version status explanation])


(defn response->lines [r]
  (loop [[acc []]]
    (match (.readline r)
      "\r\n" (reverse acc)
      _ :as line (recur (cons line acc)))))


(defn parse-lines [xs]
  (loop [[xs (list (rest xs))] [acc []]]
    (match xs
      [] (dict acc)
      [x #* xs] (recur xs (cons (parse-header-line x) acc)))))

; TODO
(defmacro with-socket []
  '())

; TODO: How do you implement sockets?
(defn url-request [url]
  (let [[_ scheme host path] url
        s (make-socket socket.AF_INET socket.SOCK_STREAM socket.IPPROTO_TCP)
        r (make-request (make-request-line 'get path (make-version 1 0)) (make-header-line "Host" host))]
    (.connect s #(host *http-port*))
    (send-request s r)
    (let [response (.makefile s "r" :encoding "utf8" :newline *newline*)
          hs (-> response response->lines parse-lines)
          content (.read response)]
      (print hs)
    ;   (assert (not (in 'transfer-encoding hs))) ; TODO
    ;   (assert (not (in 'content-length hs))) ; TODO
      (.close s)
      content)))


(setv url-string "http://example.org/index.html")
(setv url (make-url #* (parse-url url-string)))
