(require
 hyrule [-> loop defmain])

(import
 socket
 ssl
 hy
 hyrule [recur]
 toolz [first drop count cons assoc])


(defmacro def [#* xs]
  `(setv ~@xs))

(defmacro assert-in [x xs]
  `(assert (in ~x ~xs)))

(def *newline* "\r\n")
(def *http-port* 80)
(def *https-port* 443)


(defn rest [xs]
  (drop 1 xs))


(defn second [xs]
  (get xs 1))

(defn reverse [xs]
  (list (reversed (list xs))))

(setv make-symbol hy.models.Symbol)

(defn list->str [xs]
  (.join "" xs))

; TODO: write my own split
(defn split [s sep]
  [])

(defn newline? [s]
  (= s *newline*))


(defn split-scheme [s]
  (.split s "://" 1))

(defn http? [s]
  (or (= s "http") (= s 'http)))


(defn https? [s]
  (or (= s "https") (= s 'https)))


(defn scheme->port [scheme]
  (cond
    (http? scheme) *http-port*
    (https? scheme) *https-port*))


(defn split-host [s]
  (.split
   (+ s (if (in "/" s) "" "/"))
   "/"
   1))


(defn has-port? [host]
  (in ":" host))


(defn split-port [host]
  (.split host ":" 1))


(defn parse-url [s]
  (let [[scheme rest] (split-scheme s)
        [host path] (split-host rest)
        [host port] (if (has-port? host) (split-port host) [host (scheme->port scheme)])]
    [scheme host (+ "/" path) (int port)]))


(defn make-url [scheme host path port]
  (assert-in scheme ["http" "https"])
  ['url scheme host path port])


(defn str->url [s]
  (make-url #* (parse-url s)))


(def make-socket socket.socket)


(defn make-secure-socket [family type proto host]
  (let [ctx (ssl.create_default_context)]
    (.wrap_socket ctx (make-socket family type proto) :server_hostname host)))


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

(defn make-request-line-get [path]
  (make-request-line 'get path (make-version 1 0)))


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

(defn make-get-request [path host]
  (make-request (make-request-line-get path) (make-header-line "Host" host)))


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


; TODO: return stream
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

; TODO
(defmacro match* []
  '())

(defmacro when-in [x xs #* body]
  `(when (in ~x ~xs) ~@body))


(defmacro print-when [test #* xs]
  `(when ~test (print ~@xs)))


; TODO: add response?
; TODO: support for more addresses
(defmacro with-socket [ss a scheme #* body]
  (let [[s family type proto] ss
        [host port] a]
    `(try
       (let [sock# (if (https? ~scheme)
                     (make-secure-socket ~family ~type ~proto ~host) 
                     (make-socket ~family ~type ~proto))]
         (with [~s sock#]
           (.connect ~s ~a)
           ~@body))
       (finally
         (.close ~s)))))

; TODO: How do you implement sockets?
; TODO: simplify
(defn url-request [url]
  (def [_ scheme host path port] url)
  (with-socket [s socket.AF_INET socket.SOCK_STREAM socket.IPPROTO_TCP] #(host port) scheme
    (send-request s (make-get-request path host))
    (let [response (.makefile s "r" :encoding "utf8" :newline *newline*)
          hs (-> response response->lines parse-lines)
          content (.read response)]
      ; (print-when (in 'transfer-encoding hs) "warning: transfer-encoding") ; TODO
      ; (print-when (in 'content-length hs) "warning: content-length")
      content)))

(defn parse-text [body]
  (loop [[xs (list body)] [tag? False] [acc []] [accs []]]
    (match xs
      [] (reverse accs)
      ["<" #* xs]  (recur xs True [] (if acc (cons (.join "" (reverse acc)) accs) accs))
      [">" #* xs] (recur xs False acc accs)
      [x #* xs] :if (not tag?) (recur xs tag? (cons x acc) accs)
      [x #* xs] (recur xs tag? acc accs))))


(defn request-text [url]
  (-> url url-request parse-text))


(defn show [body]
  (-> body parse-text list->str print))


(defn load [url]
  (show (url-request url)))


(defmain [browser url]
  (-> url str->url load))


(def url-string "http://example.org/index.html")
(def url-string-secure "https://example.org/index.html")
(def url (str->url url-string))
(def url-secure (str->url url-string-secure))
