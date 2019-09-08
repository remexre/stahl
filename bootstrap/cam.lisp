(in-package #:bootstrap)

(defmethod expr-to-cam ((expr expr-app))
  (error "TODO expr-to-cam of app"))

(defmethod expr-to-cam ((expr expr-lam))
  (error "TODO expr-to-cam of lam"))

(defmethod expr-to-cam ((expr expr-lit-string))
  (error "TODO expr-to-cam of str"))

(defmethod expr-to-cam ((expr expr-pi))
  (error "TODO expr-to-cam of pi"))

(defmethod expr-to-cam ((expr expr-type-of-types))
  (error "TODO expr-to-cam of ty"))

(defmethod expr-to-cam ((expr expr-var))
  (error "TODO expr-to-cam of var"))
