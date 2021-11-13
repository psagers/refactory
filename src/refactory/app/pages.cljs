(ns refactory.app.pages)


(defmulti config identity)


(defmethod config :default
  [_]
  {})
