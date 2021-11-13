(ns refactory.app.util)


(defmacro forall
  "Convenience macro for (doall (for ...))"
  [& args]
  `(doall (for ~@args)))
