(ns refactory.app.util)


(defmacro forall
  "Convenience macro for (doall (for ...))"
  [& args]
  `(doall (for ~@args)))


(defmacro for<>
  "Like forall, but returns a React fragment instead of a seq.

  This makes Reagent happy when you want to generate a list without :key
  metadata."
  [& args]
  `(into [:<>] (for ~@args)))
