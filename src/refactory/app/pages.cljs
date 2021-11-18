(ns refactory.app.pages)


(defmulti config
  "Configuration for each page. Should return a map with any of our supported
  keys:

    :enter - A re-frame event to dispatch before switching to the page.
    :leave - A re-frame event to dispatch before switching away from a page.
  "
  identity)


(defmethod config :default
  [_]
  {})
