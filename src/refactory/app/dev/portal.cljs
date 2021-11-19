(ns refactory.app.dev.portal
  (:require [portal.web]))


(add-tap #'portal.web/submit)
(portal.web/open)
