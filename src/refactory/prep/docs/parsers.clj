(ns refactory.prep.docs.parsers
  "kern parser combinators for parsing complex embedded values in Docs.json."
  (:require [blancas.kern.core :as k :refer :all])
  (:import [clojure.lang MapEntry]))


(def period (sym* \.))
(def comma (sym* \,))
(def underscore (sym* \_))
(def hyphen (sym* \-))


(def identifier
  (<+> letter (many (<|> alpha-num underscore hyphen))))


(defn parens
  [inner]
  (between (sym* \() (sym* \)) inner))


(defn list-of
  [item-parser]
  (parens (sep-by comma item-parser)))


(def dict-entry
  (<$> (fn [[k _ v]] (MapEntry. k v))
    (<*> identifier
         (skip (sym* \=))
         (<+> (many (none-of* ",)"))))))


(def icon
  (>> (token* "Texture2D")
      white-space
      (sym* \/)
      (<$>
        #(str % ".png")
        (<+> (many-till any-char period)))))


(def dict
  (<$>
    #(into {} %)
    (parens (sep-by comma dict-entry))))


(def class-path-name
  (>> (many-till any-char period)
      identifier))


(def generated-class-name
  (>> (token* "BlueprintGeneratedClass")
      (between (token* "'\"")
               (token* "\"'")
               class-path-name)))


(def class-name
  (<|> generated-class-name class-path-name))


(comment
  (k/parse identifier "BP_EquipmentDescriptorBeacon_C")

  (k/parse class-name "/Game/FactoryGame/Resource/Equipment/Beacon/BP_EquipmentDescriptorBeacon.BP_EquipmentDescriptorBeacon_C")
  (k/parse class-name "BlueprintGeneratedClass'\"/Game/FactoryGame/Resource/Equipment/Beacon/BP_EquipmentDescriptorBeacon.BP_EquipmentDescriptorBeacon_C\"'")

  (k/parse (list-of class-name) "(/Game/FactoryGame/Equipment/BuildGun/BP_BuildGun.BP_BuildGun_C)")

  (k/parse dict-entry "ItemClass=BlueprintGeneratedClass'\"/Game/FactoryGame/Resource/Equipment/Beacon/BP_EquipmentDescriptorBeacon.BP_EquipmentDescriptorBeacon_C\"'")
  (k/parse dict "(a=1,b=2)")
  (k/parse dict "(ItemClass=BlueprintGeneratedClass'\"/Game/FactoryGame/Resource/Equipment/Beacon/BP_EquipmentDescriptorBeacon.BP_EquipmentDescriptorBeacon_C\"',Amount=1)")
  (k/parse (list-of dict)
           "((ItemClass=BlueprintGeneratedClass'\"/Game/FactoryGame/Resource/Equipment/Beacon/BP_EquipmentDescriptorBeacon.BP_EquipmentDescriptorBeacon_C\"',Amount=1),(ItemClass=BlueprintGeneratedClass'\"/Game/FactoryGame/Resource/Parts/SteelPipe/Desc_SteelPipe.Desc_SteelPipe_C\"',Amount=10),(ItemClass=BlueprintGeneratedClass'\"/Game/FactoryGame/Resource/Parts/GunPowder/Desc_Gunpowder.Desc_Gunpowder_C\"',Amount=10),(ItemClass=BlueprintGeneratedClass'\"/Game/FactoryGame/Resource/Parts/Rubber/Desc_Rubber.Desc_Rubber_C\"',Amount=10))"))
