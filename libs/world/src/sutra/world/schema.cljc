(ns sutra.world.schema
  (:require [sutra.types.entities :as ents]))

(def Section (into [:enum] ents/sections))

(def Node [:tuple Section :any])

(def FrameNode [:tuple [:= :frame] :keyword])

(def Change
  [:or [:tuple Node :keyword :any :any] [:tuple FrameNode [:= :value] :any :any]])

(def Delta [:vector Change])

(def World [:map-of :keyword :any])

(def IndexRow [:map [:id :keyword] [:facts fn?]])

(def ViewRow
  [:map [:id :keyword] [:reads [:or [:set Section] [:= :all]]] [:init fn?] [:step fn?]])

(def GateRow
  [:map
   [:id :keyword]
   [:rule :qualified-keyword]
   [:gate {:optional true} :keyword]
   [:scope [:enum :local :global]]
   [:why {:optional true} :string]
   [:collapse {:optional true} :boolean]
   [:lint {:optional true} :boolean]
   [:keys fn?]
   [:all fn?]
   [:check fn?]])

(def Db
  [:map
   [:ents World]
   [:idx [:map-of :keyword [:map-of :any [:set Node]]]]
   [:views [:map-of :keyword :any]]
   [:view-rows [:vector ViewRow]]
   [:delta Delta]])
