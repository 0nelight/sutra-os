(ns sutra.types.paths.peering
  (:require [sutra.types.paths.rules :refer [reads writes]]))

(defn- admin [url reason methods]
  {:url url :audit :access :methods methods :service :platform :auth :admin :reason reason})

(def ^:private m-space
  {:member (str "a space's peer connections: who can manage the space manages them"
                ", checked in the handler (sutra.peering.http/manages?)")})

(defn- managed
  [url reason methods]
  {:url url :audit :access :service :platform :reason reason
   :methods (update-vals methods #(assoc % :requires m-space))})

(def endpoints
  {:peering-connections (managed "/api/peering/connections"
                                 "the peer connections of the spaces the caller manages"
                                 {:get {:effects reads} :post {:effects writes}})
   :peering-accept      (managed "/api/peering/accept" "taking in a peer request for a space"
                                 {:post {:effects writes}})
   :peering-connection  (managed "/api/peering/connections/:connection"
                                 "one peer connection: settings, mappings, health"
                                 {:get {:effects reads} :delete {:effects writes}})
   :peering-activate    (managed "/api/peering/connections/:connection/activate"
                                 "activating a peer connection" {:post {:effects writes}})
   :peering-status      (managed "/api/peering/connections/:connection/status"
                                 "disabling or enabling a peer connection"
                                 {:post {:effects writes}})
   :peering-ceiling     (managed "/api/peering/connections/:connection/ceiling"
                                 "a peer connection's security ceiling, changed by both sides"
                                 {:post {:effects writes}})
   :peering-ingress     (managed "/api/peering/connections/:connection/ingress"
                                 "the addresses a peer connection's data comes from"
                                 {:post {:effects writes}})
   :peering-markings    (managed "/api/peering/connections/:connection/markings"
                                 "which local marking a peer's marking stands for"
                                 {:post {:effects writes}})
   :peering-mappings    (managed "/api/peering/connections/:connection/mappings"
                                 "the object types a peer connection peers"
                                 {:get {:effects reads} :post {:effects writes}})
   :peering-mapping     (managed "/api/peering/connections/:connection/mappings/:type"
                                 "an object type no longer peered" {:delete {:effects writes}})
   :peering-media       (managed "/api/peering/connections/:connection/media"
                                 "the media sets a peer connection peers"
                                 {:get {:effects reads} :post {:effects writes}})
   :peering-media-set   (managed "/api/peering/connections/:connection/media/:set"
                                 "a media set no longer peered" {:delete {:effects writes}})
   :peering-shares      (managed "/api/peering/connections/:connection/shares"
                                 "the datasets a peer connection shares for the other side to sync"
                                 {:get {:effects reads} :post {:effects writes}})
   :peering-share       (managed "/api/peering/connections/:connection/shares/:dataset"
                                 "a dataset no longer shared" {:delete {:effects writes}})
   :peering-jobs        (managed "/api/peering/connections/:connection/jobs"
                                 "a peer connection's jobs and health" {:get {:effects reads}})
   :peering-run         (admin "/api/peering/run" "one round of every peer connection now"
                               {:post {:effects writes}})
   :peering-profiles    (managed "/api/peering/profiles"
                                 "the peer profiles of the spaces the caller manages"
                                 {:get {:effects reads} :post {:effects writes}})
   :peering-profile-install (managed "/api/peering/profile-install"
                                     "a counterpart profile installed for a space here"
                                     {:post {:effects writes}})
   :peering-profile     (managed "/api/peering/profiles/:profile"
                                 "one peer profile: its targets, connections, counterpart"
                                 {:get {:effects reads} :delete {:effects writes}})
   :peering-mesh        (admin "/api/peering/mesh"
                               "the instances of the mesh and their labels"
                               {:get {:effects reads} :post {:effects writes}})
   :peering-peer        {:url "/api/peering/peer"
                         :methods {:post {:effects writes}}
                         :service :platform
                         :auth :public
                         :reason (str "the other instance of a peer connection; the payload's"
                                      " signature names it (sutra.peering.wire)")}})
