(ns sutra.access.api
  (:require [sutra.access.authority :as authority]
            [sutra.access.federation :as federation]
            [sutra.access.oauth :as oauth]
            [sutra.access.oidc :as oidc]
            [sutra.access.principals :as pr]
            [sutra.access.requests :as requests]
            [sutra.access.sessions :as sessions]
            [sutra.access.throttle :as throttle]))

(set! *warn-on-reflection* true)

(defn granted-resources [auth schema section] (authority/granted-resources auth schema section))

(defn granted? [auth schema a] (authority/granted? auth schema a))

(defn of-principal [store branch username] (authority/of-principal store branch username))

(defn owns [store branch who] (authority/owns store branch who))

(defn approver [store branch who] (authority/approver store branch who))

(defn grant-key [g] (oauth/grant-key g))

(defn own-service-user? [client] (oauth/own-service-user? client))

(def oidc-scopes oidc/oidc-scopes)

(defn openid? [scope] (oidc/openid? scope))

(defn oidc-issuer [req] (oidc/issuer req))

(defn id-token [store opts] (oidc/id-token store opts))

(defn ensure-signing-key! [store] (oidc/ensure-signing-key! store))

(defn authenticated-at [store token] (sessions/authenticated-at store token))

(defn wire-params [req] (oidc/wire-params req))

(defn client-credentials [req params] (oidc/client-credentials req params))

(defn fetch [store username] (pr/fetch store username))

(defn service-user? [p] (pr/service-user? p))

(defn problems [request] (requests/problems request))

(defn tasks [request] (requests/tasks request))

(defn mint-agent-token! [store p agent routes] (sessions/mint-agent-token! store p agent routes))

(defn end-session! [store token] (sessions/end! store token))

(defn group-directory [store] (authority/group-directory store))

(defn group-names [store] (authority/group-names store))

(defn user-emails [store usernames] (authority/user-emails store usernames))

(defn username-of [store principal] (authority/username-of store principal))

(defn throttled [req attempt fields f] (throttle/throttled req attempt fields f))

(defn attempted! [req attempt ok? fields] (throttle/attempted! req attempt ok? fields))

(def live-branch authority/live-branch)

(defn memberships [snapshot schema id] (authority/memberships snapshot schema id))

(defn principal-rows [store] (pr/all-rows store))

(defn usernames-by-object-id [store] (authority/usernames-by-object-id store))

(defn instance-identity [store] (federation/identity-of store))

(defn instance-trusts [store] (federation/trusts store))

(defn sign-payload [store payload] (federation/sign-payload store payload))

(defn signed-by [store payload signature] (federation/signed-by store payload signature))

(defn signature-valid? [public-key payload sig]
  (federation/signature-valid? public-key payload sig))

(defn trust-by-fingerprint [store fp] (federation/trust-by-fingerprint store fp))

(defn key-fingerprint [public-key] (federation/fingerprint public-key))
