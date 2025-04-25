;; TaleForge: Decentralized Story Creation Platform

;; Constants
(define-constant contract-admin tx-sender)
(define-constant error-admin-only (err u100))
(define-constant error-not-found (err u101))
(define-constant error-already-exists (err u102))
(define-constant error-voting-ended (err u103))
(define-constant error-invalid-choice (err u104))

;; Data Maps
(define-map narratives
  { narrative-id: uint }
  { headline: (string-ascii 100), latest-segment: uint, is-finished: bool }
)

(define-map segments
  { narrative-id: uint, segment-id: uint }
  { text: (string-utf8 1000), creator: principal }
)

(define-map narrative-branches
  { narrative-id: uint, branch-id: uint }
  { paths: (list 2 (string-ascii 100)), tallies: (list 2 uint), is-active: bool }
)

(define-map narrative-participants { narrative-id: uint, creator: principal } bool)

;; NFT Definitions
(define-non-fungible-token tale-token uint)

;; Variables
(define-data-var next-narrative-id uint u0)
(define-data-var next-branch-id uint u0)

;; Private Functions
(define-private (is-admin)
  (is-eq tx-sender contract-admin)
)

;; Public Functions
(define-public (create-narrative (headline (string-ascii 100)))
  (let
    (
      (new-narrative-id (+ (var-get next-narrative-id) u1))
    )
    (try! (nft-mint? tale-token new-narrative-id tx-sender))
    (map-set narratives { narrative-id: new-narrative-id } { headline: headline, latest-segment: u0, is-finished: false })
    (var-set next-narrative-id new-narrative-id)
    (ok new-narrative-id)
  )
)


;; Read-only Functions
(define-read-only (get-narrative (narrative-id uint))
  (map-get? narratives { narrative-id: narrative-id })
)

(define-read-only (get-segment (narrative-id uint) (segment-id uint))
  (map-get? segments { narrative-id: narrative-id, segment-id: segment-id })
)

(define-read-only (get-branch (narrative-id uint) (branch-id uint))
  (map-get? narrative-branches { narrative-id: narrative-id, branch-id: branch-id })
)

(define-read-only (is-narrative-participant (narrative-id uint) (creator principal))
  (default-to false (map-get? narrative-participants { narrative-id: narrative-id, creator: creator }))
)