;; TaleForge: Decentralized Story Creation Platform

;; Constants
(define-constant contract-admin tx-sender)
(define-constant error-admin-only (err u100))
(define-constant error-not-found (err u101))
(define-constant error-already-exists (err u102))
(define-constant error-voting-ended (err u103))
(define-constant error-invalid-choice (err u104))
(define-constant error-invalid-input (err u105))

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

(define-private (validate-narrative-id (narrative-id uint))
  (match (map-get? narratives { narrative-id: narrative-id })
    narrative true
    false
  )
)

(define-private (validate-branch-id (narrative-id uint) (branch-id uint))
  (match (map-get? narrative-branches { narrative-id: narrative-id, branch-id: branch-id })
    branch true
    false
  )
)

;; Public Functions
(define-public (create-narrative (headline (string-ascii 100)))
  (let
    (
      (new-narrative-id (+ (var-get next-narrative-id) u1))
    )
    ;; No validation needed for headline as string-ascii ensures valid data
    (try! (nft-mint? tale-token new-narrative-id tx-sender))
    (map-set narratives { narrative-id: new-narrative-id } { headline: headline, latest-segment: u0, is-finished: false })
    (var-set next-narrative-id new-narrative-id)
    (ok new-narrative-id)
  )
)

(define-public (add-segment (narrative-id uint) (text (string-utf8 1000)))
  (let
    (
      (narrative-exists (validate-narrative-id narrative-id))
    )
    ;; Validate that the narrative exists
    (asserts! narrative-exists (err error-not-found))
    
    (let
      (
        (narrative (unwrap! (map-get? narratives { narrative-id: narrative-id }) (err error-not-found)))
        (new-segment-id (+ (get latest-segment narrative) u1))
      )
      (asserts! (not (get is-finished narrative)) (err error-voting-ended))
      ;; No validation needed for text as string-utf8 ensures valid data
      (map-set segments { narrative-id: narrative-id, segment-id: new-segment-id } { text: text, creator: tx-sender })
      (map-set narratives { narrative-id: narrative-id }
        (merge narrative { latest-segment: new-segment-id }))
      (map-set narrative-participants { narrative-id: narrative-id, creator: tx-sender } true)
      (ok new-segment-id)
    )
  )
)

(define-public (create-branch (narrative-id uint) (path-a (string-ascii 100)) (path-b (string-ascii 100)))
  (let
    (
      (narrative-exists (validate-narrative-id narrative-id))
    )
    ;; Validate that the narrative exists
    (asserts! narrative-exists (err error-not-found))
    
    (let
      (
        (narrative (unwrap! (map-get? narratives { narrative-id: narrative-id }) (err error-not-found)))
        (new-branch-id (+ (var-get next-branch-id) u1))
      )
      (asserts! (not (get is-finished narrative)) (err error-voting-ended))
      ;; No validation needed for path-a and path-b as string-ascii ensures valid data
      (map-set narrative-branches { narrative-id: narrative-id, branch-id: new-branch-id }
        { paths: (list path-a path-b), tallies: (list u0 u0), is-active: true })
      (var-set next-branch-id new-branch-id)
      (ok new-branch-id)
    )
  )
)

(define-public (vote-on-branch (narrative-id uint) (branch-id uint) (choice uint))
  (let
    (
      (branch-exists (validate-branch-id narrative-id branch-id))
    )
    ;; Validate that the branch exists
    (asserts! branch-exists (err error-not-found))
    
    (let
      (
        (branch (unwrap! (map-get? narrative-branches { narrative-id: narrative-id, branch-id: branch-id }) (err error-not-found)))
        (current-tallies (get tallies branch))
      )
      (asserts! (get is-active branch) (err error-voting-ended))
      (asserts! (or (is-eq choice u0) (is-eq choice u1)) (err error-invalid-choice))
      (ok (map-set narrative-branches { narrative-id: narrative-id, branch-id: branch-id }
        (merge branch { tallies: (list
          (if (is-eq choice u0) (+ (default-to u0 (element-at? current-tallies u0)) u1) (default-to u0 (element-at? current-tallies u0)))
          (if (is-eq choice u1) (+ (default-to u0 (element-at? current-tallies u1)) u1) (default-to u0 (element-at? current-tallies u1)))
        )})))
    )
  )
)

(define-public (close-branch-voting (narrative-id uint) (branch-id uint))
  (let
    (
      (branch-exists (validate-branch-id narrative-id branch-id))
    )
    ;; Validate that the branch exists
    (asserts! branch-exists (err error-not-found))
    (asserts! (is-admin) (err error-admin-only))
    
    (let
      (
        (branch (unwrap! (map-get? narrative-branches { narrative-id: narrative-id, branch-id: branch-id }) (err error-not-found)))
      )
      (ok (map-set narrative-branches { narrative-id: narrative-id, branch-id: branch-id }
        (merge branch { is-active: false })))
    )
  )
)

(define-public (finish-narrative (narrative-id uint))
  (let
    (
      (narrative-exists (validate-narrative-id narrative-id))
    )
    ;; Validate that the narrative exists
    (asserts! narrative-exists (err error-not-found))
    (asserts! (is-admin) (err error-admin-only))
    
    (let
      (
        (narrative (unwrap! (map-get? narratives { narrative-id: narrative-id }) (err error-not-found)))
      )
      (ok (map-set narratives { narrative-id: narrative-id }
        (merge narrative { is-finished: true })))
    )
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