(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u200))
(define-constant ERR_PROPOSAL_NOT_FOUND (err u201))
(define-constant ERR_PROPOSAL_ALREADY_EXECUTED (err u202))
(define-constant ERR_PROPOSAL_NOT_READY (err u203))
(define-constant ERR_PROPOSAL_EXPIRED (err u204))
(define-constant ERR_INVALID_TIMELOCK (err u205))
(define-constant ERR_INVALID_ACTION (err u206))

(define-constant MIN_TIMELOCK_DELAY u144)
(define-constant MAX_TIMELOCK_DELAY u4320)
(define-constant PROPOSAL_LIFETIME u8640)

(define-data-var proposal-nonce uint u0)
(define-data-var default-timelock-delay uint u288)

(define-map admins principal bool)
(define-map proposals uint {
  proposer: principal,
  target: principal,
  action: (string-ascii 50),
  created-at: uint,
  execute-after: uint,
  executed: bool,
  description: (string-ascii 200)
})

(map-set admins CONTRACT_OWNER true)

(define-read-only (is-admin (user principal))
  (default-to false (map-get? admins user))
)

(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals proposal-id)
)

(define-read-only (get-current-nonce)
  (var-get proposal-nonce)
)

(define-read-only (get-default-timelock-delay)
  (var-get default-timelock-delay)
)

(define-read-only (can-execute-proposal (proposal-id uint))
  (match (map-get? proposals proposal-id)
    proposal (and
      (not (get executed proposal))
      (>= stacks-block-height (get execute-after proposal))
      (<= stacks-block-height (+ (get created-at proposal) PROPOSAL_LIFETIME))
    )
    false
  )
)

(define-read-only (get-proposals-by-status (executed bool))
  (let ((current-nonce (var-get proposal-nonce)))
    (filter-proposals u0 current-nonce executed (list))
  )
)

(define-private (filter-proposals (counter uint) (max-counter uint) (target-executed bool) (acc (list 50 uint)))
  (if (>= counter max-counter)
    acc
    (match (map-get? proposals counter)
      proposal (if (is-eq (get executed proposal) target-executed)
        (filter-proposals (+ counter u1) max-counter target-executed (unwrap-panic (as-max-len? (append acc counter) u50)))
        (filter-proposals (+ counter u1) max-counter target-executed acc)
      )
      (filter-proposals (+ counter u1) max-counter target-executed acc)
    )
  )
)

(define-public (add-admin (new-admin principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (map-set admins new-admin true)
    (ok true)
  )
)

(define-public (remove-admin (admin-to-remove principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (not (is-eq admin-to-remove CONTRACT_OWNER)) ERR_UNAUTHORIZED)
    (map-delete admins admin-to-remove)
    (ok true)
  )
)

(define-public (set-default-timelock-delay (new-delay uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (and (>= new-delay MIN_TIMELOCK_DELAY) (<= new-delay MAX_TIMELOCK_DELAY)) ERR_INVALID_TIMELOCK)
    (var-set default-timelock-delay new-delay)
    (ok true)
  )
)

(define-public (create-proposal (target principal) (action (string-ascii 50)) (description (string-ascii 200)) (custom-delay (optional uint)))
  (let (
    (current-nonce (var-get proposal-nonce))
    (timelock-delay (default-to (var-get default-timelock-delay) custom-delay))
  )
    (begin
      (asserts! (is-admin tx-sender) ERR_UNAUTHORIZED)
      (asserts! (and (>= timelock-delay MIN_TIMELOCK_DELAY) (<= timelock-delay MAX_TIMELOCK_DELAY)) ERR_INVALID_TIMELOCK)
      (map-set proposals current-nonce {
        proposer: tx-sender,
        target: target,
        action: action,
        created-at: stacks-block-height,
        execute-after: (+ stacks-block-height timelock-delay),
        executed: false,
        description: description
      })
      (var-set proposal-nonce (+ current-nonce u1))
      (ok current-nonce)
    )
  )
)

(define-public (execute-proposal (proposal-id uint))
  (match (map-get? proposals proposal-id)
    proposal (begin
      (asserts! (is-admin tx-sender) ERR_UNAUTHORIZED)
      (asserts! (not (get executed proposal)) ERR_PROPOSAL_ALREADY_EXECUTED)
      (asserts! (>= stacks-block-height (get execute-after proposal)) ERR_PROPOSAL_NOT_READY)
      (asserts! (<= stacks-block-height (+ (get created-at proposal) PROPOSAL_LIFETIME)) ERR_PROPOSAL_EXPIRED)
      (map-set proposals proposal-id (merge proposal {executed: true}))
      (ok {
        proposal-id: proposal-id,
        target: (get target proposal),
        action: (get action proposal),
        executed-by: tx-sender,
        executed-at: stacks-block-height
      })
    )
    ERR_PROPOSAL_NOT_FOUND
  )
)

(define-public (cancel-proposal (proposal-id uint))
  (match (map-get? proposals proposal-id)
    proposal (begin
      (asserts! (or (is-eq tx-sender (get proposer proposal)) (is-eq tx-sender CONTRACT_OWNER)) ERR_UNAUTHORIZED)
      (asserts! (not (get executed proposal)) ERR_PROPOSAL_ALREADY_EXECUTED)
      (map-delete proposals proposal-id)
      (ok true)
    )
    ERR_PROPOSAL_NOT_FOUND
  )
)

(define-read-only (get-pending-proposals)
  (get-proposals-by-status false)
)

(define-read-only (get-executed-proposals)
  (get-proposals-by-status true)
)

(define-read-only (get-proposal-status (proposal-id uint))
  (match (map-get? proposals proposal-id)
    proposal (if (get executed proposal)
      "executed"
      (if (< stacks-block-height (get execute-after proposal))
        "pending"
        (if (<= stacks-block-height (+ (get created-at proposal) PROPOSAL_LIFETIME))
          "ready"
          "expired"
        )
      )
    )
    "not-found"
  )
)

(define-read-only (get-contract-info)
  {
    owner: CONTRACT_OWNER,
    total-proposals: (var-get proposal-nonce),
    default-timelock-delay: (var-get default-timelock-delay),
    min-timelock-delay: MIN_TIMELOCK_DELAY,
    max-timelock-delay: MAX_TIMELOCK_DELAY,
    proposal-lifetime: PROPOSAL_LIFETIME
  }
)
