(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u300))
(define-constant ERR_TRANSACTION_NOT_FOUND (err u301))
(define-constant ERR_ALREADY_SIGNED (err u302))
(define-constant ERR_TRANSACTION_EXECUTED (err u303))
(define-constant ERR_INSUFFICIENT_SIGNATURES (err u304))
(define-constant ERR_TRANSACTION_EXPIRED (err u305))
(define-constant ERR_INVALID_THRESHOLD (err u306))
(define-constant ERR_NOT_PENDING (err u307))

(define-constant MIN_SIGNATURE_THRESHOLD u2)
(define-constant MAX_SIGNATURE_THRESHOLD u10)
(define-constant TRANSACTION_LIFETIME u1440)

(define-data-var transaction-nonce uint u0)
(define-data-var signature-threshold uint u3)

(define-map admins principal bool)
(define-map pending-transactions uint {
  proposer: principal,
  target-function: (string-ascii 50),
  data: (string-ascii 200),
  created-at: uint,
  expires-at: uint,
  executed: bool,
  current-signatures: uint
})
(define-map transaction-signatures {transaction-id: uint, signer: principal} bool)
(define-map signature-records uint (list 10 principal))

(map-set admins CONTRACT_OWNER true)

(define-read-only (is-admin (user principal))
  (default-to false (map-get? admins user))
)

(define-read-only (get-signature-threshold)
  (var-get signature-threshold)
)

(define-read-only (get-transaction (transaction-id uint))
  (map-get? pending-transactions transaction-id)
)

(define-read-only (get-current-nonce)
  (var-get transaction-nonce)
)

(define-read-only (has-signed (transaction-id uint) (signer principal))
  (default-to false (map-get? transaction-signatures {transaction-id: transaction-id, signer: signer}))
)

(define-read-only (get-transaction-signers (transaction-id uint))
  (default-to (list) (map-get? signature-records transaction-id))
)

(define-read-only (can-execute-transaction (transaction-id uint))
  (match (map-get? pending-transactions transaction-id)
    transaction (and
      (not (get executed transaction))
      (>= (get current-signatures transaction) (var-get signature-threshold))
      (<= stacks-block-height (get expires-at transaction))
    )
    false
  )
)

(define-private (count-pending-transactions (counter uint) (max-counter uint) (acc uint))
  (if (>= counter max-counter)
    acc
    (match (map-get? pending-transactions counter)
      transaction (if (not (get executed transaction))
        (count-pending-transactions (+ counter u1) max-counter (+ acc u1))
        (count-pending-transactions (+ counter u1) max-counter acc)
      )
      (count-pending-transactions (+ counter u1) max-counter acc)
    )
  )
)

(define-read-only (get-pending-transactions-count)
  (let ((current-nonce (var-get transaction-nonce)))
    (count-pending-transactions u0 current-nonce u0)
  )
)

(define-public (set-signature-threshold (new-threshold uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (and (>= new-threshold MIN_SIGNATURE_THRESHOLD) (<= new-threshold MAX_SIGNATURE_THRESHOLD)) ERR_INVALID_THRESHOLD)
    (var-set signature-threshold new-threshold)
    (ok true)
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

(define-public (create-transaction (target-function (string-ascii 50)) (data (string-ascii 200)))
  (let ((current-nonce (var-get transaction-nonce)))
    (begin
      (asserts! (is-admin tx-sender) ERR_UNAUTHORIZED)
      (map-set pending-transactions current-nonce {
        proposer: tx-sender,
        target-function: target-function,
        data: data,
        created-at: stacks-block-height,
        expires-at: (+ stacks-block-height TRANSACTION_LIFETIME),
        executed: false,
        current-signatures: u1
      })
      (map-set transaction-signatures {transaction-id: current-nonce, signer: tx-sender} true)
      (map-set signature-records current-nonce (list tx-sender))
      (var-set transaction-nonce (+ current-nonce u1))
      (ok current-nonce)
    )
  )
)

(define-public (sign-transaction (transaction-id uint))
  (match (map-get? pending-transactions transaction-id)
    transaction (begin
      (asserts! (is-admin tx-sender) ERR_UNAUTHORIZED)
      (asserts! (not (get executed transaction)) ERR_TRANSACTION_EXECUTED)
      (asserts! (<= stacks-block-height (get expires-at transaction)) ERR_TRANSACTION_EXPIRED)
      (asserts! (not (has-signed transaction-id tx-sender)) ERR_ALREADY_SIGNED)
      (let (
        (current-signers (get-transaction-signers transaction-id))
        (new-signature-count (+ (get current-signatures transaction) u1))
      )
        (map-set transaction-signatures {transaction-id: transaction-id, signer: tx-sender} true)
        (map-set signature-records transaction-id (unwrap-panic (as-max-len? (append current-signers tx-sender) u10)))
        (map-set pending-transactions transaction-id (merge transaction {current-signatures: new-signature-count}))
        (ok new-signature-count)
      )
    )
    ERR_TRANSACTION_NOT_FOUND
  )
)

(define-public (execute-transaction (transaction-id uint))
  (match (map-get? pending-transactions transaction-id)
    transaction (begin
      (asserts! (is-admin tx-sender) ERR_UNAUTHORIZED)
      (asserts! (not (get executed transaction)) ERR_TRANSACTION_EXECUTED)
      (asserts! (<= stacks-block-height (get expires-at transaction)) ERR_TRANSACTION_EXPIRED)
      (asserts! (>= (get current-signatures transaction) (var-get signature-threshold)) ERR_INSUFFICIENT_SIGNATURES)
      (map-set pending-transactions transaction-id (merge transaction {executed: true}))
      (ok {
        transaction-id: transaction-id,
        target-function: (get target-function transaction),
        data: (get data transaction),
        executed-by: tx-sender,
        executed-at: stacks-block-height,
        final-signature-count: (get current-signatures transaction)
      })
    )
    ERR_TRANSACTION_NOT_FOUND
  )
)

(define-public (revoke-signature (transaction-id uint))
  (match (map-get? pending-transactions transaction-id)
    transaction (begin
      (asserts! (is-admin tx-sender) ERR_UNAUTHORIZED)
      (asserts! (not (get executed transaction)) ERR_TRANSACTION_EXECUTED)
      (asserts! (has-signed transaction-id tx-sender) ERR_NOT_PENDING)
      (let (
        (current-signers (get-transaction-signers transaction-id))
        (new-signature-count (- (get current-signatures transaction) u1))
        (filtered-signers (filter-signer-from-list current-signers tx-sender))
      )
        (map-delete transaction-signatures {transaction-id: transaction-id, signer: tx-sender})
        (map-set signature-records transaction-id filtered-signers)
        (map-set pending-transactions transaction-id (merge transaction {current-signatures: new-signature-count}))
        (ok new-signature-count)
      )
    )
    ERR_TRANSACTION_NOT_FOUND
  )
)

(define-private (filter-signer-from-list (signers (list 10 principal)) (signer-to-remove principal))
  (fold filter-signer signers (list))
)

(define-private (filter-signer (signer principal) (acc (list 10 principal)))
  (if (is-eq signer tx-sender)
    acc
    (unwrap-panic (as-max-len? (append acc signer) u10))
  )
)

(define-read-only (get-transaction-status (transaction-id uint))
  (match (map-get? pending-transactions transaction-id)
    transaction (if (get executed transaction)
      "executed"
      (if (> stacks-block-height (get expires-at transaction))
        "expired"
        (if (>= (get current-signatures transaction) (var-get signature-threshold))
          "ready-to-execute"
          "pending-signatures"
        )
      )
    )
    "not-found"
  )
)

(define-read-only (get-contract-info)
  {
    owner: CONTRACT_OWNER,
    signature-threshold: (var-get signature-threshold),
    total-transactions: (var-get transaction-nonce),
    pending-transactions: (get-pending-transactions-count),
    transaction-lifetime: TRANSACTION_LIFETIME,
    min-threshold: MIN_SIGNATURE_THRESHOLD,
    max-threshold: MAX_SIGNATURE_THRESHOLD
  }
)
