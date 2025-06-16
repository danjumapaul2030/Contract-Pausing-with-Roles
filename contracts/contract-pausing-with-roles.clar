(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_CONTRACT_PAUSED (err u101))
(define-constant ERR_ALREADY_PAUSED (err u102))
(define-constant ERR_NOT_PAUSED (err u103))
(define-constant ERR_ADMIN_EXISTS (err u104))
(define-constant ERR_NOT_ADMIN (err u105))
(define-constant ERR_CANNOT_REMOVE_OWNER (err u106))
(define-constant ERR_INSUFFICIENT_BALANCE (err u107))
(define-constant ERR_TRANSFER_FAILED (err u108))

(define-data-var contract-paused bool false)
(define-data-var total-deposits uint u0)

(define-map admins principal bool)
(define-map user-balances principal uint)
(define-map pause-history uint {admin: principal, action: (string-ascii 10), timestamp: uint})
(define-data-var pause-history-nonce uint u0)

(map-set admins CONTRACT_OWNER true)

(define-read-only (is-contract-paused)
  (var-get contract-paused)
)

(define-read-only (is-admin (user principal))
  (default-to false (map-get? admins user))
)

(define-read-only (get-user-balance (user principal))
  (default-to u0 (map-get? user-balances user))
)

(define-read-only (get-total-deposits)
  (var-get total-deposits)
)

(define-read-only (get-pause-history (id uint))
  (map-get? pause-history id)
)

(define-read-only (get-current-pause-nonce)
  (var-get pause-history-nonce)
)

(define-private (is-authorized-admin (user principal))
  (is-admin user)
)

(define-private (record-pause-action (action (string-ascii 10)))
  (let ((current-nonce (var-get pause-history-nonce)))
    (map-set pause-history current-nonce {
      admin: tx-sender,
      action: action,
      timestamp: stacks-block-height
    })
    (var-set pause-history-nonce (+ current-nonce u1))
  )
)

(define-public (add-admin (new-admin principal))
  (begin
    (asserts! (is-authorized-admin tx-sender) ERR_UNAUTHORIZED)
    (asserts! (not (is-admin new-admin)) ERR_ADMIN_EXISTS)
    (map-set admins new-admin true)
    (ok true)
  )
)

(define-public (remove-admin (admin-to-remove principal))
  (begin
    (asserts! (is-authorized-admin tx-sender) ERR_UNAUTHORIZED)
    (asserts! (not (is-eq admin-to-remove CONTRACT_OWNER)) ERR_CANNOT_REMOVE_OWNER)
    (asserts! (is-admin admin-to-remove) ERR_NOT_ADMIN)
    (map-delete admins admin-to-remove)
    (ok true)
  )
)

(define-public (pause-contract)
  (begin
    (asserts! (is-authorized-admin tx-sender) ERR_UNAUTHORIZED)
    (asserts! (not (var-get contract-paused)) ERR_ALREADY_PAUSED)
    (var-set contract-paused true)
    (record-pause-action "pause")
    (ok true)
  )
)

(define-public (unpause-contract)
  (begin
    (asserts! (is-authorized-admin tx-sender) ERR_UNAUTHORIZED)
    (asserts! (var-get contract-paused) ERR_NOT_PAUSED)
    (var-set contract-paused false)
    (record-pause-action "unpause")
    (ok true)
  )
)

(define-public (deposit (amount uint))
  (begin
    (asserts! (not (var-get contract-paused)) ERR_CONTRACT_PAUSED)
    (asserts! (> amount u0) ERR_INSUFFICIENT_BALANCE)
    (match (stx-transfer? amount tx-sender (as-contract tx-sender))
      success (begin
        (map-set user-balances tx-sender (+ (get-user-balance tx-sender) amount))
        (var-set total-deposits (+ (var-get total-deposits) amount))
        (ok amount)
      )
      error ERR_TRANSFER_FAILED
    )
  )
)

(define-public (withdraw (amount uint))
  (let ((user-balance (get-user-balance tx-sender)))
    (begin
      (asserts! (not (var-get contract-paused)) ERR_CONTRACT_PAUSED)
      (asserts! (>= user-balance amount) ERR_INSUFFICIENT_BALANCE)
      (asserts! (> amount u0) ERR_INSUFFICIENT_BALANCE)
      (match (as-contract (stx-transfer? amount tx-sender tx-sender))
        success (begin
          (map-set user-balances tx-sender (- user-balance amount))
          (var-set total-deposits (- (var-get total-deposits) amount))
          (ok amount)
        )
        error ERR_TRANSFER_FAILED
      )
    )
  )
)

(define-public (emergency-withdraw)
  (let ((user-balance (get-user-balance tx-sender)))
    (begin
      (asserts! (var-get contract-paused) ERR_NOT_PAUSED)
      (asserts! (> user-balance u0) ERR_INSUFFICIENT_BALANCE)
      (match (as-contract (stx-transfer? user-balance tx-sender tx-sender))
        success (begin
          (map-set user-balances tx-sender u0)
          (var-set total-deposits (- (var-get total-deposits) user-balance))
          (ok user-balance)
        )
        error ERR_TRANSFER_FAILED
      )
    )
  )
)

(define-public (transfer-to-user (recipient principal) (amount uint))
  (let ((sender-balance (get-user-balance tx-sender)))
    (begin
      (asserts! (not (var-get contract-paused)) ERR_CONTRACT_PAUSED)
      (asserts! (>= sender-balance amount) ERR_INSUFFICIENT_BALANCE)
      (asserts! (> amount u0) ERR_INSUFFICIENT_BALANCE)
      (map-set user-balances tx-sender (- sender-balance amount))
      (map-set user-balances recipient (+ (get-user-balance recipient) amount))
      (ok amount)
    )
  )
)

(define-public (batch-deposit (amounts (list 10 uint)))
  (begin
    (asserts! (not (var-get contract-paused)) ERR_CONTRACT_PAUSED)
    (ok (fold process-deposit amounts u0))
  )
)

(define-private (process-deposit (amount uint) (total uint))
  (match (stx-transfer? amount tx-sender (as-contract tx-sender))
    success (begin
      (map-set user-balances tx-sender (+ (get-user-balance tx-sender) amount))
      (var-set total-deposits (+ (var-get total-deposits) amount))
      (+ total amount)
    )
    error total
  )
)

(define-read-only (get-contract-info)
  {
    paused: (var-get contract-paused),
    total-deposits: (var-get total-deposits),
    owner: CONTRACT_OWNER,
    pause-history-count: (var-get pause-history-nonce)
  }
)