(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u400))
(define-constant ERR_RATE_LIMIT_EXCEEDED (err u401))
(define-constant ERR_INVALID_OPERATION (err u402))
(define-constant ERR_INVALID_LIMIT (err u403))
(define-constant ERR_USER_NOT_FOUND (err u404))

(define-constant DEFAULT_TIME_WINDOW u144)
(define-constant MAX_OPERATIONS_PER_WINDOW u100)
(define-constant MIN_TIME_WINDOW u10)
(define-constant MAX_TIME_WINDOW u1440)

(define-data-var global-rate-limiting-enabled bool true)
(define-data-var violation-penalty-blocks uint u72)

(define-map admins principal bool)
(define-map operation-limits (string-ascii 20) {max-calls: uint, time-window: uint})
(define-map user-activity {user: principal, operation: (string-ascii 20)} {call-count: uint, window-start: uint, last-call: uint})
(define-map user-violations principal {violation-count: uint, last-violation: uint, penalty-until: uint})
(define-map operation-stats (string-ascii 20) {total-calls: uint, total-violations: uint, last-updated: uint})

(map-set admins CONTRACT_OWNER true)

(map-set operation-limits "deposit" {max-calls: u10, time-window: u144})
(map-set operation-limits "withdraw" {max-calls: u5, time-window: u144})
(map-set operation-limits "transfer" {max-calls: u20, time-window: u144})
(map-set operation-limits "batch-deposit" {max-calls: u2, time-window: u288})

(define-read-only (is-admin (user principal))
  (default-to false (map-get? admins user))
)

(define-read-only (is-rate-limiting-enabled)
  (var-get global-rate-limiting-enabled)
)

(define-read-only (get-operation-limit (operation (string-ascii 20)))
  (map-get? operation-limits operation)
)

(define-read-only (get-user-activity (user principal) (operation (string-ascii 20)))
  (map-get? user-activity {user: user, operation: operation})
)

(define-read-only (get-user-violations (user principal))
  (default-to {violation-count: u0, last-violation: u0, penalty-until: u0} (map-get? user-violations user))
)

(define-read-only (get-operation-stats (operation (string-ascii 20)))
  (default-to {total-calls: u0, total-violations: u0, last-updated: u0} (map-get? operation-stats operation))
)

(define-read-only (is-user-penalized (user principal))
  (let ((violation-data (get-user-violations user)))
    (> (get penalty-until violation-data) stacks-block-height)
  )
)

(define-read-only (can-user-perform-operation (user principal) (operation (string-ascii 20)))
  (if (not (var-get global-rate-limiting-enabled))
    true
    (if (is-user-penalized user)
      false
      (match (get-operation-limit operation)
        limit-data (let (
          (activity (get-user-activity user operation))
          (current-block stacks-block-height)
        )
          (match activity
            user-data (let (
              (window-start (get window-start user-data))
              (call-count (get call-count user-data))
              (time-window (get time-window limit-data))
              (max-calls (get max-calls limit-data))
            )
              (if (>= (- current-block window-start) time-window)
                true
                (< call-count max-calls)
              )
            )
            true
          )
        )
        false
      )
    )
  )
)

(define-private (record-operation-call (user principal) (operation (string-ascii 20)))
  (match (get-operation-limit operation)
    limit-data (let (
      (current-block stacks-block-height)
      (activity (get-user-activity user operation))
      (time-window (get time-window limit-data))
    )
      (match activity
        user-data (let (
          (window-start (get window-start user-data))
          (call-count (get call-count user-data))
        )
          (if (>= (- current-block window-start) time-window)
            (map-set user-activity {user: user, operation: operation} {
              call-count: u1,
              window-start: current-block,
              last-call: current-block
            })
            (map-set user-activity {user: user, operation: operation} {
              call-count: (+ call-count u1),
              window-start: window-start,
              last-call: current-block
            })
          )
        )
        (map-set user-activity {user: user, operation: operation} {
          call-count: u1,
          window-start: current-block,
          last-call: current-block
        })
      )
      (update-operation-stats operation)
    )
    false
  )
)

(define-private (record-violation (user principal) (operation (string-ascii 20)))
  (let (
    (current-violations (get-user-violations user))
    (violation-count (get violation-count current-violations))
    (penalty-blocks (var-get violation-penalty-blocks))
  )
    (map-set user-violations user {
      violation-count: (+ violation-count u1),
      last-violation: stacks-block-height,
      penalty-until: (+ stacks-block-height penalty-blocks)
    })
    (update-operation-violation-stats operation)
  )
)

(define-private (update-operation-stats (operation (string-ascii 20)))
  (let ((current-stats (get-operation-stats operation)))
    (map-set operation-stats operation {
      total-calls: (+ (get total-calls current-stats) u1),
      total-violations: (get total-violations current-stats),
      last-updated: stacks-block-height
    })
  )
)

(define-private (update-operation-violation-stats (operation (string-ascii 20)))
  (let ((current-stats (get-operation-stats operation)))
    (map-set operation-stats operation {
      total-calls: (get total-calls current-stats),
      total-violations: (+ (get total-violations current-stats) u1),
      last-updated: stacks-block-height
    })
  )
)

(define-public (check-and-update-rate-limit (user principal) (operation (string-ascii 20)))
  (if (not (var-get global-rate-limiting-enabled))
    (ok true)
    (if (can-user-perform-operation user operation)
      (begin
        (record-operation-call user operation)
        (ok true)
      )
      (begin
        (record-violation user operation)
        ERR_RATE_LIMIT_EXCEEDED
      )
    )
  )
)

(define-public (set-operation-limit (operation (string-ascii 20)) (max-calls uint) (time-window uint))
  (begin
    (asserts! (is-admin tx-sender) ERR_UNAUTHORIZED)
    (asserts! (<= max-calls MAX_OPERATIONS_PER_WINDOW) ERR_INVALID_LIMIT)
    (asserts! (and (>= time-window MIN_TIME_WINDOW) (<= time-window MAX_TIME_WINDOW)) ERR_INVALID_LIMIT)
    (map-set operation-limits operation {max-calls: max-calls, time-window: time-window})
    (ok true)
  )
)

(define-public (toggle-rate-limiting)
  (begin
    (asserts! (is-admin tx-sender) ERR_UNAUTHORIZED)
    (var-set global-rate-limiting-enabled (not (var-get global-rate-limiting-enabled)))
    (ok (var-get global-rate-limiting-enabled))
  )
)

(define-public (set-violation-penalty (penalty-blocks uint))
  (begin
    (asserts! (is-admin tx-sender) ERR_UNAUTHORIZED)
    (asserts! (<= penalty-blocks MAX_TIME_WINDOW) ERR_INVALID_LIMIT)
    (var-set violation-penalty-blocks penalty-blocks)
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

(define-public (clear-user-violations (user principal))
  (begin
    (asserts! (is-admin tx-sender) ERR_UNAUTHORIZED)
    (map-delete user-violations user)
    (ok true)
  )
)

(define-public (reset-user-activity (user principal) (operation (string-ascii 20)))
  (begin
    (asserts! (is-admin tx-sender) ERR_UNAUTHORIZED)
    (map-delete user-activity {user: user, operation: operation})
    (ok true)
  )
)

(define-read-only (get-user-rate-limit-status (user principal) (operation (string-ascii 20)))
  (let (
    (can-perform (can-user-perform-operation user operation))
    (is-penalized (is-user-penalized user))
    (activity (get-user-activity user operation))
    (violations (get-user-violations user))
  )
    {
      can-perform: can-perform,
      is-penalized: is-penalized,
      activity: activity,
      violations: violations
    }
  )
)

(define-read-only (get-all-operation-limits)
  {
    deposit: (get-operation-limit "deposit"),
    withdraw: (get-operation-limit "withdraw"),
    transfer: (get-operation-limit "transfer"),
    batch-deposit: (get-operation-limit "batch-deposit")
  }
)

(define-read-only (get-system-stats)
  {
    rate-limiting-enabled: (var-get global-rate-limiting-enabled),
    violation-penalty-blocks: (var-get violation-penalty-blocks),
    deposit-stats: (get-operation-stats "deposit"),
    withdraw-stats: (get-operation-stats "withdraw"),
    transfer-stats: (get-operation-stats "transfer"),
    batch-deposit-stats: (get-operation-stats "batch-deposit")
  }
)
