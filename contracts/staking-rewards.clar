(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u500))
(define-constant ERR_STAKE_NOT_FOUND (err u501))
(define-constant ERR_INSUFFICIENT_AMOUNT (err u502))
(define-constant ERR_STAKE_LOCKED (err u503))
(define-constant ERR_INVALID_TIER (err u504))
(define-constant ERR_NO_REWARDS (err u505))
(define-constant ERR_POOL_DEPLETED (err u506))
(define-constant ERR_ALREADY_CLAIMED (err u507))

(define-constant TIER_SHORT u1)
(define-constant TIER_MEDIUM u2)
(define-constant TIER_LONG u3)
(define-constant TIER_EXTENDED u4)

(define-constant LOCK_PERIOD_SHORT u144)
(define-constant LOCK_PERIOD_MEDIUM u720)
(define-constant LOCK_PERIOD_LONG u2160)
(define-constant LOCK_PERIOD_EXTENDED u4320)

(define-constant REWARD_RATE_SHORT u5)
(define-constant REWARD_RATE_MEDIUM u12)
(define-constant REWARD_RATE_LONG u25)
(define-constant REWARD_RATE_EXTENDED u50)

(define-constant MIN_STAKE_AMOUNT u1000000)
(define-constant REWARD_PRECISION u10000)

(define-data-var stake-nonce uint u0)
(define-data-var total-staked uint u0)
(define-data-var total-rewards-distributed uint u0)
(define-data-var reward-pool-balance uint u0)
(define-data-var staking-enabled bool true)

(define-map admins principal bool)
(define-map user-stakes {user: principal, stake-id: uint} {
  amount: uint,
  tier: uint,
  start-block: uint,
  unlock-block: uint,
  reward-rate: uint,
  claimed: bool,
  last-claim-block: uint
})
(define-map user-stake-count principal uint)
(define-map stake-tier-stats uint {total-staked: uint, total-stakers: uint, total-rewards: uint})

(map-set admins CONTRACT_OWNER true)

(map-set stake-tier-stats TIER_SHORT {total-staked: u0, total-stakers: u0, total-rewards: u0})
(map-set stake-tier-stats TIER_MEDIUM {total-staked: u0, total-stakers: u0, total-rewards: u0})
(map-set stake-tier-stats TIER_LONG {total-staked: u0, total-stakers: u0, total-rewards: u0})
(map-set stake-tier-stats TIER_EXTENDED {total-staked: u0, total-stakers: u0, total-rewards: u0})

(define-read-only (is-admin (user principal))
  (default-to false (map-get? admins user))
)

(define-read-only (is-staking-enabled)
  (var-get staking-enabled)
)

(define-read-only (get-stake (user principal) (stake-id uint))
  (map-get? user-stakes {user: user, stake-id: stake-id})
)

(define-read-only (get-user-stake-count (user principal))
  (default-to u0 (map-get? user-stake-count user))
)

(define-read-only (get-tier-stats (tier uint))
  (default-to {total-staked: u0, total-stakers: u0, total-rewards: u0} (map-get? stake-tier-stats tier))
)

(define-read-only (get-total-staked)
  (var-get total-staked)
)

(define-read-only (get-reward-pool-balance)
  (var-get reward-pool-balance)
)

(define-read-only (get-lock-period-for-tier (tier uint))
  (if (is-eq tier TIER_SHORT)
    LOCK_PERIOD_SHORT
    (if (is-eq tier TIER_MEDIUM)
      LOCK_PERIOD_MEDIUM
      (if (is-eq tier TIER_LONG)
        LOCK_PERIOD_LONG
        (if (is-eq tier TIER_EXTENDED)
          LOCK_PERIOD_EXTENDED
          u0
        )
      )
    )
  )
)

(define-read-only (get-reward-rate-for-tier (tier uint))
  (if (is-eq tier TIER_SHORT)
    REWARD_RATE_SHORT
    (if (is-eq tier TIER_MEDIUM)
      REWARD_RATE_MEDIUM
      (if (is-eq tier TIER_LONG)
        REWARD_RATE_LONG
        (if (is-eq tier TIER_EXTENDED)
          REWARD_RATE_EXTENDED
          u0
        )
      )
    )
  )
)

(define-read-only (is-stake-unlocked (user principal) (stake-id uint))
  (match (get-stake user stake-id)
    stake-data (>= stacks-block-height (get unlock-block stake-data))
    false
  )
)

(define-read-only (calculate-pending-rewards (user principal) (stake-id uint))
  (match (get-stake user stake-id)
    stake-data (let (
      (amount (get amount stake-data))
      (reward-rate (get reward-rate stake-data))
      (last-claim (get last-claim-block stake-data))
      (current-block stacks-block-height)
      (unlock-block (get unlock-block stake-data))
      (blocks-staked (if (> current-block unlock-block)
        (- unlock-block last-claim)
        (- current-block last-claim)
      ))
    )
      (/ (* (* amount reward-rate) blocks-staked) (* REWARD_PRECISION u144))
    )
    u0
  )
)

(define-private (update-tier-stats-on-stake (tier uint) (amount uint))
  (let ((current-stats (get-tier-stats tier)))
    (map-set stake-tier-stats tier {
      total-staked: (+ (get total-staked current-stats) amount),
      total-stakers: (+ (get total-stakers current-stats) u1),
      total-rewards: (get total-rewards current-stats)
    })
  )
)

(define-private (update-tier-stats-on-unstake (tier uint) (amount uint))
  (let ((current-stats (get-tier-stats tier)))
    (map-set stake-tier-stats tier {
      total-staked: (- (get total-staked current-stats) amount),
      total-stakers: (- (get total-stakers current-stats) u1),
      total-rewards: (get total-rewards current-stats)
    })
  )
)

(define-private (update-tier-stats-on-reward (tier uint) (reward-amount uint))
  (let ((current-stats (get-tier-stats tier)))
    (map-set stake-tier-stats tier {
      total-staked: (get total-staked current-stats),
      total-stakers: (get total-stakers current-stats),
      total-rewards: (+ (get total-rewards current-stats) reward-amount)
    })
  )
)

(define-public (create-stake (amount uint) (tier uint))
  (let (
    (user-count (get-user-stake-count tx-sender))
    (lock-period (get-lock-period-for-tier tier))
    (reward-rate (get-reward-rate-for-tier tier))
    (current-block stacks-block-height)
  )
    (begin
      (asserts! (var-get staking-enabled) ERR_UNAUTHORIZED)
      (asserts! (>= amount MIN_STAKE_AMOUNT) ERR_INSUFFICIENT_AMOUNT)
      (asserts! (> lock-period u0) ERR_INVALID_TIER)
      (match (stx-transfer? amount tx-sender (as-contract tx-sender))
        success (begin
          (map-set user-stakes {user: tx-sender, stake-id: user-count} {
            amount: amount,
            tier: tier,
            start-block: current-block,
            unlock-block: (+ current-block lock-period),
            reward-rate: reward-rate,
            claimed: false,
            last-claim-block: current-block
          })
          (map-set user-stake-count tx-sender (+ user-count u1))
          (var-set total-staked (+ (var-get total-staked) amount))
          (update-tier-stats-on-stake tier amount)
          (ok user-count)
        )
        error ERR_INSUFFICIENT_AMOUNT
      )
    )
  )
)

(define-public (claim-rewards (stake-id uint))
  (match (get-stake tx-sender stake-id)
    stake-data (let (
      (pending-rewards (calculate-pending-rewards tx-sender stake-id))
      (current-pool (var-get reward-pool-balance))
    )
      (begin
        (asserts! (> pending-rewards u0) ERR_NO_REWARDS)
        (asserts! (>= current-pool pending-rewards) ERR_POOL_DEPLETED)
        (match (as-contract (stx-transfer? pending-rewards tx-sender tx-sender))
          success (begin
            (map-set user-stakes {user: tx-sender, stake-id: stake-id} 
              (merge stake-data {last-claim-block: stacks-block-height})
            )
            (var-set reward-pool-balance (- current-pool pending-rewards))
            (var-set total-rewards-distributed (+ (var-get total-rewards-distributed) pending-rewards))
            (update-tier-stats-on-reward (get tier stake-data) pending-rewards)
            (ok pending-rewards)
          )
          error ERR_POOL_DEPLETED
        )
      )
    )
    ERR_STAKE_NOT_FOUND
  )
)

(define-public (unstake (stake-id uint))
  (match (get-stake tx-sender stake-id)
    stake-data (let (
      (amount (get amount stake-data))
      (tier (get tier stake-data))
      (pending-rewards (calculate-pending-rewards tx-sender stake-id))
      (total-payout (+ amount pending-rewards))
      (current-pool (var-get reward-pool-balance))
    )
      (begin
        (asserts! (not (get claimed stake-data)) ERR_ALREADY_CLAIMED)
        (asserts! (>= stacks-block-height (get unlock-block stake-data)) ERR_STAKE_LOCKED)
        (asserts! (>= current-pool pending-rewards) ERR_POOL_DEPLETED)
        (match (as-contract (stx-transfer? total-payout tx-sender tx-sender))
          success (begin
            (map-set user-stakes {user: tx-sender, stake-id: stake-id} 
              (merge stake-data {claimed: true})
            )
            (var-set total-staked (- (var-get total-staked) amount))
            (var-set reward-pool-balance (- current-pool pending-rewards))
            (var-set total-rewards-distributed (+ (var-get total-rewards-distributed) pending-rewards))
            (update-tier-stats-on-unstake tier amount)
            (update-tier-stats-on-reward tier pending-rewards)
            (ok {amount: amount, rewards: pending-rewards, total: total-payout})
          )
          error ERR_INSUFFICIENT_AMOUNT
        )
      )
    )
    ERR_STAKE_NOT_FOUND
  )
)

(define-public (fund-reward-pool (amount uint))
  (begin
    (asserts! (> amount u0) ERR_INSUFFICIENT_AMOUNT)
    (match (stx-transfer? amount tx-sender (as-contract tx-sender))
      success (begin
        (var-set reward-pool-balance (+ (var-get reward-pool-balance) amount))
        (ok amount)
      )
      error ERR_INSUFFICIENT_AMOUNT
    )
  )
)

(define-public (toggle-staking)
  (begin
    (asserts! (is-admin tx-sender) ERR_UNAUTHORIZED)
    (var-set staking-enabled (not (var-get staking-enabled)))
    (ok (var-get staking-enabled))
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

(define-read-only (get-user-stake-info (user principal) (stake-id uint))
  (match (get-stake user stake-id)
    stake-data {
      stake: (some stake-data),
      pending-rewards: (calculate-pending-rewards user stake-id),
      is-unlocked: (is-stake-unlocked user stake-id),
      blocks-remaining: (if (>= stacks-block-height (get unlock-block stake-data))
        u0
        (- (get unlock-block stake-data) stacks-block-height)
      )
    }
    {
      stake: none,
      pending-rewards: u0,
      is-unlocked: false,
      blocks-remaining: u0
    }
  )
)

(define-read-only (get-tier-info (tier uint))
  {
    tier: tier,
    lock-period: (get-lock-period-for-tier tier),
    reward-rate: (get-reward-rate-for-tier tier),
    stats: (get-tier-stats tier)
  }
)

(define-read-only (get-all-tier-info)
  {
    short: (get-tier-info TIER_SHORT),
    medium: (get-tier-info TIER_MEDIUM),
    long: (get-tier-info TIER_LONG),
    extended: (get-tier-info TIER_EXTENDED)
  }
)

(define-read-only (get-contract-stats)
  {
    staking-enabled: (var-get staking-enabled),
    total-staked: (var-get total-staked),
    reward-pool-balance: (var-get reward-pool-balance),
    total-rewards-distributed: (var-get total-rewards-distributed),
    total-stakes-created: (var-get stake-nonce),
    min-stake-amount: MIN_STAKE_AMOUNT
  }
)
