;; Delta-Activate Prediction Market Protocol
;; A simplified prediction market with staking and liquidity provision

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-insufficient-balance (err u103))
(define-constant err-market-closed (err u104))
(define-constant err-market-not-resolved (err u105))
(define-constant err-already-predicted (err u106))
(define-constant err-invalid-outcome (err u107))

;; Data Variables
(define-data-var market-counter uint u0)
(define-data-var min-stake-amount uint u1000000) ;; 1 STX minimum

;; Data Maps
(define-map markets
  uint
  {
    creator: principal,
    description: (string-ascii 256),
    total-pool: uint,
    outcome-count: uint,
    resolved: bool,
    winning-outcome: (optional uint),
    end-block: uint
  }
)

(define-map market-outcomes
  { market-id: uint, outcome-id: uint }
  {
    total-staked: uint,
    description: (string-ascii 128)
  }
)

(define-map user-predictions
  { market-id: uint, user: principal }
  {
    outcome-id: uint,
    amount: uint,
    block-height: uint
  }
)

(define-map user-accuracy-scores
  principal
  {
    total-predictions: uint,
    correct-predictions: uint,
    accuracy-multiplier: uint ;; basis points (10000 = 1x)
  }
)

(define-map liquidity-positions
  { market-id: uint, provider: principal }
  {
    amount: uint,
    entry-block: uint
  }
)

;; Private helper functions for min/max
(define-private (min-uint (a uint) (b uint))
  (if (<= a b) a b)
)

(define-private (max-uint (a uint) (b uint))
  (if (>= a b) a b)
)

;; Read-only functions
(define-read-only (get-market (market-id uint))
  (map-get? markets market-id)
)

(define-read-only (get-market-outcome (market-id uint) (outcome-id uint))
  (map-get? market-outcomes { market-id: market-id, outcome-id: outcome-id })
)

(define-read-only (get-user-prediction (market-id uint) (user principal))
  (map-get? user-predictions { market-id: market-id, user: user })
)

(define-read-only (get-user-accuracy (user principal))
  (default-to 
    { total-predictions: u0, correct-predictions: u0, accuracy-multiplier: u10000 }
    (map-get? user-accuracy-scores user)
  )
)

(define-read-only (get-liquidity-position (market-id uint) (provider principal))
  (map-get? liquidity-positions { market-id: market-id, provider: provider })
)

(define-read-only (calculate-payout (market-id uint) (user principal))
  (let (
    (market (unwrap! (get-market market-id) (err err-not-found)))
    (prediction (unwrap! (get-user-prediction market-id user) (err err-not-found)))
    (winning-outcome (unwrap! (get winning-outcome market) (err err-market-not-resolved)))
    (user-outcome (get outcome-id prediction))
    (stake-amount (get amount prediction))
    (total-pool (get total-pool market))
    (winning-outcome-data (unwrap! (get-market-outcome market-id winning-outcome) (err err-not-found)))
    (winning-total (get total-staked winning-outcome-data))
  )
    (if (is-eq user-outcome winning-outcome)
      (ok (/ (* stake-amount total-pool) winning-total))
      (ok u0)
    )
  )
)

;; Public functions
(define-public (create-market (description (string-ascii 256)) (outcome-count uint) (duration-blocks uint))
  (let (
    (market-id (+ (var-get market-counter) u1))
    (end-block (+ block-height duration-blocks))
  )
    (asserts! (> outcome-count u1) (err err-invalid-outcome))
    (map-set markets market-id {
      creator: tx-sender,
      description: description,
      total-pool: u0,
      outcome-count: outcome-count,
      resolved: false,
      winning-outcome: none,
      end-block: end-block
    })
    (var-set market-counter market-id)
    (ok market-id)
  )
)

(define-public (set-outcome-description (market-id uint) (outcome-id uint) (description (string-ascii 128)))
  (let (
    (market (unwrap! (get-market market-id) err-not-found))
  )
    (asserts! (is-eq tx-sender (get creator market)) err-owner-only)
    (asserts! (< outcome-id (get outcome-count market)) err-invalid-outcome)
    (map-set market-outcomes 
      { market-id: market-id, outcome-id: outcome-id }
      { total-staked: u0, description: description }
    )
    (ok true)
  )
)

(define-public (stake-on-outcome (market-id uint) (outcome-id uint) (amount uint))
  (let (
    (market (unwrap! (get-market market-id) err-not-found))
    (existing-prediction (get-user-prediction market-id tx-sender))
    (outcome-data (default-to 
      { total-staked: u0, description: "" }
      (get-market-outcome market-id outcome-id)
    ))
  )
    (asserts! (is-none existing-prediction) err-already-predicted)
    (asserts! (not (get resolved market)) err-market-closed)
    (asserts! (< block-height (get end-block market)) err-market-closed)
    (asserts! (< outcome-id (get outcome-count market)) err-invalid-outcome)
    (asserts! (>= amount (var-get min-stake-amount)) err-insufficient-balance)
    
    ;; Transfer STX to contract
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    ;; Update market total pool
    (map-set markets market-id 
      (merge market { total-pool: (+ (get total-pool market) amount) })
    )
    
    ;; Update outcome total
    (map-set market-outcomes 
      { market-id: market-id, outcome-id: outcome-id }
      (merge outcome-data { total-staked: (+ (get total-staked outcome-data) amount) })
    )
    
    ;; Record user prediction
    (map-set user-predictions
      { market-id: market-id, user: tx-sender }
      {
        outcome-id: outcome-id,
        amount: amount,
        block-height: block-height
      }
    )
    
    (ok true)
  )
)

(define-public (provide-liquidity (market-id uint) (amount uint))
  (let (
    (market (unwrap! (get-market market-id) err-not-found))
  )
    (asserts! (not (get resolved market)) err-market-closed)
    
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    (map-set liquidity-positions
      { market-id: market-id, provider: tx-sender }
      {
        amount: amount,
        entry-block: block-height
      }
    )
    
    (ok true)
  )
)

(define-public (resolve-market (market-id uint) (winning-outcome uint))
  (let (
    (market (unwrap! (get-market market-id) err-not-found))
  )
    (asserts! (is-eq tx-sender (get creator market)) err-owner-only)
    (asserts! (not (get resolved market)) err-market-closed)
    (asserts! (>= block-height (get end-block market)) err-market-closed)
    (asserts! (< winning-outcome (get outcome-count market)) err-invalid-outcome)
    
    (map-set markets market-id
      (merge market {
        resolved: true,
        winning-outcome: (some winning-outcome)
      })
    )
    
    (ok true)
  )
)

(define-public (claim-winnings (market-id uint))
  (let (
    (market (unwrap! (get-market market-id) err-not-found))
    (prediction (unwrap! (get-user-prediction market-id tx-sender) err-not-found))
    (payout (unwrap! (calculate-payout market-id tx-sender) err-market-not-resolved))
    (user-accuracy (get-user-accuracy tx-sender))
  )
    (asserts! (get resolved market) err-market-not-resolved)
    (asserts! (> payout u0) err-insufficient-balance)
    
    ;; Transfer winnings
    (try! (as-contract (stx-transfer? payout tx-sender tx-sender)))
    
    ;; Update accuracy scores
    (if (> payout u0)
      (map-set user-accuracy-scores tx-sender {
        total-predictions: (+ (get total-predictions user-accuracy) u1),
        correct-predictions: (+ (get correct-predictions user-accuracy) u1),
        accuracy-multiplier: (min-uint u20000 (+ (get accuracy-multiplier user-accuracy) u500))
      })
      (map-set user-accuracy-scores tx-sender {
        total-predictions: (+ (get total-predictions user-accuracy) u1),
        correct-predictions: (get correct-predictions user-accuracy),
        accuracy-multiplier: (max-uint u5000 (- (get accuracy-multiplier user-accuracy) u250))
      })
    )
    
    ;; Remove prediction record
    (map-delete user-predictions { market-id: market-id, user: tx-sender })
    
    (ok payout)
  )
)

(define-public (set-min-stake (new-min uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set min-stake-amount new-min)
    (ok true)
  )
)