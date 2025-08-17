;; Enhanced Bitcoin Prediction Market
;; Advanced Features: Multi-outcome, Oracle Integration, Risk Management, Security
;; ----------------------------

;; Error Constants
(define-constant ERR-TRANSFER-FAILED (err u100))
(define-constant ERR-INVALID-DEADLINE (err u101))
(define-constant ERR-INVALID-STAKE (err u102))
(define-constant ERR-NOT-AUTHORIZED (err u403))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-INVALID-BET (err u105))
(define-constant ERR-PREDICTION-EXPIRED (err u106))
(define-constant ERR-OUTCOME-ALREADY-SET (err u107))
(define-constant ERR-PREDICTION-NOT-EXPIRED (err u108))
(define-constant ERR-ALREADY-CLAIMED (err u109))
(define-constant ERR-NO-WINNINGS (err u110))
(define-constant ERR-BETTING-CLOSED (err u111))
(define-constant ERR-INVALID-OUTCOME (err u112))
(define-constant ERR-INSUFFICIENT-REPUTATION (err u113))
(define-constant ERR-BET-LIMIT-EXCEEDED (err u114))
(define-constant ERR-MARKET-PAUSED (err u115))
(define-constant ERR-DISPUTE-PERIOD-ACTIVE (err u116))
(define-constant ERR-ORACLE-NOT-AUTHORIZED (err u117))
(define-constant ERR-INVALID-MULTISIG (err u118))
(define-constant ERR-CIRCUIT-BREAKER-ACTIVE (err u119))
(define-constant ERR-INSUFFICIENT-INSURANCE (err u120))

;; Contract Constants
(define-constant MIN-STAKE u1000000) ;; 1 STX minimum
(define-constant MIN-BET u100000) ;; 0.1 STX minimum
(define-constant MAX-BET-RATIO u500) ;; Max 50% of total pool per bet
(define-constant DISPUTE-PERIOD u144) ;; 24 hours in blocks
(define-constant MIN-REPUTATION u100)
(define-constant CIRCUIT-BREAKER-THRESHOLD u10000000000) ;; 10,000 STX
(define-constant INSURANCE-RATE u20) ;; 2% insurance fee

;; Prediction Types
(define-constant PREDICTION-BINARY u0)
(define-constant PREDICTION-MULTI u1)
(define-constant PREDICTION-NUMERICAL u2)

;; Data Maps
(define-map predictions
  uint
  {
    creator: principal,
    prediction-type: uint,
    block-deadline: uint,
    condition: (buff 32),
    stake: uint,
    outcome: (optional uint), ;; Changed to uint for multi-outcome support
    created-at: uint,
    total-bets: uint,
    is-settled: bool,
    dispute-end: (optional uint),
    oracle-address: (optional principal),
    num-outcomes: uint, ;; For multi-outcome predictions
    insurance-pool: uint,
    reputation-required: uint,
  }
)

(define-map prediction-outcomes
  {
    prediction-id: uint,
    outcome-id: uint,
  }
  {
    total-bets: uint,
    description: (string-ascii 100),
  }
)

(define-map bets
  {
    prediction-id: uint,
    bettor: principal,
  }
  {
    amount: uint,
    outcome-id: uint, ;; Changed from bet-on bool to outcome-id uint
    claimed: bool,
    timestamp: uint,
  }
)

(define-map user-reputation
  principal
  {
    score: uint,
    total-predictions: uint,
    correct-predictions: uint,
    total-volume: uint,
  }
)

(define-map oracles
  principal
  {
    is-authorized: bool,
    reputation: uint,
    total-resolutions: uint,
    correct-resolutions: uint,
  }
)

(define-map disputes
  uint
  {
    disputer: principal,
    disputed-outcome: uint,
    evidence-hash: (buff 32),
    dispute-stake: uint,
    votes-for: uint,
    votes-against: uint,
    resolved: bool,
  }
)

(define-map multisig-proposals
  uint
  {
    proposal-type: uint, ;; 0: outcome, 1: parameter change, 2: emergency
    target-id: uint,
    proposed-value: uint,
    proposer: principal,
    signatures: (list 10 principal),
    executed: bool,
    created-at: uint,
  }
)

(define-map user-limits
  principal
  {
    daily-limit: uint,
    daily-spent: uint,
    last-reset: uint,
    is-vip: bool,
  }
)

;; Data Variables
(define-data-var prediction-counter uint u0)
(define-data-var proposal-counter uint u0)
(define-data-var dispute-counter uint u0)
(define-data-var contract-fee uint u50) ;; 5% fee
(define-data-var contract-owner principal tx-sender)
(define-data-var is-paused bool false)
(define-data-var circuit-breaker-active bool false)
(define-data-var required-signatures uint u3)
(define-data-var insurance-fund uint u0)

;; Authorization Lists
(define-data-var authorized-oracles (list 20 principal) (list))
(define-data-var multisig-members (list 10 principal) (list))

;; Read-only Functions
(define-read-only (get-prediction-readonly (id uint))
  (map-get? predictions id)
)

(define-read-only (get-prediction-outcome
    (prediction-id uint)
    (outcome-id uint)
  )
  (map-get? prediction-outcomes {
    prediction-id: prediction-id,
    outcome-id: outcome-id,
  })
)

(define-read-only (get-user-bet
    (prediction-id uint)
    (bettor principal)
  )
  (map-get? bets {
    prediction-id: prediction-id,
    bettor: bettor,
  })
)

(define-read-only (get-user-reputation (user principal))
  (default-to {
    score: u0,
    total-predictions: u0,
    correct-predictions: u0,
    total-volume: u0,
  }
    (map-get? user-reputation user)
  )
)

(define-read-only (get-oracle-info (oracle principal))
  (map-get? oracles oracle)
)

(define-read-only (is-oracle-authorized (oracle principal))
  (match (map-get? oracles oracle)
    oracle-info (get is-authorized oracle-info)
    false
  )
)

(define-read-only (get-dispute (dispute-id uint))
  (map-get? disputes dispute-id)
)

(define-read-only (calculate-reputation-score
    (correct uint)
    (total uint)
  )
  (if (> total u0)
    (/ (* correct u1000) total)
    u0
  )
)

(define-read-only (get-user-daily-limit (user principal))
  (match (map-get? user-limits user)
    limits
    (if (get is-vip limits)
      u50000000000
      u5000000000
    )
    ;; 50k STX for VIP, 5k for regular
    u1000000000 ;; 1k STX default
  )
)

;; Administrative Functions
(define-public (set-contract-owner (new-owner principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    (var-set contract-owner new-owner)
    (ok true)
  )
)

(define-public (toggle-pause)
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    (var-set is-paused (not (var-get is-paused)))
    (ok (var-get is-paused))
  )
)

(define-public (add-oracle (oracle principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    (map-set oracles oracle {
      is-authorized: true,
      reputation: u1000,
      total-resolutions: u0,
      correct-resolutions: u0,
    })
    (var-set authorized-oracles
      (unwrap! (as-max-len? (append (var-get authorized-oracles) oracle) u20)
        ERR-INVALID-BET
      ))
    (ok true)
  )
)

(define-public (add-multisig-member (member principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    (var-set multisig-members
      (unwrap! (as-max-len? (append (var-get multisig-members) member) u10)
        ERR-INVALID-MULTISIG
      ))
    (ok true)
  )
)

;; Enhanced Prediction Creation
(define-public (create-multi-outcome-prediction
    (block-deadline uint)
    (condition-hash (buff 32))
    (stake uint)
    (outcomes (list 10 (string-ascii 100)))
    (oracle (optional principal))
    (reputation-required uint)
  )
  (let (
      (id (var-get prediction-counter))
      (num-outcomes (len outcomes))
    )
    (begin
      ;; Validations
      (asserts! (not (var-get is-paused)) ERR-MARKET-PAUSED)
      (asserts! (not (var-get circuit-breaker-active)) ERR-CIRCUIT-BREAKER-ACTIVE)
      (asserts! (> block-deadline stacks-block-height) ERR-INVALID-DEADLINE)
      (asserts! (>= stake MIN-STAKE) ERR-INVALID-STAKE)
      (asserts! (and (> num-outcomes u1) (<= num-outcomes u10))
        ERR-INVALID-OUTCOME
      )
      (asserts!
        (>= (get score (get-user-reputation tx-sender)) reputation-required)
        ERR-INSUFFICIENT-REPUTATION
      )

      ;; Check oracle authorization if provided
      (match oracle
        oracle-addr (asserts! (is-oracle-authorized oracle-addr) ERR-ORACLE-NOT-AUTHORIZED)
        true
      )

      ;; Calculate insurance requirement
      (let ((insurance-amount (/ (* stake INSURANCE-RATE) u1000)))
        ;; Transfer stake + insurance to contract
        (unwrap!
          (stx-transfer? (+ stake insurance-amount) tx-sender
            (as-contract tx-sender)
          )
          ERR-TRANSFER-FAILED
        )

        ;; Update insurance fund
        (var-set insurance-fund (+ (var-get insurance-fund) insurance-amount))

        ;; Update prediction counter
        (var-set prediction-counter (+ id u1))

        ;; Create prediction
        (map-set predictions id {
          creator: tx-sender,
          prediction-type: PREDICTION-MULTI,
          block-deadline: block-deadline,
          condition: condition-hash,
          stake: stake,
          outcome: none,
          created-at: stacks-block-height,
          total-bets: u0,
          is-settled: false,
          dispute-end: none,
          oracle-address: oracle,
          num-outcomes: num-outcomes,
          insurance-pool: insurance-amount,
          reputation-required: reputation-required,
        })

        ;; Create outcome entries
        (map create-outcome-entry
          (map + (list u0 u1 u2 u3 u4 u5 u6 u7 u8 u9)
            (list u0 u0 u0 u0 u0 u0 u0 u0 u0 u0)
          )
          outcomes
        )

        (ok id)
      )
    )
  )
)

;; Helper function for creating outcome entries
(define-private (create-outcome-entry
    (outcome-id uint)
    (description (string-ascii 100))
  )
  (let ((prediction-id (- (var-get prediction-counter) u1)))
    (if (< outcome-id
        (unwrap-panic (get num-outcomes (map-get? predictions prediction-id)))
      )
      (map-set prediction-outcomes {
        prediction-id: prediction-id,
        outcome-id: outcome-id,
      } {
        total-bets: u0,
        description: description,
      })
      false
    )
  )
)

;; Enhanced Betting with Risk Management
(define-public (place-bet-with-limits
    (prediction-id uint)
    (outcome-id uint)
    (amount uint)
  )
  (let (
      (prediction (unwrap! (map-get? predictions prediction-id) ERR-NOT-FOUND))
      (user-rep (get-user-reputation tx-sender))
      (daily-limit (get-user-daily-limit tx-sender))
      (user-limits-data (default-to {
        daily-limit: daily-limit,
        daily-spent: u0,
        last-reset: stacks-block-height,
        is-vip: false,
      }
        (map-get? user-limits tx-sender)
      ))
    )
    (begin
      ;; Validations
      (asserts! (not (var-get is-paused)) ERR-MARKET-PAUSED)
      (asserts! (not (var-get circuit-breaker-active)) ERR-CIRCUIT-BREAKER-ACTIVE)
      (asserts! (>= amount MIN-BET) ERR-INVALID-BET)
      (asserts! (< outcome-id (get num-outcomes prediction)) ERR-INVALID-OUTCOME)
      (asserts! (< stacks-block-height (get block-deadline prediction))
        ERR-PREDICTION-EXPIRED
      )
      (asserts! (is-none (get outcome prediction)) ERR-BETTING-CLOSED)
      (asserts! (>= (get score user-rep) (get reputation-required prediction))
        ERR-INSUFFICIENT-REPUTATION
      )

      ;; Check bet size limits (max 50% of current pool)
      (let ((max-bet (/ (* (get total-bets prediction) MAX-BET-RATIO) u1000)))
        (asserts! (or (is-eq (get total-bets prediction) u0) (<= amount max-bet))
          ERR-BET-LIMIT-EXCEEDED
        )
      )

      ;; Check daily limits
      (let ((today-spent (if (>= (- stacks-block-height (get last-reset user-limits-data)) u144)
          u0 ;; Reset if more than 24 hours
          (get daily-spent user-limits-data)
        )))
        (asserts! (<= (+ today-spent amount) (get daily-limit user-limits-data))
          ERR-BET-LIMIT-EXCEEDED
        )

        ;; Circuit breaker check
        (if (> amount CIRCUIT-BREAKER-THRESHOLD)
          (var-set circuit-breaker-active true)
          true
        )

        ;; Transfer bet amount to contract
        (unwrap! (stx-transfer? amount tx-sender (as-contract tx-sender))
          ERR-TRANSFER-FAILED
        )

        ;; Record the bet
        (map-set bets {
          prediction-id: prediction-id,
          bettor: tx-sender,
        } {
          amount: amount,
          outcome-id: outcome-id,
          claimed: false,
          timestamp: stacks-block-height,
        })

        ;; Update prediction totals
        (map-set predictions prediction-id
          (merge prediction { total-bets: (+ (get total-bets prediction) amount) })
        )

        ;; Update outcome totals
        (let ((outcome-data (unwrap!
            (map-get? prediction-outcomes {
              prediction-id: prediction-id,
              outcome-id: outcome-id,
            })
            ERR-NOT-FOUND
          )))
          (map-set prediction-outcomes {
            prediction-id: prediction-id,
            outcome-id: outcome-id,
          }
            (merge outcome-data { total-bets: (+ (get total-bets outcome-data) amount) })
          )
        )

        ;; Update user limits
        (map-set user-limits tx-sender
          (merge user-limits-data {
            daily-spent: (+ today-spent amount),
            last-reset: (if (>= (- stacks-block-height (get last-reset user-limits-data)) u144)
              stacks-block-height
              (get last-reset user-limits-data)
            ),
          })
        )

        (ok true)
      )
    )
  )
)

;; Oracle-based Outcome Resolution
(define-public (oracle-resolve-outcome
    (prediction-id uint)
    (outcome-id uint)
    (evidence-hash (buff 32))
  )
  (let ((prediction (unwrap! (map-get? predictions prediction-id) ERR-NOT-FOUND)))
    (begin
      ;; Validations
      (asserts! (>= stacks-block-height (get block-deadline prediction))
        ERR-PREDICTION-NOT-EXPIRED
      )
      (asserts! (is-none (get outcome prediction)) ERR-OUTCOME-ALREADY-SET)

      ;; Check oracle authorization
      (match (get oracle-address prediction)
        oracle-addr (asserts! (is-eq tx-sender oracle-addr) ERR-NOT-AUTHORIZED)
        (asserts! (is-oracle-authorized tx-sender) ERR-ORACLE-NOT-AUTHORIZED)
      )

      (asserts! (< outcome-id (get num-outcomes prediction)) ERR-INVALID-OUTCOME)

      ;; Set outcome with dispute period
      (map-set predictions prediction-id
        (merge prediction {
          outcome: (some outcome-id),
          dispute-end: (some (+ stacks-block-height DISPUTE-PERIOD)),
        })
      )

      ;; Update oracle reputation
      (let ((oracle-info (unwrap! (map-get? oracles tx-sender) ERR-NOT-FOUND)))
        (map-set oracles tx-sender
          (merge oracle-info { total-resolutions: (+ (get total-resolutions oracle-info) u1) })
        )
      )

      (ok true)
    )
  )
)

;; Dispute Resolution System
(define-public (dispute-outcome
    (prediction-id uint)
    (disputed-outcome uint)
    (evidence-hash (buff 32))
    (dispute-stake uint)
  )
  (let (
      (prediction (unwrap! (map-get? predictions prediction-id) ERR-NOT-FOUND))
      (dispute-id (var-get dispute-counter))
    )
    (begin
      ;; Validations
      (asserts! (is-some (get outcome prediction)) ERR-NOT-FOUND)
      (asserts! (is-some (get dispute-end prediction)) ERR-NOT-FOUND)
      (asserts!
        (< stacks-block-height (unwrap-panic (get dispute-end prediction)))
        ERR-DISPUTE-PERIOD-ACTIVE
      )
      (asserts! (>= dispute-stake MIN-STAKE) ERR-INVALID-STAKE)

      ;; Transfer dispute stake
      (unwrap! (stx-transfer? dispute-stake tx-sender (as-contract tx-sender))
        ERR-TRANSFER-FAILED
      )

      ;; Create dispute
      (map-set disputes dispute-id {
        disputer: tx-sender,
        disputed-outcome: disputed-outcome,
        evidence-hash: evidence-hash,
        dispute-stake: dispute-stake,
        votes-for: u0,
        votes-against: u0,
        resolved: false,
      })

      (var-set dispute-counter (+ dispute-id u1))
      (ok dispute-id)
    )
  )
)

;; Multi-signature Proposal System
(define-public (create-multisig-proposal
    (proposal-type uint)
    (target-id uint)
    (proposed-value uint)
  )
  (let ((proposal-id (var-get proposal-counter)))
    (begin
      ;; Check if sender is multisig member
      (asserts! (is-some (index-of (var-get multisig-members) tx-sender))
        ERR-NOT-AUTHORIZED
      )

      (map-set multisig-proposals proposal-id {
        proposal-type: proposal-type,
        target-id: target-id,
        proposed-value: proposed-value,
        proposer: tx-sender,
        signatures: (list tx-sender),
        executed: false,
        created-at: stacks-block-height,
      })

      (var-set proposal-counter (+ proposal-id u1))
      (ok proposal-id)
    )
  )
)

(define-public (sign-multisig-proposal (proposal-id uint))
  (let ((proposal (unwrap! (map-get? multisig-proposals proposal-id) ERR-NOT-FOUND)))
    (begin
      ;; Check if sender is multisig member and hasn't signed yet
      (asserts! (is-some (index-of (var-get multisig-members) tx-sender))
        ERR-NOT-AUTHORIZED
      )
      (asserts! (is-none (index-of (get signatures proposal) tx-sender))
        ERR-INVALID-MULTISIG
      )
      (asserts! (not (get executed proposal)) ERR-INVALID-MULTISIG)

      ;; Add signature
      (let ((new-signatures (unwrap! (as-max-len? (append (get signatures proposal) tx-sender) u10)
          ERR-INVALID-MULTISIG
        )))
        (map-set multisig-proposals proposal-id
          (merge proposal { signatures: new-signatures })
        )

        ;; Execute if enough signatures
        (if (>= (len new-signatures) (var-get required-signatures))
          (execute-multisig-proposal proposal-id)
          (ok true)
        )
      )
    )
  )
)

;; Execute multisig proposal (simplified implementation)
(define-private (execute-multisig-proposal (proposal-id uint))
  (let ((proposal (unwrap! (map-get? multisig-proposals proposal-id) ERR-NOT-FOUND)))
    (begin
      (map-set multisig-proposals proposal-id (merge proposal { executed: true }))
      ;; Implementation would depend on proposal type
      (ok true)
    )
  )
)

;; Enhanced Claim Winnings with Insurance
(define-public (claim-winnings-with-insurance (prediction-id uint))
  (let (
      (prediction (unwrap! (map-get? predictions prediction-id) ERR-NOT-FOUND))
      (user-bet (unwrap!
        (map-get? bets {
          prediction-id: prediction-id,
          bettor: tx-sender,
        })
        ERR-NOT-FOUND
      ))
    )
    (begin
      ;; Validations
      (asserts! (get is-settled prediction) ERR-PREDICTION-NOT-EXPIRED)
      (asserts! (not (get claimed user-bet)) ERR-ALREADY-CLAIMED)
      (asserts! (is-none (get dispute-end prediction)) ERR-DISPUTE-PERIOD-ACTIVE)

      ;; Check if user won
      (match (get outcome prediction)
        outcome-val (if (is-eq (get outcome-id user-bet) outcome-val)
          (let (
              (winner-outcome (unwrap!
                (map-get? prediction-outcomes {
                  prediction-id: prediction-id,
                  outcome-id: outcome-val,
                })
                ERR-NOT-FOUND
              ))
              (winner-pool (get total-bets winner-outcome))
              (total-pool (get total-bets prediction))
              (user-amount (get amount user-bet))
              (winnings (if (> winner-pool u0)
                (/ (* user-amount total-pool) winner-pool)
                u0
              ))
              (fee (/ (* winnings (var-get contract-fee)) u1000))
              (net-winnings (- winnings fee))
            )
            (if (> net-winnings u0)
              (begin
                ;; Mark as claimed
                (map-set bets {
                  prediction-id: prediction-id,
                  bettor: tx-sender,
                }
                  (merge user-bet { claimed: true })
                )

                ;; Update user reputation
                (let ((user-rep (get-user-reputation tx-sender)))
                  (map-set user-reputation tx-sender
                    (merge user-rep {
                      correct-predictions: (+ (get correct-predictions user-rep) u1),
                      total-volume: (+ (get total-volume user-rep) user-amount),
                      score: (calculate-reputation-score
                        (+ (get correct-predictions user-rep) u1)
                        (get total-predictions user-rep)
                      ),
                    })
                  )
                )

                ;; Transfer winnings
                (unwrap!
                  (as-contract (stx-transfer? net-winnings tx-sender tx-sender))
                  ERR-TRANSFER-FAILED
                )
                (ok net-winnings)
              )
              ERR-NO-WINNINGS
            )
          )
          ;; User lost - check insurance eligibility
          (let ((insurance-payout (/ (get insurance-pool prediction) (get num-outcomes prediction))))
            (if (and (> insurance-payout u0) (>= (get score (get-user-reputation tx-sender)) u500))
              (begin
                (map-set bets {
                  prediction-id: prediction-id,
                  bettor: tx-sender,
                }
                  (merge user-bet { claimed: true })
                )
                (unwrap!
                  (as-contract (stx-transfer? insurance-payout tx-sender tx-sender))
                  ERR-TRANSFER-FAILED
                )
                (ok insurance-payout)
              )
              ERR-NO-WINNINGS
            )
          )
        )
        ERR-NOT-FOUND
      )
    )
  )
)

;; Emergency Functions
(define-public (emergency-pause)
  (begin
    (asserts!
      (or
        (is-eq tx-sender (var-get contract-owner))
        (is-some (index-of (var-get multisig-members) tx-sender))
      )
      ERR-NOT-AUTHORIZED
    )
    (var-set is-paused true)
    (var-set circuit-breaker-active true)
    (ok true)
  )
)

(define-public (reset-circuit-breaker)
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    (var-set circuit-breaker-active false)
    (ok true)
  )
)

;; Insurance Fund Management
(define-public (contribute-to-insurance (amount uint))
  (begin
    (unwrap! (stx-transfer? amount tx-sender (as-contract tx-sender))
      ERR-TRANSFER-FAILED
    )
    (var-set insurance-fund (+ (var-get insurance-fund) amount))
    (ok true)
  )
)

(define-read-only (get-insurance-fund-balance)
  (var-get insurance-fund)
)

;; Legacy compatibility functions
(define-public (create-prediction
    (block-deadline uint)
    (condition-hash (buff 32))
    (stake uint)
  )
  (create-multi-outcome-prediction block-deadline condition-hash stake
    (list "Yes" "No") none u0
  )
)

(define-public (place-bet
    (prediction-id uint)
    (bet-on bool)
    (amount uint)
  )
  (place-bet-with-limits prediction-id (if bet-on
    u0
    u1
  ) amount
  )
)

(define-public (reveal-outcome
    (id uint)
    (outcome bool)
  )
  (oracle-resolve-outcome id (if outcome
    u0
    u1
  ) 0x00
  )
)

(define-public (claim-winnings (prediction-id uint))
  (claim-winnings-with-insurance prediction-id)
)

(define-public (get-prediction (id uint))
  (match (map-get? predictions id)
    pred (ok pred)
    ERR-NOT-FOUND
  )
)

(define-public (get-current-id)
  (ok (var-get prediction-counter))
)

(define-public (refund-creator (prediction-id uint))
  (let ((prediction (unwrap! (map-get? predictions prediction-id) ERR-NOT-FOUND)))
    (begin
      (asserts! (is-eq (get creator prediction) tx-sender) ERR-NOT-AUTHORIZED)
      (asserts! (is-eq (get total-bets prediction) u0) ERR-INVALID-BET)
      (unwrap!
        (as-contract (stx-transfer? (get stake prediction) tx-sender tx-sender))
        ERR-TRANSFER-FAILED
      )
      (ok true)
    )
  )
)
