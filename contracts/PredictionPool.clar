;; Bitcoin Prediction Market
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

;; Contract Constants
(define-constant MIN-STAKE u1000000) ;; 1 STX minimum
(define-constant MIN-BET u100000) ;; 0.1 STX minimum

;; Data Maps
(define-map predictions
  uint
  {
    creator: principal,
    block-deadline: uint,
    condition: (buff 32), ;; pre-agreed hash of condition
    stake: uint,
    outcome: (optional bool),
    created-at: uint, ;; block height when created
    total-bets: uint, ;; total amount bet on this prediction
    total-yes-bets: uint,
    total-no-bets: uint,
    is-settled: bool,
  }
)

(define-map bets
  {
    prediction-id: uint,
    bettor: principal,
  }
  {
    amount: uint,
    bet-on: bool, ;; true or false prediction
    claimed: bool, ;; whether winnings have been claimed
  }
)

;; Data Variables
(define-data-var prediction-counter uint u0)
(define-data-var contract-fee uint u50) ;; 5% fee (out of 1000)

;; Read-only Functions
(define-read-only (get-prediction-readonly (id uint))
  (map-get? predictions id)
)

(define-read-only (get-current-id-readonly)
  (var-get prediction-counter)
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

(define-read-only (get-contract-fee)
  (var-get contract-fee)
)

;; Public Functions
(define-public (create-prediction
    (block-deadline uint)
    (condition-hash (buff 32))
    (stake uint)
  )
  (let ((id (var-get prediction-counter)))
    (begin
      ;; Validations
      (asserts! (> block-deadline stacks-block-height) ERR-INVALID-DEADLINE)
      (asserts! (>= stake MIN-STAKE) ERR-INVALID-STAKE)
      (asserts! (is-eq (len condition-hash) u32) ERR-INVALID-BET)
      ;; Update prediction counter
      (var-set prediction-counter (+ id u1))
      ;; Transfer stake to contract
      (unwrap! (stx-transfer? stake tx-sender (as-contract tx-sender))
        ERR-TRANSFER-FAILED
      )
      ;; Create prediction with all required fields
      (map-set predictions id {
        creator: tx-sender,
        block-deadline: block-deadline,
        condition: condition-hash,
        stake: stake,
        outcome: none,
        created-at: stacks-block-height,
        total-bets: u0,
        total-yes-bets: u0,
        total-no-bets: u0,
        is-settled: false,
      })
      (ok id)
    )
  )
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

(define-public (place-bet
    (prediction-id uint)
    (bet-on bool)
    (amount uint)
  )
  (let ((prediction (unwrap! (map-get? predictions prediction-id) ERR-NOT-FOUND)))
    (begin
      ;; Validations
      (asserts! (>= amount MIN-BET) ERR-INVALID-BET)
      (asserts! (< stacks-block-height (get block-deadline prediction))
        ERR-PREDICTION-EXPIRED
      )
      (asserts! (is-none (get outcome prediction)) ERR-BETTING-CLOSED)
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
        bet-on: bet-on,
        claimed: false,
      })
      ;; Update prediction totals
      (map-set predictions prediction-id
        (merge prediction {
          total-bets: (+ (get total-bets prediction) amount),
          total-yes-bets: (if bet-on
            (+ (get total-yes-bets prediction) amount)
            (get total-yes-bets prediction)
          ),
          total-no-bets: (if bet-on
            (get total-no-bets prediction)
            (+ (get total-no-bets prediction) amount)
          ),
        })
      )
      (ok true)
    )
  )
)

(define-public (reveal-outcome
    (id uint)
    (outcome bool)
  )
  (let ((prediction (map-get? predictions id)))
    (if (is-some prediction)
      (let ((pred (unwrap! prediction ERR-NOT-FOUND)))
        (begin
          ;; Validations
          (asserts! (is-eq (get creator pred) tx-sender) ERR-NOT-AUTHORIZED)
          (asserts! (>= stacks-block-height (get block-deadline pred))
            ERR-PREDICTION-NOT-EXPIRED
          )
          (asserts! (is-none (get outcome pred)) ERR-OUTCOME-ALREADY-SET)
          ;; Update prediction with outcome using explicit field mapping
          (map-set predictions id {
            creator: (get creator pred),
            block-deadline: (get block-deadline pred),
            condition: (get condition pred),
            stake: (get stake pred),
            outcome: (some outcome),
            created-at: (get created-at pred),
            total-bets: (get total-bets pred),
            total-yes-bets: (get total-yes-bets pred),
            total-no-bets: (get total-no-bets pred),
            is-settled: true,
          })
          (ok true)
        )
      )
      ERR-NOT-FOUND
    )
  )
)

(define-public (claim-winnings (prediction-id uint))
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
      ;; Check if user won
      (match (get outcome prediction)
        outcome-val
        (if (is-eq (get bet-on user-bet) outcome-val)
          (let (
              (winner-pool (if outcome-val
                (get total-yes-bets prediction)
                (get total-no-bets prediction)
              ))
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
          ERR-NO-WINNINGS ;; User lost
        )
        ERR-NOT-FOUND ;; No outcome set
      )
    )
  )
)

(define-public (refund-creator (prediction-id uint))
  (let ((prediction (unwrap! (map-get? predictions prediction-id) ERR-NOT-FOUND)))
    (begin
      ;; Only creator can refund and only if no bets were placed
      (asserts! (is-eq (get creator prediction) tx-sender) ERR-NOT-AUTHORIZED)
      (asserts! (is-eq (get total-bets prediction) u0) ERR-INVALID-BET)
      ;; Transfer stake back to creator
      (unwrap!
        (as-contract (stx-transfer? (get stake prediction) tx-sender tx-sender))
        ERR-TRANSFER-FAILED
      )
      (ok true)
    )
  )
)
