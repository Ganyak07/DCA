;; sBTC Dollar-Cost Averaging (DCA) Smart Contract
;; This contract allows users to automatically purchase sBTC at regular intervals

;; =================
;; CONSTANTS & ERRORS
;; =================

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-AMOUNT (err u101))
(define-constant ERR-INVALID-FREQUENCY (err u102))
(define-constant ERR-SCHEDULE-NOT-FOUND (err u103))
(define-constant ERR-SCHEDULE-NOT-ACTIVE (err u104))
(define-constant ERR-EXECUTION-TOO-EARLY (err u105))
(define-constant ERR-INSUFFICIENT-BALANCE (err u106))
(define-constant ERR-SWAP-FAILED (err u107))
(define-constant ERR-TRANSFER-FAILED (err u108))

;; Contract constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant MIN-FREQUENCY u144) ;; Minimum 1 day in blocks (144 blocks ≈ 24 hours)
(define-constant MIN-AMOUNT u1000000) ;; Minimum 1 STX (in microSTX)
(define-constant FEE-RATE u50) ;; 0.5% fee (50/10000)

;; Token contracts (replace with actual addresses)
(define-constant STX-TOKEN 'ST1HTBVD3JG9C05J7HBJTHGR0GGW7KXW28M5JS8QE.stx-token)
(define-constant SBTC-TOKEN 'ST1HTBVD3JG9C05J7HBJTHGR0GGW7KXW28M5JS8QE.sbtc-token)
(define-constant DEX-CONTRACT 'ST1HTBVD3JG9C05J7HBJTHGR0GGW7KXW28M5JS8QE.alex-dex)

;; =================
;; DATA STRUCTURES
;; =================

;; User DCA schedule
(define-map user-schedules
    { user: principal }
    {
        amount-per-purchase: uint,
        frequency-blocks: uint,
        next-execution-block: uint,
        total-deposited: uint,
        total-purchased: uint,
        sbtc-accumulated: uint,
        active: bool,
        created-at: uint
    }
)

;; User STX deposits for DCA
(define-map user-balances
    { user: principal }
    { stx-balance: uint }
)

;; Contract statistics
(define-data-var total-users uint u0)
(define-data-var total-stx-processed uint u0)
(define-data-var total-sbtc-purchased uint u0)
(define-data-var contract-fees-collected uint u0)

;; =================
;; READ-ONLY FUNCTIONS
;; =================

;; Get user's DCA schedule
(define-read-only (get-user-schedule (user principal))
    (map-get? user-schedules { user: user })
)

;; Get user's STX balance in contract
(define-read-only (get-user-balance (user principal))
    (default-to u0 (get stx-balance (map-get? user-balances { user: user })))
)

;; Check if user can execute DCA (enough balance and time passed)
(define-read-only (can-execute-dca (user principal))
    (let (
        (schedule (map-get? user-schedules { user: user }))
        (balance (get-user-balance user))
    )
    (match schedule
        schedule-data
        (and 
            (get active schedule-data)
            (>= block-height (get next-execution-block schedule-data))
            (>= balance (get amount-per-purchase schedule-data))
        )
        false
    ))
)

;; Get contract statistics
(define-read-only (get-contract-stats)
    {
        total-users: (var-get total-users),
        total-stx-processed: (var-get total-stx-processed),
        total-sbtc-purchased: (var-get total-sbtc-purchased),
        fees-collected: (var-get contract-fees-collected)
    }
)

;; Calculate fee for given amount
(define-read-only (calculate-fee (amount uint))
    (/ (* amount FEE-RATE) u10000)
)

;; =================
;; PRIVATE FUNCTIONS
;; =================

;; Update user balance
(define-private (update-user-balance (user principal) (new-balance uint))
    (map-set user-balances
        { user: user }
        { stx-balance: new-balance }
    )
)

;; Simulate DEX swap (replace with actual DEX integration)
(define-private (swap-stx-to-sbtc (amount uint))
    ;; This is a simplified swap simulation
    ;; In reality, you'd integrate with ALEX or another DEX
    ;; For demo purposes, assume 1 STX = 0.00001 sBTC (adjust based on actual rates)
    (let ((sbtc-amount (/ amount u100000))) ;; Simplified conversion rate
        (ok sbtc-amount)
    )
)

;; =================
;; PUBLIC FUNCTIONS
;; =================

;; Create a new DCA schedule
(define-public (create-dca-schedule (amount-per-purchase uint) (frequency-blocks uint))
    (let (
        (user tx-sender)
        (existing-schedule (map-get? user-schedules { user: user }))
    )
    (begin
        ;; Validate inputs
        (asserts! (>= amount-per-purchase MIN-AMOUNT) ERR-INVALID-AMOUNT)
        (asserts! (>= frequency-blocks MIN-FREQUENCY) ERR-INVALID-FREQUENCY)
        
        ;; Check if user already has an active schedule
        (match existing-schedule
            schedule-data
            (asserts! (not (get active schedule-data)) ERR-NOT-AUTHORIZED)
            true
        )
        
        ;; Create new schedule
        (map-set user-schedules
            { user: user }
            {
                amount-per-purchase: amount-per-purchase,
                frequency-blocks: frequency-blocks,
                next-execution-block: (+ block-height frequency-blocks),
                total-deposited: u0,
                total-purchased: u0,
                sbtc-accumulated: u0,
                active: true,
                created-at: block-height
            }
        )
        
        ;; Initialize user balance if needed
        (if (is-none (map-get? user-balances { user: user }))
            (begin
                (update-user-balance user u0)
                (var-set total-users (+ (var-get total-users) u1))
            )
            true
        )
        
        (ok true)
    ))
)

;; Deposit STX for DCA purchases
(define-public (deposit-stx (amount uint))
    (let (
        (user tx-sender)
        (current-balance (get-user-balance user))
    )
    (begin
        ;; Validate amount
        (asserts! (> amount u0) ERR-INVALID-AMOUNT)
        
        ;; Transfer STX from user to contract
        (try! (stx-transfer? amount user (as-contract tx-sender)))
        
        ;; Update user balance
        (update-user-balance user (+ current-balance amount))
        
        (ok true)
    ))
)

;; Execute DCA purchase for a user
(define-public (execute-dca (user principal))
    (let (
        (schedule (unwrap! (map-get? user-schedules { user: user }) ERR-SCHEDULE-NOT-FOUND))
        (current-balance (get-user-balance user))
        (purchase-amount (get amount-per-purchase schedule))
        (fee-amount (calculate-fee purchase-amount))
        (net-purchase-amount (- purchase-amount fee-amount))
    )
    (begin
        ;; Validate execution conditions
        (asserts! (get active schedule) ERR-SCHEDULE-NOT-ACTIVE)
        (asserts! (>= block-height (get next-execution-block schedule)) ERR-EXECUTION-TOO-EARLY)
        (asserts! (>= current-balance purchase-amount) ERR-INSUFFICIENT-BALANCE)
        
        ;; Perform swap (simplified for demo)
        (match (swap-stx-to-sbtc net-purchase-amount)
            ok-value
            (begin
                ;; Update user balance
                (update-user-balance user (- current-balance purchase-amount))
                
                ;; Update schedule
                (map-set user-schedules
                    { user: user }
                    (merge schedule {
                        next-execution-block: (+ block-height (get frequency-blocks schedule)),
                        total-purchased: (+ (get total-purchased schedule) net-purchase-amount),
                        sbtc-accumulated: (+ (get sbtc-accumulated schedule) ok-value)
                    })
                )
                
                ;; Update contract stats
                (var-set total-stx-processed (+ (var-get total-stx-processed) purchase-amount))
                (var-set total-sbtc-purchased (+ (var-get total-sbtc-purchased) ok-value))
                (var-set contract-fees-collected (+ (var-get contract-fees-collected) fee-amount))
                
                (ok ok-value)
            )
            err-value
            (err ERR-SWAP-FAILED)
        )
    ))
)

;; Cancel DCA schedule
(define-public (cancel-dca-schedule)
    (let (
        (user tx-sender)
        (schedule (unwrap! (map-get? user-schedules { user: user }) ERR-SCHEDULE-NOT-FOUND))
    )
    (begin
        ;; Verify user owns the schedule
        (asserts! (get active schedule) ERR-SCHEDULE-NOT-ACTIVE)
        
        ;; Deactivate schedule
        (map-set user-schedules
            { user: user }
            (merge schedule { active: false })
        )
        
        (ok true)
    ))
)

;; Withdraw accumulated sBTC
(define-public (withdraw-sbtc)
    (let (
        (user tx-sender)
        (schedule (unwrap! (map-get? user-schedules { user: user }) ERR-SCHEDULE-NOT-FOUND))
        (sbtc-amount (get sbtc-accumulated schedule))
    )
    (begin
        ;; Check if user has sBTC to withdraw
        (asserts! (> sbtc-amount u0) ERR-INSUFFICIENT-BALANCE)
        
        ;; Transfer sBTC to user (simplified - in reality use actual sBTC token)
        ;; (try! (contract-call? SBTC-TOKEN transfer sbtc-amount (as-contract tx-sender) user none))
        
        ;; Update schedule to reset accumulated sBTC
        (map-set user-schedules
            { user: user }
            (merge schedule { sbtc-accumulated: u0 })
        )
        
        (ok sbtc-amount)
    ))
)

;; Withdraw remaining STX balance
(define-public (withdraw-stx (amount uint))
    (let (
        (user tx-sender)
        (current-balance (get-user-balance user))
    )
    (begin
        ;; Validate withdrawal amount
        (asserts! (<= amount current-balance) ERR-INSUFFICIENT-BALANCE)
        (asserts! (> amount u0) ERR-INVALID-AMOUNT)
        
        ;; Transfer STX back to user
        (try! (as-contract (stx-transfer? amount tx-sender user)))
        
        ;; Update balance
        (update-user-balance user (- current-balance amount))
        
        (ok true)
    ))
)

;; Admin function to collect fees (only contract owner)
(define-public (collect-fees)
    (let ((fees (var-get contract-fees-collected)))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (asserts! (> fees u0) ERR-INSUFFICIENT-BALANCE)
        
        ;; Transfer collected fees to owner
        (try! (as-contract (stx-transfer? fees tx-sender CONTRACT-OWNER)))
        
        ;; Reset fee counter
        (var-set contract-fees-collected u0)
        
        (ok fees)
    ))
)
