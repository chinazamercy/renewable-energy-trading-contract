;; Peer-to-Peer Energy Trading Smart Contract
;; Decentralized renewable energy marketplace

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u700))
(define-constant ERR_PRODUCER_NOT_FOUND (err u701))
(define-constant ERR_CONSUMER_NOT_FOUND (err u702))
(define-constant ERR_INSUFFICIENT_ENERGY (err u703))
(define-constant ERR_INSUFFICIENT_PAYMENT (err u704))
(define-constant ERR_TRADE_NOT_FOUND (err u705))
(define-constant ERR_INVALID_CAPACITY (err u706))

;; Data Variables
(define-data-var trade-id-nonce uint u0)
(define-data-var producer-id-nonce uint u0)
(define-data-var consumer-id-nonce uint u0)
(define-data-var base-energy-price uint u100000) ;; 0.1 STX per kWh
(define-data-var platform-fee-percentage uint u5) ;; 5%
(define-data-var contract-paused bool false)

;; Data Structures
(define-map energy-producers
  { producer: principal }
  {
    producer-id: uint,
    name: (string-utf8 128),
    location: (string-utf8 256),
    energy-type: (string-ascii 50),
    capacity-kwh: uint,
    available-energy: uint,
    price-per-kwh: uint,
    reputation-score: uint,
    total-sales: uint,
    registration-date: uint,
    verified: bool
  }
)

(define-map energy-consumers
  { consumer: principal }
  {
    consumer-id: uint,
    name: (string-utf8 128),
    location: (string-utf8 256),
    demand-kwh: uint,
    max-price-per-kwh: uint,
    energy-preference: (string-ascii 50),
    total-purchases: uint,
    registration-date: uint
  }
)

(define-map energy-trades
  { trade-id: uint }
  {
    producer: principal,
    consumer: principal,
    energy-amount: uint,
    price-per-kwh: uint,
    total-cost: uint,
    trade-date: uint,
    delivery-date: uint,
    status: (string-ascii 20),
    energy-type: (string-ascii 50),
    carbon-offset: uint
  }
)

(define-map energy-certificates
  { certificate-id: uint }
  {
    producer: principal,
    energy-amount: uint,
    generation-date: uint,
    energy-source: (string-ascii 50),
    carbon-offset: uint,
    verified: bool,
    used: bool
  }
)

(define-map market-orders
  { order-id: uint }
  {
    creator: principal,
    order-type: (string-ascii 10), ;; "buy" or "sell"
    energy-amount: uint,
    price-per-kwh: uint,
    energy-type: (string-ascii 50),
    expiry-block: uint,
    filled: bool,
    created-block: uint
  }
)

;; Read-only functions
(define-read-only (get-producer (producer principal))
  (map-get? energy-producers { producer: producer })
)

(define-read-only (get-consumer (consumer principal))
  (map-get? energy-consumers { consumer: consumer })
)

(define-read-only (get-trade (trade-id uint))
  (map-get? energy-trades { trade-id: trade-id })
)

(define-read-only (get-energy-certificate (certificate-id uint))
  (map-get? energy-certificates { certificate-id: certificate-id })
)

(define-read-only (calculate-trade-cost (energy-amount uint) (price-per-kwh uint))
  (let (
    (base-cost (* energy-amount price-per-kwh))
    (platform-fee (/ (* base-cost (var-get platform-fee-percentage)) u100))
  )
  (+ base-cost platform-fee)
  )
)

(define-read-only (get-market-price)
  (var-get base-energy-price)
)

;; Private functions
(define-private (increment-trade-id)
  (begin
  (var-set trade-id-nonce (+ (var-get trade-id-nonce) u1))
  (var-get trade-id-nonce)
  )
)

(define-private (increment-producer-id)
  (begin
  (var-set producer-id-nonce (+ (var-get producer-id-nonce) u1))
  (var-get producer-id-nonce)
  )
)

(define-private (increment-consumer-id)
  (begin
  (var-set consumer-id-nonce (+ (var-get consumer-id-nonce) u1))
  (var-get consumer-id-nonce)
  )
)

;; Public functions
(define-public (register-producer
  (name (string-utf8 128))
  (location (string-utf8 256))
  (energy-type (string-ascii 50))
  (capacity-kwh uint)
  (price-per-kwh uint)
  )
  (let (
    (producer-id (increment-producer-id))
  )
  (asserts! (not (var-get contract-paused)) (err u999))
  (asserts! (> capacity-kwh u0) ERR_INVALID_CAPACITY)
  
  (map-set energy-producers
    { producer: tx-sender }
    {
      producer-id: producer-id,
      name: name,
      location: location,
      energy-type: energy-type,
      capacity-kwh: capacity-kwh,
      available-energy: capacity-kwh,
      price-per-kwh: price-per-kwh,
      reputation-score: u100,
      total-sales: u0,
      registration-date: block-height,
      verified: false
    }
  )
  
  (ok producer-id)
  )
)

(define-public (register-consumer
  (name (string-utf8 128))
  (location (string-utf8 256))
  (demand-kwh uint)
  (max-price-per-kwh uint)
  (energy-preference (string-ascii 50))
  )
  (let (
    (consumer-id (increment-consumer-id))
  )
  (asserts! (not (var-get contract-paused)) (err u999))
  
  (map-set energy-consumers
    { consumer: tx-sender }
    {
      consumer-id: consumer-id,
      name: name,
      location: location,
      demand-kwh: demand-kwh,
      max-price-per-kwh: max-price-per-kwh,
      energy-preference: energy-preference,
      total-purchases: u0,
      registration-date: block-height
    }
  )
  
  (ok consumer-id)
  )
)

(define-public (create-energy-trade
  (producer principal)
  (energy-amount uint)
  (delivery-date uint)
  )
  (let (
    (trade-id (increment-trade-id))
    (producer-info (unwrap! (get-producer producer) ERR_PRODUCER_NOT_FOUND))
    (consumer-info (unwrap! (get-consumer tx-sender) ERR_CONSUMER_NOT_FOUND))
    (price-per-kwh (get price-per-kwh producer-info))
    (total-cost (calculate-trade-cost energy-amount price-per-kwh))
  )
  (asserts! (not (var-get contract-paused)) (err u999))
  (asserts! (>= (get available-energy producer-info) energy-amount) ERR_INSUFFICIENT_ENERGY)
  (asserts! (>= (stx-get-balance tx-sender) total-cost) ERR_INSUFFICIENT_PAYMENT)
  (asserts! (<= price-per-kwh (get max-price-per-kwh consumer-info)) (err u707))
  
  ;; Transfer payment to escrow
  (try! (stx-transfer? total-cost tx-sender (as-contract tx-sender)))
  
  ;; Create trade record
  (map-set energy-trades
    { trade-id: trade-id }
    {
      producer: producer,
      consumer: tx-sender,
      energy-amount: energy-amount,
      price-per-kwh: price-per-kwh,
      total-cost: total-cost,
      trade-date: block-height,
      delivery-date: delivery-date,
      status: "pending",
      energy-type: (get energy-type producer-info),
      carbon-offset: (* energy-amount u2) ;; 2kg CO2 offset per kWh
    }
  )
  
  ;; Update producer available energy
  (map-set energy-producers
    { producer: producer }
    (merge producer-info {
      available-energy: (- (get available-energy producer-info) energy-amount)
    })
  )
  
  (ok trade-id)
  )
)

(define-public (confirm-delivery (trade-id uint))
  (let (
    (trade-info (unwrap! (get-trade trade-id) ERR_TRADE_NOT_FOUND))
  )
  (asserts! (is-eq tx-sender (get producer trade-info)) ERR_NOT_AUTHORIZED)
  (asserts! (is-eq (get status trade-info) "pending") (err u708))
  
  ;; Update trade status
  (map-set energy-trades
    { trade-id: trade-id }
    (merge trade-info { status: "delivered" })
  )
  
  ;; Release payment to producer
  (let (
    (producer-payment (- (get total-cost trade-info) 
                        (/ (* (get total-cost trade-info) (var-get platform-fee-percentage)) u100)))
    (platform-fee (/ (* (get total-cost trade-info) (var-get platform-fee-percentage)) u100))
  )
  (try! (as-contract (stx-transfer? producer-payment tx-sender (get producer trade-info))))
  (try! (as-contract (stx-transfer? platform-fee tx-sender CONTRACT_OWNER)))
  )
  
  ;; Update producer stats
  (match (get-producer (get producer trade-info))
    producer-info
      (map-set energy-producers
        { producer: (get producer trade-info) }
        (merge producer-info {
          total-sales: (+ (get total-sales producer-info) (get energy-amount trade-info)),
          reputation-score: (+ (get reputation-score producer-info) u5)
        })
      )
    false
  )
  
  (ok true)
  )
)

(define-public (update-energy-availability (new-available uint))
  (let (
    (producer-info (unwrap! (get-producer tx-sender) ERR_PRODUCER_NOT_FOUND))
  )
  (asserts! (<= new-available (get capacity-kwh producer-info)) ERR_INVALID_CAPACITY)
  
  (map-set energy-producers
    { producer: tx-sender }
    (merge producer-info { available-energy: new-available })
  )
  
  (ok true)
  )
)

(define-public (set-energy-price (new-price uint))
  (let (
    (producer-info (unwrap! (get-producer tx-sender) ERR_PRODUCER_NOT_FOUND))
  )
  (map-set energy-producers
    { producer: tx-sender }
    (merge producer-info { price-per-kwh: new-price })
  )
  
  (ok true)
  )
)

;; Admin functions
(define-public (verify-producer (producer principal))
  (begin
  (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
  
  (match (get-producer producer)
    producer-info
      (map-set energy-producers
        { producer: producer }
        (merge producer-info { verified: true })
      )
    false
  )
  
  (ok true)
  )
)

(define-public (set-base-price (new-price uint))
  (begin
  (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
  (var-set base-energy-price new-price)
  (ok true)
  )
)

(define-public (pause-contract)
  (begin
  (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
  (var-set contract-paused true)
  (ok true)
  )
)


;; title: peer-to-peer-energy-trading
;; version:
;; summary:
;; description:

;; traits
;;

;; token definitions
;;

;; constants
;;

;; data vars
;;

;; data maps
;;

;; public functions
;;

;; read only functions
;;

;; private functions
;;

