;; Smart Grid Management Smart Contract
;; Intelligent grid load balancing and distribution system

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u800))
(define-constant ERR_GRID_NODE_NOT_FOUND (err u801))
(define-constant ERR_INVALID_LOAD (err u802))
(define-constant ERR_GRID_OVERLOAD (err u803))
(define-constant ERR_STORAGE_FULL (err u804))
(define-constant ERR_INSUFFICIENT_STORAGE (err u805))

;; Data Variables
(define-data-var grid-node-id-nonce uint u0)
(define-data-var storage-unit-id-nonce uint u0)
(define-data-var total-grid-capacity uint u1000000) ;; 1M kWh
(define-data-var current-grid-load uint u0)
(define-data-var emergency-threshold uint u90) ;; 90% capacity
(define-data-var optimal-load-percentage uint u75) ;; 75% capacity

;; Data Structures
(define-map grid-nodes
  { node-id: uint }
  {
    operator: principal,
    location: (string-utf8 256),
    capacity-kwh: uint,
    current-load: uint,
    energy-type: (string-ascii 50),
    status: (string-ascii 20),
    last-maintenance: uint,
    efficiency-rating: uint,
    connection-date: uint
  }
)

(define-map energy-storage-units
  { storage-id: uint }
  {
    operator: principal,
    location: (string-utf8 256),
    capacity-kwh: uint,
    stored-energy: uint,
    charge-rate: uint,
    discharge-rate: uint,
    efficiency: uint,
    status: (string-ascii 20),
    installation-date: uint
  }
)

(define-map grid-transactions
  { transaction-id: uint }
  {
    from-node: uint,
    to-node: uint,
    energy-amount: uint,
    transaction-date: uint,
    transaction-type: (string-ascii 30),
    cost: uint,
    completed: bool
  }
)

(define-map load-predictions
  { prediction-id: uint }
  {
    target-node: uint,
    predicted-load: uint,
    prediction-date: uint,
    prediction-period: uint,
    accuracy-score: (optional uint),
    actual-load: (optional uint)
  }
)

(define-map maintenance-schedules
  { schedule-id: uint }
  {
    node-id: uint,
    maintenance-type: (string-ascii 50),
    scheduled-date: uint,
    estimated-duration: uint,
    priority-level: uint,
    completed: bool,
    cost: uint
  }
)

;; Read-only functions
(define-read-only (get-grid-node (node-id uint))
  (map-get? grid-nodes { node-id: node-id })
)

(define-read-only (get-storage-unit (storage-id uint))
  (map-get? energy-storage-units { storage-id: storage-id })
)

(define-read-only (get-grid-status)
  {
    total-capacity: (var-get total-grid-capacity),
    current-load: (var-get current-grid-load),
    load-percentage: (/ (* (var-get current-grid-load) u100) (var-get total-grid-capacity)),
    status: (if (> (var-get current-grid-load) 
                  (/ (* (var-get total-grid-capacity) (var-get emergency-threshold)) u100))
               "emergency"
               "normal")
  }
)

(define-read-only (calculate-grid-efficiency)
  (let (
    (load-ratio (/ (* (var-get current-grid-load) u100) (var-get total-grid-capacity)))
    (optimal-ratio (var-get optimal-load-percentage))
  )
  (if (<= load-ratio optimal-ratio)
    load-ratio
    (- u200 load-ratio) ;; Efficiency decreases after optimal point
  )
  )
)

(define-read-only (get-node-utilization (node-id uint))
  (match (get-grid-node node-id)
    node-info
      (some (/ (* (get current-load node-info) u100) (get capacity-kwh node-info)))
    none
  )
)

;; Private functions
(define-private (increment-node-id)
  (begin
  (var-set grid-node-id-nonce (+ (var-get grid-node-id-nonce) u1))
  (var-get grid-node-id-nonce)
  )
)

(define-private (increment-storage-id)
  (begin
  (var-set storage-unit-id-nonce (+ (var-get storage-unit-id-nonce) u1))
  (var-get storage-unit-id-nonce)
  )
)

(define-private (update-grid-load (load-change int))
  (let (
    (current-load (var-get current-grid-load))
    (new-load (if (> load-change 0)
                 (+ current-load (to-uint load-change))
                 (- current-load (to-uint (- load-change)))))
  )
  (var-set current-grid-load new-load)
  true
  )
)

;; Public functions
(define-public (register-grid-node
  (location (string-utf8 256))
  (capacity-kwh uint)
  (energy-type (string-ascii 50))
  (efficiency-rating uint)
  )
  (let (
    (node-id (increment-node-id))
  )
  (asserts! (> capacity-kwh u0) ERR_INVALID_LOAD)
  (asserts! (<= efficiency-rating u100) (err u806))
  
  (map-set grid-nodes
    { node-id: node-id }
    {
      operator: tx-sender,
      location: location,
      capacity-kwh: capacity-kwh,
      current-load: u0,
      energy-type: energy-type,
      status: "online",
      last-maintenance: block-height,
      efficiency-rating: efficiency-rating,
      connection-date: block-height
    }
  )
  
  ;; Update total grid capacity
  (var-set total-grid-capacity (+ (var-get total-grid-capacity) capacity-kwh))
  
  (ok node-id)
  )
)

(define-public (register-storage-unit
  (location (string-utf8 256))
  (capacity-kwh uint)
  (charge-rate uint)
  (discharge-rate uint)
  (efficiency uint)
  )
  (let (
    (storage-id (increment-storage-id))
  )
  (asserts! (> capacity-kwh u0) ERR_INVALID_LOAD)
  (asserts! (<= efficiency u100) (err u807))
  
  (map-set energy-storage-units
    { storage-id: storage-id }
    {
      operator: tx-sender,
      location: location,
      capacity-kwh: capacity-kwh,
      stored-energy: u0,
      charge-rate: charge-rate,
      discharge-rate: discharge-rate,
      efficiency: efficiency,
      status: "available",
      installation-date: block-height
    }
  )
  
  (ok storage-id)
  )
)

(define-public (update-node-load (node-id uint) (new-load uint))
  (let (
    (node-info (unwrap! (get-grid-node node-id) ERR_GRID_NODE_NOT_FOUND))
  )
  (asserts! (is-eq tx-sender (get operator node-info)) ERR_NOT_AUTHORIZED)
  (asserts! (<= new-load (get capacity-kwh node-info)) ERR_GRID_OVERLOAD)
  
  ;; Calculate load difference for grid total
  (let (
    (load-diff (to-int (- new-load (get current-load node-info))))
  )
  (update-grid-load load-diff)
  )
  
  ;; Update node load
  (map-set grid-nodes
    { node-id: node-id }
    (merge node-info { current-load: new-load })
  )
  
  (ok true)
  )
)

(define-public (balance-grid-load)
  (let (
    (grid-status (get-grid-status))
    (current-efficiency (calculate-grid-efficiency))
  )
  (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
  
  ;; Simple load balancing algorithm
  ;; In a real implementation, this would analyze all nodes and redistribute load
  
  (if (> (get load-percentage grid-status) (var-get emergency-threshold))
    ;; Emergency load shedding
    (begin
      (var-set current-grid-load 
        (/ (* (var-get total-grid-capacity) (var-get optimal-load-percentage)) u100))
      (ok "emergency-load-shed")
    )
    ;; Normal optimization
    (ok "load-optimized")
  )
  )
)

(define-public (charge-storage (storage-id uint) (energy-amount uint))
  (let (
    (storage-info (unwrap! (get-storage-unit storage-id) ERR_GRID_NODE_NOT_FOUND))
  )
  (asserts! (is-eq tx-sender (get operator storage-info)) ERR_NOT_AUTHORIZED)
  
  (let (
    (available-capacity (- (get capacity-kwh storage-info) (get stored-energy storage-info)))
    (actual-charge (if (> energy-amount available-capacity) available-capacity energy-amount))
  )
  (asserts! (> actual-charge u0) ERR_STORAGE_FULL)
  
  (map-set energy-storage-units
    { storage-id: storage-id }
    (merge storage-info {
      stored-energy: (+ (get stored-energy storage-info) actual-charge)
    })
  )
  
  (ok actual-charge)
  )
  )
)

(define-public (discharge-storage (storage-id uint) (energy-amount uint))
  (let (
    (storage-info (unwrap! (get-storage-unit storage-id) ERR_GRID_NODE_NOT_FOUND))
  )
  (asserts! (is-eq tx-sender (get operator storage-info)) ERR_NOT_AUTHORIZED)
  
  (let (
    (available-energy (get stored-energy storage-info))
    (actual-discharge (if (> energy-amount available-energy) available-energy energy-amount))
  )
  (asserts! (> actual-discharge u0) ERR_INSUFFICIENT_STORAGE)
  
  (map-set energy-storage-units
    { storage-id: storage-id }
    (merge storage-info {
      stored-energy: (- (get stored-energy storage-info) actual-discharge)
    })
  )
  
  (ok actual-discharge)
  )
  )
)

(define-public (schedule-maintenance (node-id uint) (maintenance-type (string-ascii 50)) (priority-level uint))
  (let (
    (node-info (unwrap! (get-grid-node node-id) ERR_GRID_NODE_NOT_FOUND))
    (schedule-id (+ (var-get grid-node-id-nonce) u10000)) ;; Simple ID generation
  )
  (asserts! (is-eq tx-sender (get operator node-info)) ERR_NOT_AUTHORIZED)
  
  (map-set maintenance-schedules
    { schedule-id: schedule-id }
    {
      node-id: node-id,
      maintenance-type: maintenance-type,
      scheduled-date: (+ block-height u144), ;; Schedule for ~24 hours later
      estimated-duration: u72, ;; ~12 hours
      priority-level: priority-level,
      completed: false,
      cost: u500000 ;; 0.5 STX base cost
    }
  )
  
  (ok schedule-id)
  )
)

(define-public (emergency-shutdown (node-id uint))
  (let (
    (node-info (unwrap! (get-grid-node node-id) ERR_GRID_NODE_NOT_FOUND))
  )
  (asserts! (or 
    (is-eq tx-sender (get operator node-info))
    (is-eq tx-sender CONTRACT_OWNER)
  ) ERR_NOT_AUTHORIZED)
  
  ;; Update node status and reduce grid load
  (let (
    (load-reduction (to-int (get current-load node-info)))
  )
  (update-grid-load (- load-reduction))
  )
  
  (map-set grid-nodes
    { node-id: node-id }
    (merge node-info {
      status: "emergency-shutdown",
      current-load: u0
    })
  )
  
  (ok true)
  )
)

;; Admin functions
(define-public (set-emergency-threshold (new-threshold uint))
  (begin
  (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
  (asserts! (<= new-threshold u100) (err u808))
  (var-set emergency-threshold new-threshold)
  (ok true)
  )
)

(define-public (force-grid-rebalance)
  (begin
  (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
  (balance-grid-load)
  )
)


;; title: smart-grid-management
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

