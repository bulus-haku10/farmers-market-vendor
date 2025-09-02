
;; title: product-tracker
;; version: 1.0
;; summary: Product Origin and Quality Tracker
;; description: Track product origin, quality certifications, and harvest information for farmers market produce

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u200))
(define-constant ERR-PRODUCT-EXISTS (err u201))
(define-constant ERR-PRODUCT-NOT-FOUND (err u202))
(define-constant ERR-INVALID-QUALITY-SCORE (err u203))
(define-constant ERR-VENDOR-NOT-VERIFIED (err u204))

;; Constants
(define-constant MAX-QUALITY-SCORE u100)

;; Data Maps

;; Product information map
(define-map products
  { product-id: uint }
  {
    vendor-id: principal,
    name: (string-ascii 100),
    category: (string-ascii 50),
    origin-farm: (string-ascii 100),
    harvest-date: uint,
    expiry-date: uint,
    organic-certified: bool,
    quality-score: uint,
    certification-authority: (optional (string-ascii 100)),
    created-at: uint
  }
)

;; Quality assessments map - tracks quality evaluations
(define-map quality-assessments
  { product-id: uint, assessor: principal }
  {
    score: uint,
    notes: (string-ascii 200),
    timestamp: uint
  }
)

;; Track batch information for group harvests
(define-map batch-info
  { batch-id: uint }
  {
    vendor-id: principal,
    harvest-location: (string-ascii 100),
    harvest-date: uint,
    product-count: uint,
    total-weight: uint ;; in grams
  }
)

;; Product to batch mapping
(define-map product-batches
  { product-id: uint }
  { batch-id: uint }
)

;; Data vars
(define-data-var next-product-id uint u1)
(define-data-var next-batch-id uint u1)
(define-data-var contract-owner principal tx-sender)

;; Quality assessor permissions
(define-map quality-assessors
  { assessor: principal }
  { authorized: bool }
)

;; Public functions

;; Add a new product (only registered vendors can add products)
(define-public (add-product
                (name (string-ascii 100))
                (category (string-ascii 50))
                (origin-farm (string-ascii 100))
                (harvest-date uint)
                (expiry-date uint)
                (organic-certified bool)
                (certification-authority (optional (string-ascii 100))))
  (let
    ((product-id (var-get next-product-id))
     (vendor-id tx-sender))
    ;; Note: In a real implementation, we'd check if vendor is registered and verified
    ;; For this demo, we'll allow any principal to add products
    (map-set products
      {product-id: product-id}
      {
        vendor-id: vendor-id,
        name: name,
        category: category,
        origin-farm: origin-farm,
        harvest-date: harvest-date,
        expiry-date: expiry-date,
        organic-certified: organic-certified,
        quality-score: u0,
        certification-authority: certification-authority,
        created-at: stacks-block-height
      }
    )
    (var-set next-product-id (+ product-id u1))
    (ok product-id)
  )
)

;; Create a new batch for group harvests
(define-public (create-batch
                (harvest-location (string-ascii 100))
                (harvest-date uint)
                (total-weight uint))
  (let
    ((batch-id (var-get next-batch-id))
     (vendor-id tx-sender))
    (map-set batch-info
      {batch-id: batch-id}
      {
        vendor-id: vendor-id,
        harvest-location: harvest-location,
        harvest-date: harvest-date,
        product-count: u0,
        total-weight: total-weight
      }
    )
    (var-set next-batch-id (+ batch-id u1))
    (ok batch-id)
  )
)

;; Assign product to batch
(define-public (assign-product-to-batch (product-id uint) (batch-id uint))
  (let
    ((vendor-id tx-sender))
    (match (map-get? products {product-id: product-id})
      product-info (begin
        (asserts! (is-eq (get vendor-id product-info) vendor-id) ERR-NOT-AUTHORIZED)
        (match (map-get? batch-info {batch-id: batch-id})
          batch-data (begin
            (asserts! (is-eq (get vendor-id batch-data) vendor-id) ERR-NOT-AUTHORIZED)
            ;; Update batch product count
            (map-set batch-info
              {batch-id: batch-id}
              (merge batch-data {product-count: (+ (get product-count batch-data) u1)})
            )
            (ok (map-set product-batches
              {product-id: product-id}
              {batch-id: batch-id}
            ))
          )
          ERR-PRODUCT-NOT-FOUND
        )
      )
      ERR-PRODUCT-NOT-FOUND
    )
  )
)

;; Assess product quality (only authorized quality assessors)
(define-public (assess-quality
                (product-id uint)
                (score uint)
                (notes (string-ascii 200)))
  (let
    ((assessor tx-sender))
    (asserts! (is-authorized-assessor assessor) ERR-NOT-AUTHORIZED)
    (asserts! (<= score MAX-QUALITY-SCORE) ERR-INVALID-QUALITY-SCORE)
    (match (map-get? products {product-id: product-id})
      product-info (begin
        ;; Update product quality score to the new assessment
        (map-set products
          {product-id: product-id}
          (merge product-info {quality-score: score})
        )
        ;; Record the detailed assessment
        (ok (map-set quality-assessments
          {product-id: product-id, assessor: assessor}
          {
            score: score,
            notes: notes,
            timestamp: stacks-block-height
          }
        ))
      )
      ERR-PRODUCT-NOT-FOUND
    )
  )
)

;; Authorize quality assessor (only contract owner)
(define-public (authorize-assessor (assessor principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    (ok (map-set quality-assessors
      {assessor: assessor}
      {authorized: true}
    ))
  )
)

;; Revoke assessor authorization (only contract owner)
(define-public (revoke-assessor (assessor principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    (ok (map-set quality-assessors
      {assessor: assessor}
      {authorized: false}
    ))
  )
)

;; Transfer contract ownership (only current owner)
(define-public (transfer-ownership (new-owner principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    (ok (var-set contract-owner new-owner))
  )
)

;; Read only functions

;; Check if assessor is authorized
(define-read-only (is-authorized-assessor (assessor principal))
  (default-to false (get authorized (map-get? quality-assessors {assessor: assessor})))
)

;; Get product information
(define-read-only (get-product-info (product-id uint))
  (map-get? products {product-id: product-id})
)

;; Get quality assessment for a product by a specific assessor
(define-read-only (get-quality-assessment (product-id uint) (assessor principal))
  (map-get? quality-assessments {product-id: product-id, assessor: assessor})
)

;; Get batch information
(define-read-only (get-batch-info (batch-id uint))
  (map-get? batch-info {batch-id: batch-id})
)

;; Get product's batch assignment
(define-read-only (get-product-batch (product-id uint))
  (map-get? product-batches {product-id: product-id})
)

;; Get contract owner
(define-read-only (get-contract-owner)
  (var-get contract-owner)
)

;; Get next available product ID
(define-read-only (get-next-product-id)
  (var-get next-product-id)
)

;; Get next available batch ID
(define-read-only (get-next-batch-id)
  (var-get next-batch-id)
)

