
;; title: vendor-registry
;; version: 1.0
;; summary: Farmers Market Vendor Registry
;; description: A registry for farmers market vendors with verification and certification tracking

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-VENDOR-EXISTS (err u101))
(define-constant ERR-VENDOR-NOT-FOUND (err u102))
(define-constant ERR-INVALID-STATUS (err u103))

;; Data Maps

;; Vendor registry map - stores vendor information
(define-map vendors
  { vendor-id: principal }
  {
    name: (string-ascii 100),
    location: (string-ascii 100),
    certification: (optional (string-ascii 50)),
    verified: bool,
    registration-date: uint,
    status: (string-ascii 20) ;; "active", "suspended", "inactive"
  }
)

;; Verification map - tracks who verified each vendor
(define-map verifications
  { vendor-id: principal }
  { verified-by: principal, timestamp: uint }
)

;; Track admin privileges
(define-map admin-roles
  { admin: principal }
  { can-verify: bool, can-manage: bool }
)

;; Data vars
;; Contract owner with admin privileges
(define-data-var contract-owner principal tx-sender)

;; Public functions

;; Register a new vendor
(define-public (register-vendor
                (name (string-ascii 100))
                (location (string-ascii 100))
                (certification (optional (string-ascii 50))))
  (let
    ((vendor-id tx-sender))
    (asserts! (is-none (map-get? vendors {vendor-id: vendor-id})) ERR-VENDOR-EXISTS)
    (ok (map-set vendors
      {vendor-id: vendor-id}
      {
        name: name,
        location: location,
        certification: certification,
        verified: false,
        registration-date: stacks-block-height,
        status: "active"
      }
    ))
  )
)

;; Update vendor information (only the vendor can update their own info)
(define-public (update-vendor-info
                (name (string-ascii 100))
                (location (string-ascii 100))
                (certification (optional (string-ascii 50))))
  (let
    ((vendor-id tx-sender))
    (match (map-get? vendors {vendor-id: vendor-id})
      vendor-info (begin
        (ok (map-set vendors
          {vendor-id: vendor-id}
          (merge vendor-info
            {
              name: name,
              location: location,
              certification: certification
            }
          )
        ))
      )
      ERR-VENDOR-NOT-FOUND
    )
  )
)

;; Verify a vendor (only admins with verification permissions)
(define-public (verify-vendor (vendor-id principal))
  (let
    ((admin tx-sender))
    (asserts! (has-verify-permission admin) ERR-NOT-AUTHORIZED)
    (match (map-get? vendors {vendor-id: vendor-id})
      vendor-info (begin
        (map-set verifications
          {vendor-id: vendor-id}
          {verified-by: admin, timestamp: stacks-block-height}
        )
        (ok (map-set vendors
          {vendor-id: vendor-id}
          (merge vendor-info {verified: true})
        ))
      )
      ERR-VENDOR-NOT-FOUND
    )
  )
)

;; Change vendor status (only admins with management permissions)
(define-public (change-vendor-status (vendor-id principal) (new-status (string-ascii 20)))
  (let
    ((admin tx-sender))
    (asserts! (has-manage-permission admin) ERR-NOT-AUTHORIZED)
    (asserts! (or (is-eq new-status "active") (is-eq new-status "suspended") (is-eq new-status "inactive")) ERR-INVALID-STATUS)
    (match (map-get? vendors {vendor-id: vendor-id})
      vendor-info (ok (map-set vendors
        {vendor-id: vendor-id}
        (merge vendor-info {status: new-status})
      ))
      ERR-VENDOR-NOT-FOUND
    )
  )
)

;; Grant admin role (only contract owner)
(define-public (grant-admin-role (admin principal) (can-verify bool) (can-manage bool))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    (ok (map-set admin-roles
      {admin: admin}
      {can-verify: can-verify, can-manage: can-manage}
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

;; Check if an admin can verify vendors
(define-read-only (has-verify-permission (admin principal))
  (default-to false (get can-verify (map-get? admin-roles {admin: admin})))
)

;; Check if an admin can manage vendor statuses
(define-read-only (has-manage-permission (admin principal))
  (default-to false (get can-manage (map-get? admin-roles {admin: admin})))
)

;; Get vendor information
(define-read-only (get-vendor-info (vendor-id principal))
  (map-get? vendors {vendor-id: vendor-id})
)

;; Get verification information
(define-read-only (get-verification-info (vendor-id principal))
  (map-get? verifications {vendor-id: vendor-id})
)

;; Get contract owner
(define-read-only (get-contract-owner)
  (var-get contract-owner)
)

