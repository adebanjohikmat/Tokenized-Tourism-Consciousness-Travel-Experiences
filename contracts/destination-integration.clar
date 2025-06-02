;; Destination Integration Contract
;; Connects consciousness with travel locations

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u300))
(define-constant ERR_DESTINATION_NOT_FOUND (err u301))
(define-constant ERR_ALREADY_REGISTERED (err u302))
(define-constant ERR_CAPACITY_EXCEEDED (err u303))
(define-constant ERR_INVALID_PERCENTAGE (err u304))

;; Data Variables
(define-data-var next-destination-id uint u1)

;; Data Maps
(define-map destinations uint {
    name: (string-ascii 100),
    location: (string-ascii 100),
    community-contact: principal,
    max-monthly-visitors: uint,
    current-monthly-visitors: uint,
    sustainability-score: uint,
    cultural-sensitivity-level: uint,
    revenue-share-percentage: uint,
    active: bool,
    registered-at: uint
})

(define-map destination-features uint {
    sacred-sites: bool,
    natural-healing: bool,
    meditation-spaces: bool,
    cultural-ceremonies: bool,
    eco-systems: bool,
    community-projects: bool
})

(define-map destination-partnerships uint {
    local-guides: uint,
    accommodation-partners: uint,
    cultural-educators: uint,
    wellness-practitioners: uint
})

(define-map community-revenue principal {
    total-earned: uint,
    last-payout: uint,
    pending-amount: uint
})

(define-map destination-bookings uint {
    total-bookings: uint,
    current-month-bookings: uint,
    last-booking-block: uint
})

;; Public Functions

;; Register a new destination
(define-public (register-destination
    (name (string-ascii 100))
    (location (string-ascii 100))
    (community-contact principal)
    (max-monthly-visitors uint)
    (revenue-share-percentage uint)
)
    (let (
        (destination-id (var-get next-destination-id))
    )
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (asserts! (<= revenue-share-percentage u100) ERR_INVALID_PERCENTAGE)

        ;; Create destination
        (map-set destinations destination-id {
            name: name,
            location: location,
            community-contact: community-contact,
            max-monthly-visitors: max-monthly-visitors,
            current-monthly-visitors: u0,
            sustainability-score: u50, ;; Default score
            cultural-sensitivity-level: u1,
            revenue-share-percentage: revenue-share-percentage,
            active: true,
            registered-at: block-height
        })

        ;; Initialize features
        (map-set destination-features destination-id {
            sacred-sites: false,
            natural-healing: false,
            meditation-spaces: false,
            cultural-ceremonies: false,
            eco-systems: false,
            community-projects: false
        })

        ;; Initialize partnerships
        (map-set destination-partnerships destination-id {
            local-guides: u0,
            accommodation-partners: u0,
            cultural-educators: u0,
            wellness-practitioners: u0
        })

        ;; Initialize bookings
        (map-set destination-bookings destination-id {
            total-bookings: u0,
            current-month-bookings: u0,
            last-booking-block: u0
        })

        ;; Initialize community revenue
        (map-set community-revenue community-contact {
            total-earned: u0,
            last-payout: u0,
            pending-amount: u0
        })

        ;; Increment next ID
        (var-set next-destination-id (+ destination-id u1))

        (ok destination-id)
    )
)

;; Record a booking at destination
(define-public (record-destination-booking (destination-id uint) (visitor-count uint) (revenue-amount uint))
    (match (map-get? destinations destination-id)
        destination-data (begin
            (asserts! (get active destination-data) ERR_DESTINATION_NOT_FOUND)
            (asserts! (<= (+ (get current-monthly-visitors destination-data) visitor-count)
                         (get max-monthly-visitors destination-data)) ERR_CAPACITY_EXCEEDED)

            ;; Update destination visitor count
            (map-set destinations destination-id (merge destination-data {
                current-monthly-visitors: (+ (get current-monthly-visitors destination-data) visitor-count)
            }))

            ;; Update booking statistics
            (match (map-get? destination-bookings destination-id)
                booking-data (begin
                    (map-set destination-bookings destination-id (merge booking-data {
                        total-bookings: (+ (get total-bookings booking-data) u1),
                        current-month-bookings: (+ (get current-month-bookings booking-data) u1),
                        last-booking-block: block-height
                    }))

                    ;; Calculate and add community revenue
                    (let (
                        (community-share (/ (* revenue-amount (get revenue-share-percentage destination-data)) u100))
                        (community-contact (get community-contact destination-data))
                    )
                        (match (map-get? community-revenue community-contact)
                            revenue-data (begin
                                (map-set community-revenue community-contact (merge revenue-data {
                                    pending-amount: (+ (get pending-amount revenue-data) community-share)
                                }))
                                (ok true)
                            )
                            ERR_DESTINATION_NOT_FOUND
                        )
                    )
                )
                ERR_DESTINATION_NOT_FOUND
            )
        )
        ERR_DESTINATION_NOT_FOUND
    )
)

;; Update destination features
(define-public (update-destination-features
    (destination-id uint)
    (sacred-sites bool)
    (natural-healing bool)
    (meditation-spaces bool)
    (cultural-ceremonies bool)
    (eco-systems bool)
    (community-projects bool)
)
    (match (map-get? destinations destination-id)
        destination-data (begin
            (asserts! (is-eq tx-sender (get community-contact destination-data)) ERR_UNAUTHORIZED)

            (map-set destination-features destination-id {
                sacred-sites: sacred-sites,
                natural-healing: natural-healing,
                meditation-spaces: meditation-spaces,
                cultural-ceremonies: cultural-ceremonies,
                eco-systems: eco-systems,
                community-projects: community-projects
            })
            (ok true)
        )
        ERR_DESTINATION_NOT_FOUND
    )
)

;; Update sustainability score
(define-public (update-sustainability-score (destination-id uint) (new-score uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (asserts! (<= new-score u100) ERR_INVALID_PERCENTAGE)

        (match (map-get? destinations destination-id)
            destination-data (begin
                (map-set destinations destination-id (merge destination-data {
                    sustainability-score: new-score
                }))
                (ok true)
            )
            ERR_DESTINATION_NOT_FOUND
        )
    )
)

;; Payout community revenue
(define-public (payout-community-revenue (community-contact principal))
    (match (map-get? community-revenue community-contact)
        revenue-data (begin
            (let (
                (payout-amount (get pending-amount revenue-data))
            )
                (asserts! (> payout-amount u0) ERR_UNAUTHORIZED)

                ;; Transfer payment to community
                (try! (as-contract (stx-transfer? payout-amount tx-sender community-contact)))

                ;; Update revenue record
                (map-set community-revenue community-contact (merge revenue-data {
                    total-earned: (+ (get total-earned revenue-data) payout-amount),
                    last-payout: block-height,
                    pending-amount: u0
                }))

                (ok payout-amount)
            )
        )
        ERR_DESTINATION_NOT_FOUND
    )
)

;; Reset monthly visitor count (called monthly)
(define-public (reset-monthly-visitors (destination-id uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)

        (match (map-get? destinations destination-id)
            destination-data (begin
                (map-set destinations destination-id (merge destination-data {
                    current-monthly-visitors: u0
                }))

                ;; Reset monthly bookings
                (match (map-get? destination-bookings destination-id)
                    booking-data (begin
                        (map-set destination-bookings destination-id (merge booking-data {
                            current-month-bookings: u0
                        }))
                        (ok true)
                    )
                    ERR_DESTINATION_NOT_FOUND
                )
            )
            ERR_DESTINATION_NOT_FOUND
        )
    )
)

;; Read-only functions

(define-read-only (get-destination (destination-id uint))
    (map-get? destinations destination-id)
)

(define-read-only (get-destination-features (destination-id uint))
    (map-get? destination-features destination-id)
)

(define-read-only (get-destination-partnerships (destination-id uint))
    (map-get? destination-partnerships destination-id)
)

(define-read-only (get-community-revenue (community-contact principal))
    (map-get? community-revenue community-contact)
)

(define-read-only (get-destination-bookings (destination-id uint))
    (map-get? destination-bookings destination-id)
)

(define-read-only (check-destination-capacity (destination-id uint) (additional-visitors uint))
    (match (map-get? destinations destination-id)
        destination-data
            (<= (+ (get current-monthly-visitors destination-data) additional-visitors)
                (get max-monthly-visitors destination-data))
        false
    )
)

;; Admin functions

(define-public (deactivate-destination (destination-id uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)

        (match (map-get? destinations destination-id)
            destination-data (begin
                (map-set destinations destination-id (merge destination-data { active: false }))
                (ok true)
            )
            ERR_DESTINATION_NOT_FOUND
        )
    )
)
