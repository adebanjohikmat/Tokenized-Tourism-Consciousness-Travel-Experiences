;; Experience Design Contract
;; Manages consciousness travel experiences

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u200))
(define-constant ERR_EXPERIENCE_NOT_FOUND (err u201))
(define-constant ERR_INVALID_DATES (err u202))
(define-constant ERR_INSUFFICIENT_PAYMENT (err u203))
(define-constant ERR_EXPERIENCE_FULL (err u204))
(define-constant ERR_ALREADY_BOOKED (err u205))

;; Data Variables
(define-data-var next-experience-id uint u1)
(define-data-var platform-fee-percentage uint u5) ;; 5%

;; Data Maps
(define-map experiences uint {
    provider: principal,
    title: (string-ascii 100),
    description: (string-ascii 500),
    location: (string-ascii 100),
    duration-days: uint,
    max-participants: uint,
    current-participants: uint,
    price-per-person: uint,
    start-date: uint,
    end-date: uint,
    experience-type: (string-ascii 50),
    created-at: uint,
    active: bool
})

(define-map experience-activities uint {
    meditation: bool,
    yoga: bool,
    cultural-immersion: bool,
    nature-connection: bool,
    community-service: bool,
    wellness-workshops: bool
})

(define-map bookings { experience-id: uint, traveler: principal } {
    booking-date: uint,
    payment-amount: uint,
    status: (string-ascii 20),
    special-requirements: (string-ascii 200)
})

(define-map traveler-bookings principal (list 50 uint))

(define-map experience-reviews uint {
    total-reviews: uint,
    average-rating: uint,
    consciousness-impact-score: uint
})

;; Public Functions

;; Create a new experience
(define-public (create-experience
    (title (string-ascii 100))
    (description (string-ascii 500))
    (location (string-ascii 100))
    (duration-days uint)
    (max-participants uint)
    (price-per-person uint)
    (start-date uint)
    (end-date uint)
    (experience-type (string-ascii 50))
)
    (let (
        (experience-id (var-get next-experience-id))
        (provider tx-sender)
    )
        ;; Validate provider is verified (would call provider-verification contract)
        (asserts! (> end-date start-date) ERR_INVALID_DATES)
        (asserts! (> max-participants u0) ERR_UNAUTHORIZED)

        ;; Create experience
        (map-set experiences experience-id {
            provider: provider,
            title: title,
            description: description,
            location: location,
            duration-days: duration-days,
            max-participants: max-participants,
            current-participants: u0,
            price-per-person: price-per-person,
            start-date: start-date,
            end-date: end-date,
            experience-type: experience-type,
            created-at: block-height,
            active: true
        })

        ;; Initialize activities
        (map-set experience-activities experience-id {
            meditation: false,
            yoga: false,
            cultural-immersion: false,
            nature-connection: false,
            community-service: false,
            wellness-workshops: false
        })

        ;; Initialize reviews
        (map-set experience-reviews experience-id {
            total-reviews: u0,
            average-rating: u0,
            consciousness-impact-score: u0
        })

        ;; Increment next ID
        (var-set next-experience-id (+ experience-id u1))

        (ok experience-id)
    )
)

;; Book an experience
(define-public (book-experience (experience-id uint) (special-requirements (string-ascii 200)))
    (let (
        (traveler tx-sender)
        (booking-key { experience-id: experience-id, traveler: traveler })
    )
        (match (map-get? experiences experience-id)
            experience-data (begin
                (asserts! (get active experience-data) ERR_EXPERIENCE_NOT_FOUND)
                (asserts! (< (get current-participants experience-data) (get max-participants experience-data)) ERR_EXPERIENCE_FULL)
                (asserts! (is-none (map-get? bookings booking-key)) ERR_ALREADY_BOOKED)

                ;; Calculate total payment including platform fee
                (let (
                    (base-price (get price-per-person experience-data))
                    (platform-fee (/ (* base-price (var-get platform-fee-percentage)) u100))
                    (total-payment (+ base-price platform-fee))
                )
                    ;; Transfer payment
                    (try! (stx-transfer? total-payment traveler (get provider experience-data)))

                    ;; Create booking
                    (map-set bookings booking-key {
                        booking-date: block-height,
                        payment-amount: total-payment,
                        status: "confirmed",
                        special-requirements: special-requirements
                    })

                    ;; Update participant count
                    (map-set experiences experience-id (merge experience-data {
                        current-participants: (+ (get current-participants experience-data) u1)
                    }))

                    ;; Add to traveler's bookings
                    (let (
                        (current-bookings (default-to (list) (map-get? traveler-bookings traveler)))
                    )
                        (map-set traveler-bookings traveler (unwrap! (as-max-len? (append current-bookings experience-id) u50) ERR_EXPERIENCE_FULL))
                    )

                    (ok true)
                )
            )
            ERR_EXPERIENCE_NOT_FOUND
        )
    )
)

;; Cancel booking
(define-public (cancel-booking (experience-id uint))
    (let (
        (traveler tx-sender)
        (booking-key { experience-id: experience-id, traveler: traveler })
    )
        (match (map-get? bookings booking-key)
            booking-data (begin
                (asserts! (is-eq (get status booking-data) "confirmed") ERR_UNAUTHORIZED)

                ;; Update booking status
                (map-set bookings booking-key (merge booking-data { status: "cancelled" }))

                ;; Update participant count
                (match (map-get? experiences experience-id)
                    experience-data (begin
                        (map-set experiences experience-id (merge experience-data {
                            current-participants: (- (get current-participants experience-data) u1)
                        }))
                        (ok true)
                    )
                    ERR_EXPERIENCE_NOT_FOUND
                )
            )
            ERR_EXPERIENCE_NOT_FOUND
        )
    )
)

;; Update experience activities
(define-public (update-activities
    (experience-id uint)
    (meditation bool)
    (yoga bool)
    (cultural-immersion bool)
    (nature-connection bool)
    (community-service bool)
    (wellness-workshops bool)
)
    (match (map-get? experiences experience-id)
        experience-data (begin
            (asserts! (is-eq tx-sender (get provider experience-data)) ERR_UNAUTHORIZED)

            (map-set experience-activities experience-id {
                meditation: meditation,
                yoga: yoga,
                cultural-immersion: cultural-immersion,
                nature-connection: nature-connection,
                community-service: community-service,
                wellness-workshops: wellness-workshops
            })
            (ok true)
        )
        ERR_EXPERIENCE_NOT_FOUND
    )
)

;; Deactivate experience
(define-public (deactivate-experience (experience-id uint))
    (match (map-get? experiences experience-id)
        experience-data (begin
            (asserts! (is-eq tx-sender (get provider experience-data)) ERR_UNAUTHORIZED)
            (map-set experiences experience-id (merge experience-data { active: false }))
            (ok true)
        )
        ERR_EXPERIENCE_NOT_FOUND
    )
)

;; Read-only functions

(define-read-only (get-experience (experience-id uint))
    (map-get? experiences experience-id)
)

(define-read-only (get-experience-activities (experience-id uint))
    (map-get? experience-activities experience-id)
)

(define-read-only (get-booking (experience-id uint) (traveler principal))
    (map-get? bookings { experience-id: experience-id, traveler: traveler })
)

(define-read-only (get-traveler-bookings (traveler principal))
    (map-get? traveler-bookings traveler)
)

(define-read-only (get-experience-reviews (experience-id uint))
    (map-get? experience-reviews experience-id)
)

(define-read-only (get-next-experience-id)
    (var-get next-experience-id)
)

;; Admin functions

(define-public (set-platform-fee (new-fee uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (asserts! (<= new-fee u20) ERR_UNAUTHORIZED) ;; Max 20%
        (var-set platform-fee-percentage new-fee)
        (ok true)
    )
)
