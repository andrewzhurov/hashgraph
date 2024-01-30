(ns hashgraph.app.snake)

;; Goal: represent received events as a snake / tangled thread.
;;
;; On receipt of new received events, the snake grows to include them.
;;
;; Where the new snake head will be?
;; It can end just below the last not-received event.
;; But then such events can be stale and even be the same across subsequently concluded rounds.
;; So perhaps a better strategy will be to end it at a position of below
;; the last non-received event of a member whose events made it in that concluded round.
;;
;; Which route will it take?
;; It could take the main route of all members, but then some may be hardly reachable and have blocker events.
;; So perhaps a snake could take the widest available route.
;; Given there is <= 1 / 3 hardly reachable members (possible blocks) that are randomly positioned,
;; the least wide route will be of size 1 on 3 members.
;; Which is rather narrow.
;; By increasing spacing between member hashgraphs perhaps we could fit it even there.
;; This route needs to be able to accomodate ~3x events.
;; Which may require to horizontal spacing bigger, which may worsen event issue animation.
;;
;; Snake will be longer that length from last head position to the new one.
;; To fit it we can bend it in a wave form.
;; Max width of a wave ~ is fixed.
;; There may be some old events that were
;; To figure out how many waves should be there we can length-to-fill / wave-events-count-width.
;; To figure out how much waves should be there we can find y-offset per event.
;; Knowing y-offset, we can derive
