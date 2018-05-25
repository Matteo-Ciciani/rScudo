#' @include class.R accessors.R
NULL

.computeES <- function(top, bottom, profile) {
    # sort profile
    ranks <- rank(profile)
    ranks <- ranks[ranks]

    # top signature
    membership <- names(ranks) %in% top
    phits <- cumsum(membership)/length(top)
    pmisses <- cumsum(!membership)/(length(profile)-length(top))
    indexMax <- which.max(abs(phits - pmisses))
    topES <- phits[indexMax] - pmisses[indexMax]

    # bottom signature
    membership <- names(ranks) %in% bottom
    phits <- cumsum(membership)/length(bottom)
    pmisses <- cumsum(!membership)/(length(profile)-length(bottom))
    indexMax <- which.max(abs(phits - pmisses))
    bottomES <- phits[indexMax] - pmisses[indexMax]

    # compute overall ES
    ES <- (topES - bottomES)/2
    ES
}
