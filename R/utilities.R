#' @include class.R accessors.R
NULL

.computeES <- function(top, bottom, profile) {
    # top: character vecotr of gene names
    # bottom: character vecotr of gene names
    # profile: named numeric vector of expression levels

    # sort profile -------------------------------------------------------------
    ranks <- rank(profile)
    ranks <- ranks[ranks]

    # top signature ------------------------------------------------------------
    membership <- names(ranks) %in% top
    pHits <- cumsum(membership) / length(top)
    pMisses <- cumsum(!membership) / (length(profile) - length(top))
    indexMax <- which.max(abs(pHits - pMisses))
    topES <- pHits[indexMax] - pMisses[indexMax]

    # bottom signature ---------------------------------------------------------
    membership <- names(ranks) %in% bottom
    pHits <- cumsum(membership)/length(bottom)
    pMisses <- cumsum(!membership)/(length(profile)-length(bottom))
    indexMax <- which.max(abs(pHits - pMisses))
    bottomES <- pHits[indexMax] - pMisses[indexMax]

    # compute overall ES -------------------------------------------------------
    # returns -1 if top and bottom are respectively at the top and bottom of the
    # profile
    # returns 1 if top and bottom are respectively at the bottom and top of the
    # profile

    ES <- (topES - bottomES) / 2
    ES
}
