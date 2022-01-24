enum HsVaporTokenType {
    VtClaimShadowHs,
    VtClaimVrt,
    VtClaimVt
}

function getTokenName(tokenType: HsVaporTokenType) {
    switch(tokenType) {
        case HsVaporTokenType.VtClaimShadowHs:
            return "VTClaim Shadow Hypeskull";
        case HsVaporTokenType.VtClaimVrt:
            return "VTClaim VRT"
        case HsVaporTokenType.VtClaimVt:
            return "VTClaim VT"
        default:
            return ""
    }
}

export { HsVaporTokenType, getTokenName };