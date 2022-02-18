enum HsVaporTokenType {
    VtClaimShadowHs,
    VtClaimVrt,
    VtClaimVt,
    VaporizeShadowHs,
    VaporizePt
}

function getTokenName(tokenType: HsVaporTokenType) {
    switch(tokenType) {
        case HsVaporTokenType.VtClaimShadowHs:
            return "VTClaim Shadow Hypeskull";
        case HsVaporTokenType.VtClaimVrt:
            return "VTClaim VRT"
        case HsVaporTokenType.VtClaimVt:
            return "VTClaim VT"
        case HsVaporTokenType.VaporizeShadowHs:
            return "Vaporize Shadow Hypeskull"
        case HsVaporTokenType.VaporizePt:
            return "Vaporize PT"
        default:
            return ""
    }
}

export { HsVaporTokenType, getTokenName };