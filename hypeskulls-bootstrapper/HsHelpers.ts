import { Config } from "./Config";
import Helpers from "./Helpers";

class HsHelpers {
    static GetHsTokenName = (n: number, delimiter: string = "") => {
        var assetName = "HYPESKULL" + n.toString().padStart(4, '0');
        return Config.VaporPolicyId + delimiter + Helpers.AsciiToHex(assetName);
    }

    static GetShadowHsTokenName = (n: number, delimiter: string = "") => {
        var assetName = "HYPESKULL" + n.toString().padStart(4, '0') + "_SH";
        return Config.VaporPolicyId + delimiter + Helpers.AsciiToHex(assetName);
    }

    static GetVrtTokenName = (n: number, delimiter: string = "") => {
        var assetName = "HYPESKULLS_VRT_" + n.toString().padStart(4, '0');
        return Config.VaporPolicyId + delimiter + Helpers.AsciiToHex(assetName);
    }

    static GetHsMintString = (start: number, end: number) => {
        let mintString = "";
        for (var i = start; i <= end; i++) {
            mintString += " + 1 " + HsHelpers.GetHsTokenName(i, ".");
        }
        return mintString.substring(3);
    }

    static GetShHsMintString = (start: number, end: number) => {
        let mintString = "";
        for (var i = start; i <= end; i++) {
            mintString += " + 1 " + HsHelpers.GetShadowHsTokenName(i, ".");
        }
        return mintString.substring(3);
    }

    static GetVrtMintString = (start: number, end: number) => {
        let mintString = "";
        for (var i = start; i <= end; i++) {
            mintString += " + 1 " + HsHelpers.GetVrtTokenName(i, ".");
        }
        return mintString.substring(3);
    }
}

export default HsHelpers;