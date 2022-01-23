import { Config } from "./Config";
import Helpers from "./Helpers";

class HsHelpers {
    static GetShadowHsTokenName = (n: number, delimiter: string = "") => {
        var assetName = "HYPESKULL" + n.toString().padStart(4, '0') + "_SH";
        return Config.VaporPolicyId + delimiter + Helpers.ToHex(Buffer.from(assetName, "ascii"));
    }

    static GetShHsMintString = (start: number, end: number) => {
        let mintString = "";
        for (var i = start; i <= end; i++) {
            mintString += " + 1 " + HsHelpers.GetShadowHsTokenName(i, ".");
        }
        return mintString.substring(3);
    }
}

export default HsHelpers;