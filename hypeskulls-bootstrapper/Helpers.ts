import * as bip39 from 'bip39';
import { Bip32PrivateKey } from './custom_modules/@emurgo/cardano-serialization-lib-nodejs/cardano_serialization_lib';

class Helpers {
    static FromHex = (hex: string) => Buffer.from(hex, "hex");

    static ToHex = (bytes: Uint8Array) => Buffer.from(bytes).toString("hex");

    static AsciiToHex = (ascii: string) => Helpers.ToHex(Buffer.from(ascii, "ascii"));

    static Harden = (num: number) => 0x80000000 + num;

    static GenerateMnemonic = () => bip39.generateMnemonic(256);
    
    static GetEntropy = (seed_phrase: string): string => bip39.mnemonicToEntropy(seed_phrase);

    static GetPrivateKey = (rootKey: Bip32PrivateKey) => {
    
        return rootKey
            .derive(Helpers.Harden(1852)) // purpose
            .derive(Helpers.Harden(1815)) // coin type
            .derive(Helpers.Harden(0)) // account #0
            .derive(0) // external
            .derive(0)
            .to_raw_key();
    }

    static WaitFor = (ms: number = 1000) => {
        return new Promise((resolve, reject) => setTimeout(resolve, ms));
    }
}

export default Helpers;