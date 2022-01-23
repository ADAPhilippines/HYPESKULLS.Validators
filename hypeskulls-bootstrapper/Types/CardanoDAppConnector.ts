import Paginate from "./Paginate";

type CardanoDAPPConnector = {
    enable(): Promise<boolean>;

    isEnabled(): Promise<boolean>;

    getUsedAddresses(paginate?: Paginate): Promise<string[]>;

    getUtxos(): Promise<string[]>;

    signTx(tx: string, b: boolean): Promise<string>;
    submitTx(txCBORHex: string): Promise<any>;
    getCollateral() : Promise<string[]>
}

export default CardanoDAPPConnector;