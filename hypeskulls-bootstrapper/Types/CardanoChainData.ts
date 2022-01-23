import CardanoAsset from "./CardanoAsset";
import CardanoChainDataField from "./CardanoChainDataField";

type CardanoChainData = {
    Id: string,
    Amounts: CardanoAsset[],
    ConstructorIndex: number,
    CreatedAt: Date,
    DatumHash: string,
    Fields: CardanoChainDataField[],
    IsConsumed: boolean,
    OwnerAddress: string,
    TxId: string,
    TxIdx: number,
    Type: number
};

export default CardanoChainData;