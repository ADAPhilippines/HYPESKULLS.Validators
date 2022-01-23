import CardanoChainData from "./CardanoChainData";

type CardanoChainDataField = {
    Id: string,
    CreatedAt: Date,
    Type: number,
    Key: string,
    Value: string | number | CardanoChainDataField[] | CardanoChainData
}

export default CardanoChainDataField;