type CardanoProtocolParameters = {
    min_fee_a: number,
    min_fee_b: number,
    min_utxo: number,
    pool_deposit: number,
    key_deposit: number,
    max_tx_size: number,
    max_val_size: string,
    price_mem: number,
    price_step: number,
    coins_per_utxo_word: string
}

export default CardanoProtocolParameters;