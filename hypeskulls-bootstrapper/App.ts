import seed_mnemonic from "./seed_mnemonic";
import CardanoLoader from "./CardanoLoader";
import { BlockFrostAPI } from "@blockfrost/blockfrost-js";
import { Address, BigNum, PlutusData, Transaction, TransactionBuilder, TransactionOutput, TransactionOutputs, TransactionUnspentOutput, Value } from "./custom_modules/@emurgo/cardano-serialization-lib-nodejs/cardano_serialization_lib";
import Helpers from "./Helpers";
import { Config } from "./Config";
import { CoinSelection, setCardanoSerializationLib as setCoinSelectionCardanoSerializationLib } from "./coinSelection";
import CardanoAssetResponse from "./Types/CardanoAssetResponse";
import PlutusDataObject from "./Types/PlutusDataObject";
import { PlutusField, PlutusFieldType } from "./Types/PlutusField";
import HsHelpers from "./HsHelpers";
import { languageViews } from "./languageViews";
import { HubConnection, HubConnectionBuilder } from "@microsoft/signalr";

const MIN_UTXO_LOVELACE = Config.MinUtxoLovelace;
const VAPOR_POLICY_ID = Config.VaporPolicyId;
const ORIGIN_POLICY_ID = Config.OriginPolicyId;
const BLOCKFROST_PROJECT_ID = Config.BlockfrostProjectId;
const NETWORK_NAME = Config.NetworkName;

let Cardano: typeof import("./custom_modules/@emurgo/cardano-serialization-lib-nodejs/cardano_serialization_lib");
let CccSignalRConnection: HubConnection
let Network: number;
let Blockfrost: BlockFrostAPI;
let PublicAddress: string;
let IsConfirmingTx: boolean = false;

const InitializeAsync = async () => {
    await LoadCardanoAsync();
    Network = NETWORK_NAME == "testnet" ? Cardano.NetworkInfo.testnet().network_id() : Cardano.NetworkInfo.mainnet().network_id();
    Blockfrost = new BlockFrostAPI({
        projectId: BLOCKFROST_PROJECT_ID, // see: https://blockfrost.io
    });
    PublicAddress = GetPublicAddress();

    CccSignalRConnection = new HubConnectionBuilder()
    .withUrl("http://localhost:1338/vapor")
    .withAutomaticReconnect()
    .build();

    CccSignalRConnection.serverTimeoutInMilliseconds = 120 * 1000;

    CccSignalRConnection.start();

    CccSignalRConnection.on("TxSumbitSuccess", async (txId) => {
        console.log("Tx submitted:", txId);
        await WaitTxConfirmedAsync(txId);
        IsConfirmingTx = false;
    });
}

const LoadCardanoAsync = async () => {
    Cardano = await CardanoLoader.LoadAsync();
}

const GetRootKey = (entropy: string) => {
    return Cardano.Bip32PrivateKey.from_bip39_entropy(Buffer.from(entropy, 'hex'), Buffer.from(''));
};

const GetPublicAddress = () => {
    let entropy = Helpers.GetEntropy(seed_mnemonic);
    const rootKey = GetRootKey(entropy);

    const accountKey = rootKey
        .derive(Helpers.Harden(1852)) // purpose
        .derive(Helpers.Harden(1815)) // coin type
        .derive(Helpers.Harden(0)); // account #0

    const utxoPubKey = accountKey
        .derive(0) // external
        .derive(0)
        .to_public();

    const enterpriseAddr = Cardano.EnterpriseAddress.new(
        Network,
        Cardano.StakeCredential.from_keyhash(utxoPubKey.to_raw_key().hash())
    );

    return enterpriseAddr.to_address().to_bech32();
}
const ToBigNum = (value: any) => Cardano.BigNum.from_str(value.toString());

const GetUtxosAsync = async () => {
    var utxosResults = await Blockfrost.addressesUtxosAll(PublicAddress);
    var utxos: TransactionUnspentOutput[] = []
    for (const utxoResult of utxosResults) {
        utxos.push(
            Cardano.TransactionUnspentOutput.new(
                Cardano.TransactionInput.new(
                    Cardano.TransactionHash.from_bytes(Helpers.FromHex(utxoResult.tx_hash)),
                    utxoResult.output_index
                ),
                Cardano.TransactionOutput.new(
                    Cardano.Address.from_bech32(PublicAddress), // use own address since blockfrost does not provide
                    amountToValue(utxoResult.amount)
                )
            )
        );
    }
    return utxos;
}

const amountToValue = (amount: CardanoAssetResponse[]) => {
    var lovelaceAmt = amount.find(a => a.unit == "lovelace") as CardanoAssetResponse;
    var val = Cardano.Value.new(ToBigNum(lovelaceAmt.quantity));
    for (const asset of amount.filter(a => a.unit != "lovelace")) {
        val = val.checked_add(AssetValue(
            ToBigNum(0),
            asset.unit.substring(0, 56),
            asset.unit.substring(56),
            ToBigNum(asset.quantity)
        ) as Value)
    }
    return val;
}

const AssetValue = (lovelaceAmt: BigNum, policyIdHex: string, assetNameHex: string, amount: BigNum) => {
    if (Cardano !== null) {
        const val = Cardano.Value.new(lovelaceAmt);
        const ma = Cardano.MultiAsset.new();
        const assets = Cardano.Assets.new();

        assets.insert(
            Cardano.AssetName.new(Helpers.FromHex(assetNameHex)),
            amount
        );

        ma.insert(
            Cardano.ScriptHash.from_bytes(Helpers.FromHex(policyIdHex)),
            assets
        );

        val.set_multiasset(ma)
        return val;
    }
}

const CreateTxBuilder = (protocolParameters: any) =>
{
    setCoinSelectionCardanoSerializationLib(Cardano);
    CoinSelection.setProtocolParameters(
        protocolParameters.min_utxo.toString(),
        protocolParameters.min_fee_a.toString(),
        protocolParameters.min_fee_b.toString(),
        protocolParameters.max_tx_size.toString()
    );
    
    const txBuilder = Cardano.TransactionBuilder.new(
        Cardano.LinearFee.new(
            ToBigNum(protocolParameters.min_fee_a),
            ToBigNum(protocolParameters.min_fee_b)
        ),
        ToBigNum("1000000"),
        ToBigNum("500000000"),
        ToBigNum("2000000"),
        parseInt("5000"),
        16384,
        5.77e-2,
        7.21e-5,
        Cardano.LanguageViews.new(Helpers.FromHex(languageViews))
    );

    return txBuilder;
}

const BootstrapShadowHsAsync = async () =>
{
    var startIdx = 1;
    var endIdx = 150;
    while(startIdx < endIdx)
    {
        startIdx = await SendBootstrapShadowHsTxAsync(startIdx, endIdx);
        while(IsConfirmingTx)
        {
            await Helpers.WaitFor(15000);
        }
    }
}

const SendBootstrapShadowHsTxAsync = async (startIdx: number = 1, endIdx: number = 30) =>
{
    const latestBlock = await Blockfrost.blocksLatest();
    const protocolParameters = await Blockfrost.epochsParameters(latestBlock.epoch as number);
    const shDatumObject = VTClaimShDatum() as PlutusDataObject;
    const shDatumHash = Cardano.hash_plutus_data(ToPlutusData(shDatumObject) as PlutusData);

    const utxos = await GetUtxosAsync();

    var tokenIdx: number = startIdx;
    var transaction: Transaction | undefined;
    var transactionOutputs: TransactionOutput[] = [];
    while(tokenIdx <= endIdx) {
        const shOutput = Cardano.TransactionOutput.new(
            VtClaimContractAddress() as Address,
            GetHypeNftsOutput([HsHelpers.GetShadowHsTokenName(tokenIdx).substring(56)]) as Value
        );
        shOutput.set_data_hash(shDatumHash);

        var tx = ConstructTx(utxos, transactionOutputs.concat([shOutput]), protocolParameters);
        
        if(tokenIdx % 5 == 0)
            console.log("current tx size: ", tx ? tx.to_bytes().length : 0);

        if(tx != null && (tx as Transaction).to_bytes().length < protocolParameters.max_tx_size)
        {
            transaction = tx;
            transactionOutputs.push(shOutput);
            tokenIdx++;
        }
        else
            break;
    }

    console.log("Final tx size: ", (transaction as Transaction).to_bytes().length);
    console.log(`Submiting Tx for Shadow Hypeskulls ${startIdx} - ${tokenIdx - 1}`);

    CccSignalRConnection.send("SubmitVTClaimTx", 
        Helpers.ToHex((transaction as Transaction).to_bytes()), 
        [shDatumObject]
    );
    IsConfirmingTx = true;
    return tokenIdx;
}

const WaitTxConfirmedAsync = async (txId: string) => {
    console.log("Confirming Tx: ", txId);
    try {
        var tx = await Blockfrost.txs(txId);
        console.log("Tx confirmed: ", txId);
    } catch (error) {
        console.log("Tx not yet confirmed: ", txId);
        await Helpers.WaitFor(15000);
        await WaitTxConfirmedAsync(txId);
    }
}

const ConstructTx = (utxos: TransactionUnspentOutput[], outputs: TransactionOutput[], protocolParameters: any) =>
{
    try
    {
        const txBuilder = CreateTxBuilder(protocolParameters);
        const selfAddress = Cardano.Address.from_bech32(PublicAddress);
        const transactionWitnessSet = Cardano.TransactionWitnessSet.new();

        const transactionOutputs = Cardano.TransactionOutputs.new();

        outputs.forEach(o => {
            transactionOutputs.add(Cardano.TransactionOutput.from_bytes(o.to_bytes()));
            txBuilder.add_output(o);
        });

        const csResult = CoinSelection.randomImprove(
            utxos,
            transactionOutputs,
            8
        );

        csResult.inputs.forEach((utxo) => {
            txBuilder.add_input(
                utxo.output().address(),
                utxo.input(),
                utxo.output().amount()
            );
        });

        txBuilder.add_change_if_needed(selfAddress);

        const txBody = txBuilder.build();

        const transaction = Cardano.Transaction.new(
            Cardano.TransactionBody.from_bytes(txBody.to_bytes()),
            Cardano.TransactionWitnessSet.from_bytes(
                transactionWitnessSet.to_bytes()
            )
        );

        const signedTx = SignTx(transaction);
        return signedTx;
    }
    catch (error)
    {
        console.error(error);
        return null;
    }
}

const SignTx = (tx: Transaction) => {
    const rootKey = getRootKey(Helpers.GetEntropy(seed_mnemonic));
	const prvKey = Helpers.GetPrivateKey(rootKey);
	const txBody = tx.body();
	const txHash = Cardano.hash_transaction(txBody);
	const witnesses = tx.witness_set();

	// add keyhash witnesses
	const vkeyWitnesses = Cardano.Vkeywitnesses.new();
	const vkeyWitness = Cardano.make_vkey_witness(txHash, prvKey);
	vkeyWitnesses.add(vkeyWitness);
	witnesses.set_vkeys(vkeyWitnesses);

	// create the finalized transaction with witnesses
	return Cardano.Transaction.new(
		txBody,
		witnesses
	);
}

const getRootKey = (entropy: string) => {
	return Cardano.Bip32PrivateKey.from_bip39_entropy(Buffer.from(entropy, 'hex'), Buffer.from(''));
};

const GetHypeNftsOutput = (assetNames: string[], lovelace: number = 0, isVapor: boolean = true) => {
    var lovelaceAmt = lovelace > MIN_UTXO_LOVELACE ? lovelace : MIN_UTXO_LOVELACE;
    let val = Cardano.Value.new(ToBigNum(lovelaceAmt));
    assetNames.forEach(n => 
        val = val.checked_add(AssetValue(
            ToBigNum(0),
            isVapor ? VAPOR_POLICY_ID : ORIGIN_POLICY_ID,
            n,
            ToBigNum(1)) as Value)
    )
    return val;
}

const VtClaimContractAddress = () => Cardano.Address.from_bech32(Config.VtClaimContractAddress);

const VTClaimShDatum = () => {
    const datum = new PlutusDataObject(0);
    datum.Fields = []
    return datum
}

function ToPlutusData (plutusDataObj: PlutusDataObject) {
    const datumFields = Cardano.PlutusList.new();
    plutusDataObj.Fields.sort((a, b) => a.Index - b.Index);
    plutusDataObj.Fields.forEach(f => {
        datumFields.add(PlutusFieldToPlutusData(f) as PlutusData)
    })

    return Cardano.PlutusData.new_constr_plutus_data(
        Cardano.ConstrPlutusData.new(
            Cardano.Int.new_i32(plutusDataObj.ConstructorIndex),
            datumFields
        )
    );
}

function PlutusFieldToPlutusData (field: PlutusField) {
    let plutusData: PlutusData | undefined = undefined;
    switch (field.Type) {
        case PlutusFieldType.Integer:
            plutusData = Cardano.PlutusData.new_integer(Cardano.BigInt.from_str(field.Value.toString()));
            break;
        case PlutusFieldType.Bytes:
            plutusData = Cardano.PlutusData.new_bytes(Helpers.FromHex(field.Value));
            break;
        case PlutusFieldType.Data:
            plutusData = ToPlutusData(field.Value) as PlutusData;
            break;
        case PlutusFieldType.List:
            let elements = Cardano.PlutusList.new();
            field.Value.forEach((el: PlutusField) => {
                elements.add(PlutusFieldToPlutusData(el) as PlutusData)
            });
            plutusData = Cardano.PlutusData.new_list(elements);
            break;
    }

    return plutusData;
}

const Main = async () => {
    await InitializeAsync();
    console.log(PublicAddress);
    // await BootstrapShadowHsAsync();
    console.log(HsHelpers.GetShHsMintString(1,20));
}

Main();