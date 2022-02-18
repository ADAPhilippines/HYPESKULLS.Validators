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
import { HsVaporTokenType, getTokenName } from "./Types/HsVaporTokenType";
import * as Readline from "readline";
import * as fs from "fs";
import csv from "csv-parser";
import HsShufflerResultRow from "./Types/HsShufflerResultRow";
import sha256 from 'crypto-js/sha256';

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
let IsBootstrapping: boolean = false;
let Interface: Readline.Interface;
let ShufflerResult: HsShufflerResultRow[] = [];
let PrevUtxosHash: string = "";

const InitializeAsync = async () => {
    await LoadCardanoAsync();
    Network = NETWORK_NAME == "testnet" ? Cardano.NetworkInfo.testnet().network_id() : Cardano.NetworkInfo.mainnet().network_id();
    Blockfrost = new BlockFrostAPI({
        projectId: BLOCKFROST_PROJECT_ID, // see: https://blockfrost.io
    });
    PublicAddress = GetPublicAddress();

    Interface = Readline.createInterface({
        input: process.stdin,
        output: process.stdout
    });

    Interface.on('line', async (line: string) => {
        if (!IsBootstrapping) {
            IsBootstrapping = true;
            var type: HsVaporTokenType = parseInt(line[0]);
            await BootstrapContractAsync(type, 1, 1500);
        }
    });

    CccSignalRConnection = new HubConnectionBuilder()
        .withUrl("https://coordinator.seehype.com/vapor")
        .withAutomaticReconnect()
        .build();

    CccSignalRConnection.serverTimeoutInMilliseconds = 2 * 60 * 60 * 1000;

    CccSignalRConnection.start();

    CccSignalRConnection.on("TxSubmitSuccess", async (txId) => {
        console.log("Tx submitted:", txId);
        await WaitTxConfirmedAsync(txId);
        IsConfirmingTx = false;
    });

    CccSignalRConnection.on("SubmitTxError", () => {
        console.log("Error in submitting Tx");
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
    let utxosHash = "";
    let utxosResults: any;

    do {
        utxosResults = await Blockfrost.addressesUtxosAll(PublicAddress);
        utxosHash = sha256(JSON.stringify(utxosResults)).toString();
        await Helpers.WaitFor(1000);
    }
    while (PrevUtxosHash == utxosHash);

    PrevUtxosHash = utxosHash;

    let utxos: TransactionUnspentOutput[] = [];
    for (const utxoResult of utxosResults) {
        utxos.push(
            Cardano.TransactionUnspentOutput.new(
                Cardano.TransactionInput.new(
                    Cardano.TransactionHash.from_bytes(Helpers.FromHex(utxoResult.tx_hash)),
                    utxoResult.output_index
                ),
                Cardano.TransactionOutput.new(
                    Cardano.Address.from_bech32(PublicAddress), // use own address since blockfrost does not provide
                    AmountToValue(utxoResult.amount)
                )
            )
        );
    }
    return utxos;
}

const AmountToValue = (amount: CardanoAssetResponse[]) => {
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

const CreateTxBuilder = (protocolParameters: any) => {
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

const LoadShufflerDataAsync = () => {
    return new Promise<void>((resolve, reject) => {
        fs.createReadStream('shuffled.tsv')
            .pipe(csv({ separator: "\t" }))
            .on('data', (data) => ShufflerResult.push({
                VrtTokenName: data.VRT,
                VtTokenName1: data.VT1,
                VtTokenName2: data.VT2
            }))
            .on('end', () => {
                resolve();
            });
    });
}

const BootstrapContractAsync = async (tokenType: HsVaporTokenType, startIdx: number, endIdx: number) => {
    console.log(`Bootstrapping ${startIdx} - ${endIdx} ${getTokenName(tokenType)}s tokens.`);
    while (startIdx <= endIdx) {
        switch (tokenType) {
            case HsVaporTokenType.VtClaimShadowHs:
                startIdx = await SendBootstrapShadowHsTxAsync(startIdx, endIdx);
                break;
            case HsVaporTokenType.VtClaimVrt:
                startIdx = await SendBootstrapVrtTxAsync(startIdx, endIdx);
                break;
            case HsVaporTokenType.VtClaimVt:
                await LoadShufflerDataAsync();
                startIdx = await SendBootstrapVtTxAsync(startIdx, endIdx);
                break;
            case HsVaporTokenType.VaporizeShadowHs:
                startIdx = await SendBootstrapVaporizeShadowHsTxAsync(startIdx, endIdx);
                break;
            case HsVaporTokenType.VaporizePt:
                startIdx = await SendBootstrapVaporizePtTxAsync(startIdx, endIdx);
                break;
            default:
                break;
        }
        while (IsConfirmingTx) {
            await Helpers.WaitFor(10000);
        }
    }
    console.log("Bootstrap finished");
}

const SendBootstrapShadowHsTxAsync = async (startIdx: number = 1, endIdx: number = 30) => {
    const latestBlock = await Blockfrost.blocksLatest();
    const protocolParameters = await Blockfrost.epochsParameters(latestBlock.epoch as number);
    const shDatumObject = VTClaimShDatum();
    const shDatumHash = Cardano.hash_plutus_data(ToPlutusData(shDatumObject));

    const utxos = await GetUtxosAsync();

    let tokenIdx: number = startIdx;
    let transactionOutputs: TransactionOutput[] = [];
    while (tokenIdx <= endIdx && transactionOutputs.length < 100) {
        console.log("HS ", tokenIdx);
        const shOutput = Cardano.TransactionOutput.new(
            VtClaimContractAddress(),
            GetHypeNftsOutput([HsHelpers.GetShadowHsTokenName(tokenIdx).substring(56)])
        );
        shOutput.set_data_hash(shDatumHash);

        transactionOutputs.push(shOutput);
        tokenIdx++;
    }

    console.log("Building Transaction...");
    var transaction = await ConstructTxAsync(utxos, transactionOutputs, protocolParameters);

    console.log("Final tx size: ", (transaction as Transaction).to_bytes().length);
    console.log(`Submiting Tx for Shadow Hypeskulls ${startIdx} - ${tokenIdx - 1}`);

    CccSignalRConnection.send("SubmitVTClaimTx",
        Helpers.ToHex((transaction as Transaction).to_bytes()),
        [shDatumObject]
    );
    IsConfirmingTx = true;

    return tokenIdx;
}

const SendBootstrapVrtTxAsync = async (startIdx: number = 1, endIdx: number = 30) => {
    const latestBlock = await Blockfrost.blocksLatest();
    const protocolParameters = await Blockfrost.epochsParameters(latestBlock.epoch as number);

    const vrtDatumObject = VTClaimVrtDatum("") as PlutusDataObject;
    const vrtDatumHash = Cardano.hash_plutus_data(ToPlutusData(vrtDatumObject) as PlutusData);

    const utxos = await GetUtxosAsync();

    var tokenIdx: number = startIdx;
    var transactionOutputs: TransactionOutput[] = [];
    while (tokenIdx <= endIdx && transactionOutputs.length < 100) {
        console.log("VRT ", tokenIdx);
        const vrtOutput = Cardano.TransactionOutput.new(
            VtClaimContractAddress() as Address,
            GetHypeNftsOutput([HsHelpers.GetVrtTokenName(tokenIdx).substring(56)]) as Value
        );
        vrtOutput.set_data_hash(vrtDatumHash);

        transactionOutputs.push(vrtOutput);
        tokenIdx++;
    }

    console.log("Building Transaction...");
    var transaction = ConstructTx(utxos, transactionOutputs, protocolParameters);

    console.log("Final tx size: ", (transaction as Transaction).to_bytes().length);
    console.log(`Submiting Tx for Hypeskulls VRT: ${startIdx} - ${tokenIdx - 1}`);

    CccSignalRConnection.send("SubmitVTClaimTx",
        Helpers.ToHex((transaction as Transaction).to_bytes()),
        [vrtDatumObject]
    );
    IsConfirmingTx = true;
    return tokenIdx;
}

const SendBootstrapVtTxAsync = async (startIdx: number = 0, endIdx: number = 30) => {
    const latestBlock = await Blockfrost.blocksLatest();
    const protocolParameters = await Blockfrost.epochsParameters(latestBlock.epoch as number);

    const utxos = await GetUtxosAsync();

    var tokenIdx: number = startIdx;
    var transactionOutputs: TransactionOutput[] = [];
    var datumObjects: PlutusDataObject[] = [];
    while (tokenIdx <= endIdx && transactionOutputs.length < 80) {
        var shufflerResultRow = ShufflerResult[tokenIdx - 1];
        const vtDatumObject = VTClaimVtDatum(sha256(`${shufflerResultRow.VrtTokenName}_${Config.VtClaimNonce}`).toString());
        const vtDatumHash = Cardano.hash_plutus_data(ToPlutusData(vtDatumObject));

        const vtOutput = Cardano.TransactionOutput.new(
            VtClaimContractAddress() as Address,
            GetHypeNftsOutput([
                Helpers.AsciiToHex(shufflerResultRow.VtTokenName1),
                Helpers.AsciiToHex(shufflerResultRow.VtTokenName2)
            ])
        );
        vtOutput.set_data_hash(vtDatumHash);

        datumObjects.push(vtDatumObject);
        transactionOutputs.push(vtOutput);
        tokenIdx++;
    }

    var transaction = ConstructTx(utxos, transactionOutputs, protocolParameters, 3);

    console.log("Final tx size: ", (transaction as Transaction).to_bytes().length);
    console.log(`Submiting Tx for Hypeskulls VT: ${startIdx} - ${tokenIdx - 1}`);

    CccSignalRConnection.send("SubmitVTClaimTx",
        Helpers.ToHex((transaction as Transaction).to_bytes()),
        datumObjects
    );
    IsConfirmingTx = true;
    return tokenIdx;
}

const SendBootstrapVaporizeShadowHsTxAsync = async (startIdx: number = 1, endIdx: number = 30) => {
    const latestBlock = await Blockfrost.blocksLatest();
    const protocolParameters = await Blockfrost.epochsParameters(latestBlock.epoch as number);
    const shDatumObject = VaporizeShDatum();
    const shDatumHash = Cardano.hash_plutus_data(ToPlutusData(shDatumObject));

    const utxos = await GetUtxosAsync();

    let tokenIdx: number = startIdx;
    let transactionOutputs: TransactionOutput[] = [];
    while (tokenIdx <= endIdx && transactionOutputs.length < 100) {
        console.log("HS ", tokenIdx);
        const shOutput = Cardano.TransactionOutput.new(
            VaporizeContractAddress(),
            GetHypeNftsOutput([HsHelpers.GetShadowHsTokenName(tokenIdx).substring(56)])
        );
        shOutput.set_data_hash(shDatumHash);

        transactionOutputs.push(shOutput);
        tokenIdx++;
    }

    console.log("Building Transaction...");
    var transaction = await ConstructTxAsync(utxos, transactionOutputs, protocolParameters);

    console.log("Final tx size: ", (transaction as Transaction).to_bytes().length);
    console.log(`Submiting Tx for Shadow Hypeskulls ${startIdx} - ${tokenIdx - 1}`);

    CccSignalRConnection.send("SubmitVaporizeTx",
        Helpers.ToHex((transaction as Transaction).to_bytes()),
        [shDatumObject]
    );
    IsConfirmingTx = true;

    return tokenIdx;
}

const SendBootstrapVaporizePtTxAsync = async (startIdx: number = 1, endIdx: number = 30) => {
    const latestBlock = await Blockfrost.blocksLatest();
    const protocolParameters = await Blockfrost.epochsParameters(latestBlock.epoch as number);
    const ptDatumObject = VaporizePtDatum();
    const ptDatumHash = Cardano.hash_plutus_data(ToPlutusData(ptDatumObject));

    const utxos = await GetUtxosAsync();

    let tokenIdx: number = startIdx;
    let transactionOutputs: TransactionOutput[] = [];
    while (tokenIdx <= endIdx && transactionOutputs.length < 100) {
        console.log("PT ", tokenIdx);
        const shOutput = Cardano.TransactionOutput.new(
            VaporizeContractAddress(),
            GetHypeNftsOutput([Helpers.AsciiToHex("HYPESKULLS_PT")])
        );
        shOutput.set_data_hash(ptDatumHash);

        transactionOutputs.push(shOutput);
        tokenIdx++;
    }

    console.log("Building Transaction...");
    var transaction = await ConstructTxAsync(utxos, transactionOutputs, protocolParameters);

    console.log("Final tx size: ", (transaction as Transaction).to_bytes().length);
    console.log(`Submiting Tx for Shadow Hypeskulls ${startIdx} - ${tokenIdx - 1}`);

    CccSignalRConnection.send("SubmitVaporizeTx",
        Helpers.ToHex((transaction as Transaction).to_bytes()),
        [ptDatumObject]
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

const ConstructTxAsync = async (utxos: TransactionUnspentOutput[], outputs: TransactionOutput[], protocolParameters: any) => {
    return new Promise<Transaction | null>((resolve, reject) => {
        resolve(ConstructTx(utxos, outputs, protocolParameters));
    });
}

const ConstructTx = (utxos: TransactionUnspentOutput[], outputs: TransactionOutput[], protocolParameters: any, num_inputs: number = 1) => {
    try {
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
            num_inputs
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
    catch (error) {
        console.error(error);
        return null;
    }
}

const SignTx = (tx: Transaction) => {
    const rootKey = GetRootKey(Helpers.GetEntropy(seed_mnemonic));
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

const VaporizeContractAddress = () => Cardano.Address.from_bech32(Config.VaporizeContractAddress);

//#region Datum Builders
const VTClaimShDatum = () => {
    const datum = new PlutusDataObject(0);
    datum.Fields = [];
    return datum;
}

const VTClaimVrtDatum = (pkh: string = "") => {
    const datum = new PlutusDataObject(1);
    datum.Fields = [
        {
            Index: 1,
            Type: PlutusFieldType.Bytes,
            Key: "pkh",
            Value: pkh
        } as PlutusField
    ];
    return datum;
}

const VTClaimVtDatum = (hash: string = "") => {
    const datum = new PlutusDataObject(2);
    datum.Fields = [
        {
            Index: 2,
            Type: PlutusFieldType.Bytes,
            Key: "vrtHash",
            Value: hash
        } as PlutusField
    ];
    return datum;
}

const VaporizeShDatum = (pkh: string = "", orders: number = 0, delivered: number = 0) => {
    const datum = new PlutusDataObject(0);
    datum.Fields = [
        {
            Index: 0,
            Type: PlutusFieldType.Data,
            Key: "vaporizeList",
            Value: VaporizeList(pkh, orders, delivered)
        } as PlutusField,
    ]
    return datum;
}

const VaporizeList = (pkh: string = "", orders: number = 0, delivered: number = 0) => {
    const datum = new PlutusDataObject(0);
    datum.Fields = [
        {
            Index: 0,
            Type: PlutusFieldType.Bytes,
            Key: "pkh",
            Value: pkh
        } as PlutusField,
        {
            Index: 0,
            Type: PlutusFieldType.Integer,
            Key: "orders",
            Value: orders
        } as PlutusField,
        {
            Index: 0,
            Type: PlutusFieldType.Integer,
            Key: "delivered",
            Value: delivered
        } as PlutusField,
    ]
    return datum;
}

const VaporizePtDatum = (price: number = 70) => {
    const datum = new PlutusDataObject(1);
    datum.Fields = [
        {
            Index: 0,
            Type: PlutusFieldType.Integer,
            Key: "price",
            Value: price
        } as PlutusField
    ]
    return datum;
}
//#endregion


//#region Plutus Data Helpers
function ToPlutusData(plutusDataObj: PlutusDataObject) {
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

function PlutusFieldToPlutusData(field: PlutusField) {
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
//#endregion 

const Main = async () => {
    console.log("H.Y.P.E. VAPOR Bootstrapper Started!");
    await InitializeAsync();
    await Helpers.WaitFor(1000);
    console.log("Bootstrapper Wallet Addres: ", PublicAddress);
    console.log("VTClaim Contract Addres: ", Config.VtClaimContractAddress);
    console.log("Select Token Type To Bootstrap:");
    console.log("0 - VTClaim Shadow HS Tokens |  1 - VTClaim VRT Tokens | 2 - VTClaim VT Tokens");

    // console.log(HsHelpers.GetHsMintString(1331,1350));
    // const utxos = await GetUtxosAsync();

    // utxos.forEach(utxo => {
    //     var txId = Helpers.ToHex(utxo.input().transaction_id().to_bytes());
    //     console.log(txId, utxo.input().index(), utxo.output().amount().coin().to_str());
    // });
}

Main();