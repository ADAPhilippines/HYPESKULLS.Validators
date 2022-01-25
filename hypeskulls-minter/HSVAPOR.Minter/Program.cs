using System.Linq;
using System.Text;
using System.Text.Json;
using CliWrap;
using CliWrap.Buffered;
using HSVapor.Minter.Model;
namespace HSVapor.Minter;
class Program
{
	const bool ON_MAINNET = false;
	const string NETWORK = ON_MAINNET ? "--mainnet" : "--testnet-magic 1097911063";
	const string WORKING_DIRECTORY = "../../../../mint_test/";

	const string POLICY_SCRIPT_PATH = "policy/policy.script";
	const string POLICY_VKEY_PATH = "policy/policy.vkey";
	const string POLICY_SKEY_PATH = "policy/policy.skey";
	const string POLICY_ID_PATH = "policy/policyID";

	const string METADATA_DIRECTORY = $"{WORKING_DIRECTORY}/metadata/";
	const string METADATA_JSON = "vapor_token_metadata.json";
	const string METADATA_PROCESSED_JSON = "processed_metadata.json";
	const string METADATA_TSV = "vapor_token_metadata.tsv";
	const int METADATA_TSV_TOKEN_NAME_COLUMN = 0;
	const int METADATA_TSV_TOTAL_SUPPLY_COLUMN = 11;

	const string WALLET_OUT_ADDR_PATH = $"{WORKING_DIRECTORY}/payment.addr";
	const string WALLET_IN_ADDR_PATH = $"{WORKING_DIRECTORY}/payment.addr";
	
	const string PAYMENT_SKEY = "payment.skey";
	const string PAYMENT_VKEY = "payment.vkey";
	const string TX_RAW = "matx.raw";
	const string TX_SIGNED = "matx.signed";

	const ulong MIN_UTXO_VAPOR_TOKEN = 4_550_000_000;
	const int TX_OUT_BATCH_VAPOR_TOKEN = 30;

	const ulong MIN_UTXO_SH_TOKEN = 220_000_000;
	const int TX_BATCH_SH_TOKEN = 400;
	const int TX_OUT_BATCH_SH_TOKEN = 100;

	const ulong MIN_UTXO_VRT_TOKEN = 220_000_000;
	const int TX_BATCH_VRT_TOKEN= 300;
	const int TX_OUT_BATCH_VRT_TOKEN = 100;

	const int MIN_UTXO_PT_TOKEN = 2_000_000;
	const int TX_BATCH_PT_TOKEN= 1;
	const int TX_OUT_BATCH_PT_TOKEN = 1;

	static async Task Main(string[] args)
	{
		var policyId = (await File.ReadAllTextAsync(Path.Combine(WORKING_DIRECTORY, POLICY_ID_PATH))).Trim();

		var outWalletAddress = "addr_test1vze9knm2zt3sdlse5g7097sqg0y7pr6vmk0nksyw4h44s5s5jqqvy";//await GetWalletAddressAsync(WALLET_OUT_ADDR_PATH);
		if (outWalletAddress is null) return;

		var inWalletAddress = await GetWalletAddressAsync(WALLET_IN_ADDR_PATH);
		if (inWalletAddress is null) return;
		
		if (string.IsNullOrEmpty(policyId))
		{
			Console.WriteLine("Policy ID not found.");
			return;
		}

		await MintVaporAsync(policyId, inWalletAddress, outWalletAddress, MIN_UTXO_VAPOR_TOKEN);

		/**
			Utility tokens for HYPE Vapor Contracts:
			- 1500 Shadow HYPESKULLS - HYPESKULL{XXXX}_SH (non-fungible)
			- 1500 HYPESKULLS Vapor Random Tokens - HYPESKULLS_VRT_{XXXX} (non-fungible)
			- 100 HYPESKULLS Price Tier tokens - HYPESKULLS_PT (fungible)
			where XXXX means zero padded number string from 0 - 1500
		**/
		
		// var nativeAssetsToMintSh = new List<NativeAsset>();
		// for (int i = 1; i <= 1500; i++)
		// {
		// 	nativeAssetsToMintSh.Add(new NativeAsset($"HYPESKULL{i:0000}_SH", 1));
		// }
		// await MintVaporUtilityTokenAsync(policyId, inWalletAddress, outWalletAddress, MIN_UTXO_SH_TOKEN, nativeAssetsToMintSh, TX_BATCH_SH_TOKEN, TX_OUT_BATCH_SH_TOKEN);

		// var nativeAssetsToMintVrt = new List<NativeAsset>();
		// for (int i = 1; i <= 1500; i++)
		// {
		// 	nativeAssetsToMintVrt.Add(new NativeAsset($"HYPESKULLS_VRT_{i:0000}", 1));
		// }
		// await MintVaporUtilityTokenAsync(policyId, inWalletAddress, outWalletAddress, MIN_UTXO_VRT_TOKEN, nativeAssetsToMintVrt, TX_BATCH_VRT_TOKEN, TX_OUT_BATCH_VRT_TOKEN);

		// var nativeAssetsToMintPt = new List<NativeAsset> 
		// {
		// 	new NativeAsset("HYPESKULLS_PT", 100)
		// };
		// await MintVaporUtilityTokenAsync(policyId, inWalletAddress, outWalletAddress, MIN_UTXO_PT_TOKEN, nativeAssetsToMintPt, TX_BATCH_PT_TOKEN, TX_OUT_BATCH_PT_TOKEN);
	}

	static async Task MintVaporUtilityTokenAsync(string policyId, string inWalletAddress, string outWalletAddress, ulong minUtxo, List<NativeAsset> nativeAssetsToMint, int batchPerTx, int batchPerTxOut)
	{
		var totalTx = nativeAssetsToMint.Count / batchPerTx;
		var remaninderCount = nativeAssetsToMint.Count % batchPerTx;
		var nativeAssetToMintBatches = new List<List<NativeAsset>>();

		for (int i = 0; i < totalTx; i++)
		{
			var txOutNativeAssetsBatch = nativeAssetsToMint.GetRange(i * batchPerTx, batchPerTx);
			nativeAssetToMintBatches.Add(txOutNativeAssetsBatch);
		}

		if (remaninderCount is not 0)
		{
			var txNativeAssetsBatch = nativeAssetsToMint.GetRange(totalTx * batchPerTx, remaninderCount);
			nativeAssetToMintBatches.Add(txNativeAssetsBatch);
		}

		foreach (var nativeAssetsBatch in nativeAssetToMintBatches)
		{
			Console.WriteLine($"Minting {nativeAssetsBatch.First().Name} to {nativeAssetsBatch.Last().Name}");
			var walletUtxos = await GetAddressUtxoAsync(inWalletAddress);
			if (walletUtxos is null)
			{
				Console.WriteLine("Failed to retrieve address UTXOs \n");
				return;
			}
			
			var didBuildDraft = await CreateMintingDraftAsync(walletUtxos, inWalletAddress, outWalletAddress, minUtxo, policyId, nativeAssetsBatch, batchPerTxOut, null);
			if (!didBuildDraft) return;

			var signTx = await SignTransactionAsync();
			if (signTx is null) return;

			var didSubmit = await SubmitTransactionAsync();
			if (!didSubmit) return;

			Console.WriteLine("Checking transaction is confirmed.");
			while (!(await IsTransactionConfirmedAsync(signTx, inWalletAddress)))
			{
				await Task.Delay(20000);
			}
			Console.WriteLine("Submitted transaction confirmed.\n");
		}
	}

	static async Task MintVaporAsync(string policyId, string inWalletAddress, string outWalletAddress, ulong minUtxo)
	{
		Console.WriteLine("Minting Vapor tokens");
		// Read TSV File here to get total supply
		var tsvFilePath = Path.Combine(METADATA_DIRECTORY, METADATA_TSV);

		if (!File.Exists(tsvFilePath))
		{
			Console.WriteLine("TSV File not found " + tsvFilePath);
			return;
		}

		var nativeAssetsToMint = new List<NativeAsset>();
		var tsvContent = (await File.ReadAllTextAsync(tsvFilePath)).Trim();
		var tsvItems = tsvContent.Split("\n");
		var tokenItems = new List<List<string>>();

		Console.WriteLine("Reading TSV file.");
		for (var i = 1; i < tsvItems.Length; i++)
		{
			var token = tsvItems[i].Split("\t").ToList();
			tokenItems.Add(token);
			var nativeAsset = new NativeAsset(token[METADATA_TSV_TOKEN_NAME_COLUMN], int.Parse(token[METADATA_TSV_TOTAL_SUPPLY_COLUMN]));
			nativeAssetsToMint.Add(nativeAsset);
		}

		if (!nativeAssetsToMint.Any())
		{
			Console.WriteLine("No Native Assets to mint found. \n");
			return;
		}

		// change values to empty if no metadata
		var unprocessedMetadataJsonFilePath = Path.Combine(METADATA_DIRECTORY, METADATA_JSON);
		var processedMetadataFilePath =  Path.Combine(METADATA_DIRECTORY, METADATA_PROCESSED_JSON);

		if (!string.IsNullOrEmpty(METADATA_JSON))
		{
			await GenerateProcessedMetadataJsonAsync(processedMetadataFilePath, unprocessedMetadataJsonFilePath, policyId);
		}

		var walletUtxos = await GetAddressUtxoAsync(inWalletAddress);
		if (walletUtxos is null)
		{
			Console.WriteLine("Failed to retrieve address UTXOs \n");
			return;
		}

		var didBuildDraft = await CreateMintingDraftAsync(walletUtxos, inWalletAddress, outWalletAddress, minUtxo, policyId, nativeAssetsToMint, TX_OUT_BATCH_VAPOR_TOKEN, processedMetadataFilePath);
		if (!didBuildDraft) return;

		var signTx = await SignTransactionAsync();
		if (signTx is null) return;

		var didSubmit = await SubmitTransactionAsync();
		if (!didSubmit) return;

		Console.WriteLine("Checking transaction is confirmed.");
		while (!(await IsTransactionConfirmedAsync(signTx, inWalletAddress)))
		{
			await Task.Delay(20000);
		}
		Console.WriteLine("Submitted transaction confirmed.");
	}

	static async Task<string?> GetWalletAddressAsync(string addressFilePath)
	{
		var filePath = Path.GetFullPath(addressFilePath);
		if (File.Exists(filePath))
		{
			return (await File.ReadAllTextAsync(filePath)).Trim();
		}
		else
		{
			Console.WriteLine("GetWalletAddressAsync Wallet not found: \n");
			return null;
		}
	}

	static async Task<List<Utxo>?> GetAddressUtxoAsync(string address)
	{
		var result = await Cli.Wrap("cardano-cli")
			.WithArguments(string.Join(" ", new string[] 
			{
				"query",
				"utxo",
				NETWORK,
				$"--address {address}"
			}))
			.WithWorkingDirectory(WORKING_DIRECTORY)
			.ExecuteBufferedAsync();

		var stdOut = result.StandardOutput.ToString();
		var stdErr = result.StandardError.ToString();
		var utxos = new List<Utxo>();
		if (stdErr.Length > 0)
		{
			Console.WriteLine("Get Utxos error Occured: ", stdErr);
			return null;
		}

		var rawUtxos = stdOut.Trim().Split("\n").ToList();
		if (rawUtxos is not null && rawUtxos.Count > 2)
		{
			rawUtxos.RemoveRange(0, 2);
			
			foreach (var rawUtxo in rawUtxos)
			{
				utxos.Add(new Utxo(rawUtxo.Trim()));
			}
			return utxos;
		}

		return null;
	}

	static async Task<string?> GeneratePolicyIdAsync()
	{

		if (!Directory.Exists(Path.Combine(WORKING_DIRECTORY, "policy")))
		{
			Directory.CreateDirectory(Path.Combine(WORKING_DIRECTORY, "policy"));
		}

		var result = (await Cli.Wrap("cardano-cli")
			.WithArguments(string.Join(" ", new string[]
			{
				"address",
				"key-gen",
				$"--verification-key-file {POLICY_VKEY_PATH}",
				$"--signing-key-file {POLICY_SKEY_PATH}"
			}))
			.WithWorkingDirectory(Path.GetFullPath(WORKING_DIRECTORY))
			.ExecuteBufferedAsync()).StandardOutput.Trim();

		var keyHash = (await Cli.Wrap("cardano-cli")
			.WithArguments(string.Join(" ", new string[]
			{
				"address",
				"key-hash",
				$"--payment-verification-key-file {POLICY_VKEY_PATH}"
			}))
			.WithWorkingDirectory(Path.GetFullPath(WORKING_DIRECTORY))
			.ExecuteBufferedAsync()).StandardOutput.Trim();

		var jsonDict = new Dictionary<string, string>
			{
				{ "keyHash", keyHash },
				{ "type", "sig" }
			};

		var policyScriptPath = Path.Combine(WORKING_DIRECTORY, POLICY_SCRIPT_PATH);
		if (File.Exists(policyScriptPath))
			File.Delete(policyScriptPath);

		await File.WriteAllTextAsync(policyScriptPath, JsonSerializer.Serialize(jsonDict, new JsonSerializerOptions { WriteIndented = true }));

		var policyIdResult = (await Cli.Wrap("cardano-cli")
			.WithArguments(string.Join(" ", new string[]
			{
				"transaction",
				"policyid",
				$"--script-file {POLICY_SCRIPT_PATH}"
			}))
			.WithWorkingDirectory(Path.GetFullPath(WORKING_DIRECTORY))
			.ExecuteBufferedAsync());

		var policyIdResultStdOut = policyIdResult.StandardOutput.ToString();
		var policyIdResultStdErr = policyIdResult.StandardError.ToString();

		if (policyIdResultStdErr.Length > 0) 
		{
			Console.WriteLine($"GeneratPolicyIdAsync Error: \n{policyIdResultStdErr}");
			return null;
		}

		if (policyIdResultStdOut.Length > 0)
		{
			Console.WriteLine($"GeneratPolicyIdAsync Result: \n{policyIdResultStdOut}");
			await File.WriteAllTextAsync(Path.Combine(WORKING_DIRECTORY, POLICY_ID_PATH), policyIdResultStdOut.Trim());
			return policyIdResultStdOut.Trim();
		}

		return null;
	}

	static async Task GenerateProcessedMetadataJsonAsync(string outputMetadataOutputpath, string inputMetaDataPath, string policyId)
	{
		if (!File.Exists(Path.GetFullPath(inputMetaDataPath)))
		{
			Console.WriteLine($"Metadata json file to process not found: {inputMetaDataPath}");
			return;
		}

		var policyinputMetaDataPathContent = (await File.ReadAllTextAsync(Path.GetFullPath(inputMetaDataPath))).Trim();
		var processedContent = policyinputMetaDataPathContent.Replace("<policy_id>", policyId);

		if (File.Exists(Path.GetFullPath(outputMetadataOutputpath)))
			File.Delete(Path.GetFullPath(outputMetadataOutputpath));

		await File.WriteAllTextAsync(Path.GetFullPath(outputMetadataOutputpath), processedContent);
	}

	static async Task<bool> CreateMintingDraftAsync(List<Utxo> inputUtxos, string changeAddress, string outAddress, ulong minUtxo, string policyId, List<NativeAsset> nativeAssetsToMint, int batchPerTxOut, string? metadataPath)
	{
		Console.WriteLine("Building minting transaction draft.");

		var nativeAssetsMintText = new List<string>();
		foreach(var nativeAsset in nativeAssetsToMint)
		{
			nativeAssetsMintText.Add($"{nativeAsset.Supply} {policyId}.{GetHexFormattedText(nativeAsset.Name)}");
		}

		var txOutNativeAssetsArgs = new List<string>();
		var totalTxOut = nativeAssetsToMint.Count / batchPerTxOut;
		var remaninderCount = nativeAssetsToMint.Count % batchPerTxOut;

		for (int i = 0; i < totalTxOut; i++)
		{
			var txOutNativeAssetsBatch = nativeAssetsMintText.GetRange(i * batchPerTxOut, batchPerTxOut);
			var txtOutNativeAssetText = string.Join(" + ", txOutNativeAssetsBatch);
			var txOutText = $"--tx-out {outAddress}+{minUtxo}+\"{txtOutNativeAssetText}\"";
			txOutNativeAssetsArgs.Add(txOutText);
		}

		if (remaninderCount is not 0)
		{
			var txOutNativeAssetsBatch = nativeAssetsMintText.GetRange(totalTxOut * batchPerTxOut, remaninderCount);
			var txtOutNativeAssetText = string.Join(" + ", txOutNativeAssetsBatch);
			var txOutText = $"--tx-out {outAddress}+{minUtxo}+\"{txtOutNativeAssetText}\"";
			txOutNativeAssetsArgs.Add(txOutText);
		}

		ulong utxoLovelaceAmount = 0;
		var inputTxHash = string.Empty;
		var nativeAssetsToMintArgText = string.Join(" + ", nativeAssetsMintText);
		foreach (var inUtxo in inputUtxos)
		{
			if (utxoLovelaceAmount < inUtxo.Lovelace)
			{
				utxoLovelaceAmount = inUtxo.Lovelace;
				inputTxHash = $"{inUtxo.TxHash}#{inUtxo.TxIx}";
			}
		}
		var mint = $"\"{nativeAssetsToMintArgText}\"";

		var args = new List<string>
		{
			"transaction",
			"build",
			$"--tx-in {inputTxHash}",
			string.Join(" ", txOutNativeAssetsArgs),
			$"--change-address {changeAddress}",
			$"--mint={mint}",
			$"--minting-script-file {POLICY_SCRIPT_PATH}",
			NETWORK,
			"--witness-override 2",
			$"--out-file {TX_RAW}"
		};

		if (metadataPath is not null)
		{
			if (!File.Exists(metadataPath))
			{
				Console.WriteLine("Mint metadata json file not found.");
				return false;
			}
			args.Add($"--metadata-json-file metadata/{metadataPath.Split("/").Last()}");
		}

		var result = await Cli.Wrap("cardano-cli")
			.WithArguments(string.Join(" ", args))
			.WithWorkingDirectory(WORKING_DIRECTORY)
			.ExecuteBufferedAsync();

		var stdOut = result.StandardOutput.ToString();
		var stdErr = result.StandardError.ToString();

		if (stdErr.Length > 0)
		{
			Console.WriteLine($"CreateMintingDraftAsync Error: \n{stdErr}");
			return false;
		}
		else
			Console.WriteLine(stdOut);
		return true;
	}

	static async Task<string?> SignTransactionAsync()
	{
		Console.WriteLine("Signing minting transaction.");

		if (File.Exists(Path.Combine(WORKING_DIRECTORY, TX_SIGNED)))
			File.Delete(Path.Combine(WORKING_DIRECTORY, TX_SIGNED));

		var result = await Cli.Wrap("cardano-cli")
			.WithArguments(string.Join(" ", new string[]
			{
				"transaction",
				"sign",
				$"--signing-key-file {PAYMENT_SKEY}" ,
				$"--signing-key-file {POLICY_SKEY_PATH}",
				NETWORK,
				$"--tx-body-file {TX_RAW}",
				$"--out-file {TX_SIGNED}"
			}))
			.WithWorkingDirectory(Path.GetFullPath(WORKING_DIRECTORY))
			.ExecuteBufferedAsync();

		if (File.Exists(Path.Combine(WORKING_DIRECTORY, TX_SIGNED)))
		{
			Console.WriteLine("Transaction successfully signed.");

			return (await Cli.Wrap("cardano-cli")
				.WithArguments(string.Join(" ", new string[]
				{
					"transaction",
					"txid",
					$"--tx-file {TX_SIGNED}"
				}))
				.WithWorkingDirectory(Path.GetFullPath(WORKING_DIRECTORY))
				.ExecuteBufferedAsync()).StandardOutput.Trim();
		}
		else
			Console.WriteLine("SignTransactionDraftAsync not able to sign transaction.");
		return null;
	}

	static async Task<bool> SubmitTransactionAsync()
	{
		Console.WriteLine("Submitting signed transaction.");

		var result = await Cli.Wrap("cardano-cli")
			.WithArguments(string.Join(" ",  new string[]
			{
				"transaction",
				"submit",
				$"--tx-file {TX_SIGNED}" ,
				NETWORK
			}))
			.WithWorkingDirectory(Path.GetFullPath(WORKING_DIRECTORY))
			.ExecuteBufferedAsync();

		var stdOut = result.StandardOutput.ToString();
		var stdErr = result.StandardError.ToString();

		if (stdErr.Length > 0)
		{
			Console.WriteLine($"SubmitTransactionAsync Error: \n{stdErr}");
			return false;
		}

		Console.WriteLine(stdOut);
		return true;
	}

	static async Task<bool> IsTransactionConfirmedAsync(string txHash, string inputAddress)
	{
		var isConfirmed = false;
		// get current UTXOs
		var walletUtxos = await GetAddressUtxoAsync(inputAddress);
		if (walletUtxos is null)
		{
			Console.WriteLine("CheckTransactionIsConfirmedAsync Error occured :Invalid wallet address");
			return false;
		}

		foreach (var utxo in walletUtxos)
		{
			if (utxo.TxHash == txHash)
			{
				isConfirmed = true;
				break;
			}
		}

		return isConfirmed;
	}

	static string GetHexFormattedText(string text)
	{
		byte[] bytes = Encoding.Default.GetBytes(text);
		return BitConverter.ToString(bytes).Replace("-", string.Empty);
	}
}

