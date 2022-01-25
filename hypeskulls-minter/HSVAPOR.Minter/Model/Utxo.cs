namespace HSVapor.Minter.Model;
class Utxo 
{
	public string TxHash { get; set; } = string.Empty;
	public string TxIx { get; set; } = string.Empty;
	public string Amount { get; set; } = string.Empty;
	public uint Lovelace { get; set; }

	public Utxo (string rawUtxo) 
	{
		var utxoParts = rawUtxo.Split("     ");
		if(utxoParts.Length == 3) 
		{
			TxHash = utxoParts[0].Trim();
			TxIx = utxoParts[1].Trim();
			Amount = utxoParts[2].Trim();

			var amounts = Amount.Split("lovelace");
			if(amounts.Length < 1) return;
			Lovelace = uint.Parse(amounts[0].Trim());
		}
	}
}