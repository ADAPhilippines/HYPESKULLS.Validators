namespace HSVapor.Minter.Model;
class NativeAsset 
{
	public string Name { get; set; } = string.Empty;
	public int Supply { get; set; }
	
	public NativeAsset (string name, int supply) 
	{
		Name = name;
		Supply = supply;
	}
}