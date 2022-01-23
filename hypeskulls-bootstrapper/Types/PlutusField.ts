enum PlutusFieldType {
  Data = 0,
  Integer = 1,
  String = 2,
  Bytes = 3,
  List = 4
}

type PlutusField = {
  Index: number,
  Type: PlutusFieldType,
  Key: string,
  Value: any
}

export { PlutusField, PlutusFieldType };