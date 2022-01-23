import { PlutusField } from "./PlutusField";

export default class PlutusDataObject {

  public ConstructorIndex: number = 0;

  public Fields: PlutusField[] = [];

  constructor(id: number) {
    this.ConstructorIndex = id;
  }

}