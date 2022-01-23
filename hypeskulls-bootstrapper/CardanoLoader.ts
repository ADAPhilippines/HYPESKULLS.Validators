class CardanoLoader {
    public static Cardano: typeof import("./custom_modules/@emurgo/cardano-serialization-lib-nodejs/cardano_serialization_lib") | null = null;

    public static async LoadAsync() {
        if (this.Cardano === null) {
            this.Cardano = await import( "./custom_modules/@emurgo/cardano-serialization-lib-nodejs/cardano_serialization_lib");
        }
        return this.Cardano;
    }
}

export default CardanoLoader;