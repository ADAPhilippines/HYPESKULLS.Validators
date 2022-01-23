/**
 * 
 * Ported to TypeScript by Clark Alesna <clark@saib.dev>
 * Founder of SAIB Inc, Founder of ADAPH Cardano Stake Pool
 * 
 * BerryPool implementation of the __Random-Improve__ coin selection algorithm.
 *
 * = Overview
 *
 * The __Random-Improve__ coin selection algorithm works in __two phases__, by
 * /first/ selecting UTxO entries /at random/ to pay for each of the given
 * outputs, and /then/ attempting to /improve/ upon each of the selections.
 *
 * === Phase 1: Random Selection
 *
 * __In this phase, the algorithm randomly selects a minimal set of UTxO__
 * __entries to pay for each of the given outputs.__
 *
 * During this phase, the algorithm:
 *
 *   *  processes outputs in /descending order of coin value/.
 *
 *   *  maintains a /remaining UTxO set/, initially equal to the given
 *      /UTxO set/ parameter.
 *
 *   *  based on every output nature, generate a /native token UTxO subset/
 *      to narrow down to useful UTxO
 *
 *   *  maintains an /accumulated coin selection/, which is initially /empty/.
 *
 * For each output of value __/v/__, the algorithm /randomly/ selects entries
 * from the /remaining UTxO set/, until the total value of selected entries is
 * greater than or equal to __/v/__. The selected entries are then associated
 * with that output, and removed from the /remaining UTxO set/.
 *
 * This phase ends when every output has been associated with a selection of
 * UTxO entries.
 *
 * However, if the remaining UTxO set is completely exhausted before all
 * outputs can be processed, the algorithm terminates with an error.
 *
 * === Phase 2: Improvement
 *
 * __In this phase, the algorithm attempts to improve upon each of the UTxO__
 * __selections made in the previous phase, by conservatively expanding the__
 * __selection made for each output.__
 *
 * During this phase, the algorithm:
 *
 *   *  processes outputs in /ascending order of coin value/.
 *
 *   *  continues to maintain the /remaining UTxO set/ produced by the previous
 *      phase.
 *
 *   *  maintains an /accumulated coin selection/, initiated from previous phase.
 *
 * For each output of value __/v/__, the algorithm:
 *
 *  1.  __Calculates a /target range/__ for the total value of inputs used to
 *      pay for that output, defined by the triplet:
 *
 *      (/minimum/, /ideal/, /maximum/) = (/v/, /2v/, /3v/)
 *
 *  2.  __Attempts to /improve/ upon the /existing UTxO selection/__ for that
 *      output, by repeatedly selecting additional entries at random from the
 *      /remaining UTxO set/, stopping when the selection can be improved upon
 *      no further.
 *
 *      A selection with value /v1/ is considered to be an /improvement/ over a
 *      selection with value /v0/ if __all__ of the following conditions are
 *      satisfied:
 *
 *       * __Condition 1__: we have moved closer to the /ideal/ value:
 *
 *             abs (/ideal/ − /v1/) < abs (/ideal/ − /v0/)
 *
 *       * __Condition 2__: we have not exceeded the /maximum/ value:
 *
 *             /v1/ ≤ /maximum/
 *
 *       * __Condition 3__: when counting cumulatively across all outputs
 *       considered so far, we have not selected more than the /maximum/ number
 *       of UTxO entries specified by 'limit'.
 *
 *  3.  __Creates a /change value/__ for the output, equal to the total value
 *      of the /final UTxO selection/ for that output minus the value /v/ of
 *      that output.
 *
 *  4.  __Updates the /accumulated coin selection/__:
 *
 *       * Adds the /output/ to 'outputs'.
 *       * Adds the /improved UTxO selection/ to 'inputs'.
 *       * Adds the /change value/ to 'change'.
 *
 * This phase ends when every output has been processed, __or__ when the
 * /remaining UTxO set/ has been exhausted, whichever occurs sooner.
 *
 * = Termination
 *
 * When both phases are complete, the algorithm terminates.
 *
 * The /accumulated coin selection/ and /remaining UTxO set/ are returned to
 * the caller.
 *
 * === Failure Modes
 *
 * The algorithm terminates with an __error__ if:
 *
 *  1.  The /total value/ of the initial UTxO set (the amount of money
 *      /available/) is /less than/ the total value of the output list (the
 *      amount of money /required/).
 *
 *      See: __'InputsExhaustedError'__.
 *
 *  2.  The /number/ of UTxO entries needed to pay for the requested outputs
 *      would /exceed/ the upper limit specified by 'limit'.
 *
 *      See: __'InputLimitExceededError'__.
 *
 * == Motivating Principles
 *
 * There are several motivating principles behind the design of the algorithm.
 *
 * === Principle 1: Dust Management
 *
 * The probability that random selection will choose dust entries from a UTxO
 * set increases with the proportion of dust in the set.
 *
 * Therefore, for a UTxO set with a large amount of dust, there's a high
 * probability that a random subset will include a large amount of dust.
 *
 * === Principle 2: Change Management
 *
 * Ideally, coin selection algorithms should, over time, create a UTxO set that
 * has /useful/ outputs: outputs that will allow us to process future payments
 * with a minimum number of inputs.
 *
 * If for each payment request of value __/v/__ we create a change output of
 * /roughly/ the same value __/v/__, then we will end up with a distribution of
 * change values that matches the typical value distribution of payment
 * requests.
 *
 * === Principle 3: Performance Management
 *
 * Searching the UTxO set for additional entries to improve our change outputs
 * is /only/ useful if the UTxO set contains entries that are sufficiently
 * small enough. But it is precisely when the UTxO set contains many small
 * entries that it is less likely for a randomly-chosen UTxO entry to push the
 * total above the upper bound.
 */

import {
  TransactionUnspentOutput,
  TransactionOutputs,
  Value,
  MultiAsset,
  Assets,
  BigNum,
} from "./custom_modules/@emurgo/cardano-serialization-lib-nodejs/cardano_serialization_lib";

// const BigInt = window.BigInt;

let protocolParameters = { minUTxO: "", minFeeA: "", minFeeB: "", maxTxSize: "" };

type UTxOSelection = {
  selection: TransactionUnspentOutput[],
  remaining: TransactionUnspentOutput[],
  subset: TransactionUnspentOutput[],
  amount: Value
};

type ImproveRange = {
  ideal: Value,
  maximum: Value
}
/**
 * CoinSelection Module.
 */
const CoinSelection = {
  setProtocolParameters: (minUTxO: string, minFeeA: string, minFeeB: string, maxTxSize: string) => {
    protocolParameters = {
      minUTxO: minUTxO,
      minFeeA: minFeeA,
      minFeeB: minFeeB,
      maxTxSize: maxTxSize,
    };
  },

  randomImprove: (inputs: TransactionUnspentOutput[], outputs: TransactionOutputs, limit: number, preset: TransactionUnspentOutput[] = []) => {
    if (!protocolParameters)
      throw new Error(
        "Protocol parameters not set. Use setProtocolParameters()."
      );

    const _minUTxOValue =
      BigInt(outputs.len()) * BigInt(protocolParameters.minUTxO);

    let amount = Cardano.Value.new(Cardano.BigNum.from_str("0"));

    for (let i = 0; i < preset.length; i++) {
      amount = addAmounts(preset[i].output().amount(), amount);
    }

    let utxoSelection = {
      selection: [...preset], // Shallow copy
      remaining: [...inputs], // Shallow copy
      subset: [] as TransactionUnspentOutput[],
      amount: amount,
    };

    let mergedOutputsAmounts = mergeOutputsAmounts(outputs);

    // Explode amount in an array of unique asset amount for comparison's sake
    let splitOutputsAmounts = splitAmounts(mergedOutputsAmounts);

    // Phase 1: Select enough input
    for (let i = 0; i < splitOutputsAmounts.length; i++) {
      createSubSet(utxoSelection, splitOutputsAmounts[i]); // Narrow down for NatToken UTxO

      utxoSelection = select(
        utxoSelection,
        splitOutputsAmounts[i],
        limit,
        _minUTxOValue
      );
    }

    // Phase 2: Improve
    splitOutputsAmounts = sortAmountList(splitOutputsAmounts);

    for (let i = 0; i < splitOutputsAmounts.length; i++) {
      createSubSet(utxoSelection, splitOutputsAmounts[i]); // Narrow down for NatToken UTxO

      let range: ImproveRange = {
        ideal: Cardano.Value.new(Cardano.BigNum.from_str("0")),
        maximum: Cardano.Value.new(Cardano.BigNum.from_str("0"))
      };

      range.ideal = Cardano.Value.new(
        Cardano.BigNum.from_str("0")
      )
        .checked_add(splitOutputsAmounts[i])
        .checked_add(splitOutputsAmounts[i]);
      range.maximum = Cardano.Value.new(
        Cardano.BigNum.from_str("0")
      )
        .checked_add(range.ideal)
        .checked_add(splitOutputsAmounts[i]);

      improve(
        utxoSelection,
        splitOutputsAmounts[i],
        limit - utxoSelection.selection.length,
        range
      );
    }

    // Insure change hold enough Ada to cover included native assets and fees
    if (utxoSelection.remaining.length > 0) {
      const change = utxoSelection.amount.checked_sub(mergedOutputsAmounts);

      let minAmount = Cardano.Value.new(
        Cardano.min_ada_required(
          change,
          Cardano.BigNum.from_str(protocolParameters.minUTxO)
        )
      );

      const maxFee =
        BigInt(protocolParameters.minFeeA) *
        BigInt(protocolParameters.maxTxSize) +
        BigInt(protocolParameters.minFeeB);

      const maxFeeValue = Cardano.Value.new(
        Cardano.BigNum.from_str(maxFee.toString())
      );

      minAmount = minAmount.checked_add(maxFeeValue);
      const compareResult = compare(change, minAmount);
      if (compareResult !== undefined && compareResult < 0) {
        // Not enough, add missing amount and run select one last time
        const minAda = minAmount
          .checked_sub(Cardano.Value.new(change.coin()))
          .checked_add(Cardano.Value.new(utxoSelection.amount.coin()));

        createSubSet(utxoSelection, minAda);
        utxoSelection = select(utxoSelection, minAda, limit, _minUTxOValue);
      }
    }

    return {
      inputs: utxoSelection.selection,
      output: outputs,
      remaining: utxoSelection.remaining,
      amount: utxoSelection.amount,
      change: utxoSelection.amount.checked_sub(mergedOutputsAmounts),
    };
  },
};

function select(utxoSelection: UTxOSelection, outputAmount: Value, limit: number, minUTxOValue: bigint) {
  try {
    utxoSelection = randomSelect(
      cloneUTxOSelection(utxoSelection), // Deep copy in case of fallback needed
      outputAmount,
      limit - utxoSelection.selection.length,
      minUTxOValue
    );
  } catch (e: any) {
    if (e.message === "INPUT_LIMIT_EXCEEDED") {
      // Limit reached : Fallback on DescOrdAlgo
      utxoSelection = descSelect(
        utxoSelection,
        outputAmount,
        limit - utxoSelection.selection.length,
        minUTxOValue
      );
    } else {
      throw e;
    }
  }

  return utxoSelection;
}

function randomSelect(utxoSelection: UTxOSelection, outputAmount: Value, limit: number, minUTxOValue: bigint): UTxOSelection {
  let nbFreeUTxO = utxoSelection.subset.length;
  // If quantity is met, return subset into remaining list and exit
  if (
    isQtyFulfilled(outputAmount, utxoSelection.amount, minUTxOValue, nbFreeUTxO)
  ) {
    utxoSelection.remaining = [
      ...utxoSelection.remaining,
      ...utxoSelection.subset,
    ];
    utxoSelection.subset = [];
    return utxoSelection;
  }

  if (limit <= 0) {
    throw new Error("INPUT_LIMIT_EXCEEDED");
  }

  if (nbFreeUTxO <= 0) {
    if (isQtyFulfilled(outputAmount, utxoSelection.amount, BigInt(0), 0)) {
      throw new Error("MIN_UTXO_ERROR");
    }
    throw new Error("INPUTS_EXHAUSTED");
  }

  let utxo = utxoSelection.subset
    .splice(Math.floor(Math.random() * nbFreeUTxO), 1)
    .pop() as TransactionUnspentOutput;

  utxoSelection.selection.push(utxo);
  utxoSelection.amount = addAmounts(
    utxo.output().amount(),
    utxoSelection.amount
  );

  return randomSelect(utxoSelection, outputAmount, limit - 1, minUTxOValue);
}

function descSelect(utxoSelection: UTxOSelection, outputAmount: Value, limit: number, minUTxOValue: bigint) {
  // Sort UTxO subset in DESC order for required Output unit type
  utxoSelection.subset = utxoSelection.subset.sort((a, b) => {
    return Number(
      searchAmountValue(outputAmount, b.output().amount()) -
      searchAmountValue(outputAmount, a.output().amount())
    );
  });

  do {
    if (limit <= 0) {
      throw new Error("INPUT_LIMIT_EXCEEDED");
    }

    if (utxoSelection.subset.length <= 0) {
      if (isQtyFulfilled(outputAmount, utxoSelection.amount, BigInt(0), 0)) {
        throw new Error("MIN_UTXO_ERROR");
      }
      throw new Error("INPUTS_EXHAUSTED");
    }

    let utxo = utxoSelection.subset.splice(0, 1).pop() as TransactionUnspentOutput;

    utxoSelection.selection.push(utxo);
    utxoSelection.amount = addAmounts(
      utxo.output().amount(),
      utxoSelection.amount
    );

    limit--;
  } while (
    !isQtyFulfilled(
      outputAmount,
      utxoSelection.amount,
      minUTxOValue,
      utxoSelection.subset.length - 1
    )
  );

  utxoSelection.remaining = [
    ...utxoSelection.remaining,
    ...utxoSelection.subset,
  ];
  utxoSelection.subset = [];

  return utxoSelection;
}

function improve(utxoSelection: UTxOSelection, outputAmount: Value, limit: number, range: ImproveRange) {
  let nbFreeUTxO = utxoSelection.subset.length;

  let compareResult = compare(utxoSelection.amount, range.ideal);
  if (compareResult !== undefined && compareResult >= 0 ||
    nbFreeUTxO <= 0 ||
    limit <= 0
  ) {
    // Return subset in remaining
    utxoSelection.remaining = [
      ...utxoSelection.remaining,
      ...utxoSelection.subset,
    ];
    utxoSelection.subset = [];

    return;
  }

  const utxo = utxoSelection.subset
    .splice(Math.floor(Math.random() * nbFreeUTxO), 1)
    .pop() as TransactionUnspentOutput;

  const newAmount = Cardano.Value.new(
    Cardano.BigNum.from_str("0")
  )
    .checked_add(utxo.output().amount())
    .checked_add(outputAmount);

  compareResult = compare(newAmount, range.maximum);
  if (
    abs(getAmountValue(range.ideal) - getAmountValue(newAmount)) <
    abs(getAmountValue(range.ideal) - getAmountValue(outputAmount)) &&
    compareResult !== undefined && compareResult <= 0
  ) {
    utxoSelection.selection.push(utxo);
    utxoSelection.amount = addAmounts(
      utxo.output().amount(),
      utxoSelection.amount
    );
    limit--;
  } else {
    utxoSelection.remaining.push(utxo);
  }

  improve(utxoSelection, outputAmount, limit, range);
}

function mergeOutputsAmounts(outputs: TransactionOutputs) {
  let compiledAmountList = Cardano.Value.new(
    Cardano.BigNum.from_str("0")
  );

  for (let i = 0; i < outputs.len(); i++) {
    compiledAmountList = addAmounts(
      outputs.get(i).amount(),
      compiledAmountList
    );
  }

  return compiledAmountList;
}

function addAmounts(amounts: Value, compiledAmounts: Value) {
  return compiledAmounts.checked_add(amounts);
}

function splitAmounts(amounts: Value): Value[] {
  let splitAmounts = [];

  if (amounts.multiasset()) {
    let mA = amounts.multiasset() as MultiAsset;

    for (let i = 0; i < mA.keys().len(); i++) {
      let scriptHash = mA.keys().get(i);

      for (let j = 0; j < (mA.get(scriptHash) as Assets).keys().len(); j++) {
        let _assets = Cardano.Assets.new();
        let assetName = (mA.get(scriptHash) as Assets).keys().get(j);

        _assets.insert(
          Cardano.AssetName.from_bytes(assetName.to_bytes()),
          Cardano.BigNum.from_bytes(
            ((mA.get(scriptHash) as Assets).get(assetName) as BigNum).to_bytes()
          )
        );

        let _multiasset = Cardano.MultiAsset.new();
        _multiasset.insert(
          Cardano.ScriptHash.from_bytes(scriptHash.to_bytes()),
          _assets
        );
        let _value = Cardano.Value.new(
          Cardano.BigNum.from_str("0")
        );
        _value.set_multiasset(_multiasset);

        splitAmounts.push(_value);
      }
    }
  }

  // Order assets by qty DESC
  splitAmounts = sortAmountList(splitAmounts, "DESC");

  // Insure lovelace is last to account for min ada requirement
  splitAmounts.push(
    Cardano.Value.new(
      Cardano.BigNum.from_bytes(amounts.coin().to_bytes())
    )
  );

  return splitAmounts;
}

function sortAmountList(amountList: Value[], sortOrder = "ASC") {
  return amountList.sort((a, b) => {
    let sortInt = sortOrder === "DESC" ? BigInt(-1) : BigInt(1);
    return Number((getAmountValue(a) - getAmountValue(b)) * sortInt);
  });
}

function getAmountValue(amount: Value) {
  let val = BigInt(0);
  let lovelace = BigInt(amount.coin().to_str());

  const ma = amount.multiasset();
  if (lovelace > 0) {
    val = lovelace;
  } else if (ma && ma.len() > 0) {
    let scriptHash = ma.keys().get(0);
    let assetName = (ma.get(scriptHash) as Assets).keys().get(0);
    val = BigInt(((ma.get(scriptHash) as Assets).get(assetName) as BigNum).to_str());
  }

  return val;
}

function searchAmountValue(needle: Value, haystack: Value) {
  let val = BigInt(0);
  let lovelace = BigInt(needle.coin().to_str());
  const needleMA = needle.multiasset();
  const haystackMA = haystack.multiasset();
  if (lovelace > 0) {
    val = BigInt(haystack.coin().to_str());
  } else if (
    needleMA &&
    haystackMA &&
    needleMA.len() > 0 &&
    haystackMA.len() > 0
  ) {
    let scriptHash = needleMA.keys().get(0);
    let assetName = (needleMA.get(scriptHash) as Assets).keys().get(0);
    val = BigInt(((haystackMA.get(scriptHash) as Assets).get(assetName) as BigNum).to_str());
  }

  return val;
}

function createSubSet(utxoSelection: UTxOSelection, output: Value) {
  if (BigInt(output.coin().to_str()) < BigInt(1)) {
    let subset = [];
    let remaining = [];
    for (let i = 0; i < utxoSelection.remaining.length; i++) {
      if (
        compare(utxoSelection.remaining[i].output().amount(), output) !==
        undefined
      ) {
        subset.push(utxoSelection.remaining[i]);
      } else {
        remaining.push(utxoSelection.remaining[i]);
      }
    }
    utxoSelection.subset = subset;
    utxoSelection.remaining = remaining;
  } else {
    utxoSelection.subset = utxoSelection.remaining.splice(
      0,
      utxoSelection.remaining.length
    );
  }
}

function isQtyFulfilled(
  outputAmount: Value,
  cumulatedAmount: Value,
  minUTxOValue: bigint,
  nbFreeUTxO: number
) {
  let amount = outputAmount;

  if (minUTxOValue && BigInt(outputAmount.coin().to_str()) > 0) {
    let minAmount = Cardano.Value.new(
      Cardano.min_ada_required(
        cumulatedAmount,
        Cardano.BigNum.from_str(minUTxOValue.toString())
      )
    );

    // Lovelace min amount to cover assets and number of output need to be met
    let compareResult = compare(cumulatedAmount, minAmount);
    if (compareResult !== undefined && compareResult < 0) return false;

    // If requested Lovelace lower than minAmount, plan for change
    compareResult = compare(outputAmount, minAmount)
    if (compareResult !== undefined && compareResult < 0) {
      amount = minAmount.checked_add(
        Cardano.Value.new(
          Cardano.BigNum.from_str(protocolParameters.minUTxO)
        )
      );
    }

    // Try covering the max fees
    if (nbFreeUTxO > 0) {
      const maxFee =
        BigInt(protocolParameters.minFeeA) *
        BigInt(protocolParameters.maxTxSize) +
        BigInt(protocolParameters.minFeeB);

      let maxFeeValue = Cardano.Value.new(
        Cardano.BigNum.from_str(maxFee.toString())
      );

      amount = amount.checked_add(maxFeeValue);
    }
  }

  let finalCompare = compare(cumulatedAmount, amount)
  return finalCompare !== undefined && finalCompare >= 0;
}

function cloneUTxOSelection(utxoSelection: UTxOSelection) {
  return {
    selection: cloneUTxOList(utxoSelection.selection),
    remaining: cloneUTxOList(utxoSelection.remaining),
    subset: cloneUTxOList(utxoSelection.subset),
    amount: cloneValue(utxoSelection.amount),
  };
}

const cloneUTxOList = (utxoList: TransactionUnspentOutput[]) =>
  utxoList.map((utxo) =>
    Cardano.TransactionUnspentOutput.from_bytes(utxo.to_bytes())
  );

const cloneValue = (value: Value) => Cardano.Value.from_bytes(value.to_bytes());

// Helper
function abs(big: bigint) {
  return big < 0 ? big * BigInt(-1) : big;
}

function compare(group: Value, candidate: Value) {
  let gQty = BigInt(group.coin().to_str());
  let cQty = BigInt(candidate.coin().to_str());

  const candidateMA = candidate.multiasset();
  if (candidateMA) {
    let cScriptHash = candidateMA.keys().get(0);
    let cAssetName = (candidateMA.get(cScriptHash) as Assets).keys().get(0);
    const groupMA = group.multiasset();
    if (groupMA && groupMA.len()) {
      const groupAssets = groupMA.get(cScriptHash);
      if (
        groupAssets &&
        groupAssets.get(cAssetName)
      ) {
        gQty = BigInt(
          (groupAssets.get(cAssetName) as BigNum).to_str()
        );
        cQty = BigInt(
          ((candidateMA.get(cScriptHash) as Assets).get(cAssetName) as BigNum).to_str()
        );
      } else {
        return undefined;
      }
    } else {
      return undefined;
    }
  }

  return gQty >= cQty ? (gQty === cQty ? 0 : 1) : -1;
}

let Cardano: typeof import("./custom_modules/@emurgo/cardano-serialization-lib-nodejs/cardano_serialization_lib");
const setCardanoSerializationLib = (_cardano: typeof import("./custom_modules/@emurgo/cardano-serialization-lib-nodejs/cardano_serialization_lib")) => {
  Cardano = _cardano;
};

export { CoinSelection, setCardanoSerializationLib };