import bigInt from "big-integer";
import JSONbig from "json-bigint";

export function decodeBigInt(fail, succ, json) {
  if (Number.isInteger(json) || typeof json === "bigint") {
    return succ(bigInt(json));
  } else {
    return fail;
  }
}

export function encodeBigInt(a) {
  if (JSON.stringify !== JSONbig.stringify) {
    console.warn(
      "Tried to encode BitInt without patching JSON.stringify. Wrap your app in Data.BigInt.Argonaut.withJsonPatch."
    );
    return a.toJSNumber();
  } else {
    return a.value;
  }
}
