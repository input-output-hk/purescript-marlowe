import jsonBigInt from "json-bigint";
import { main } from "./output/Test.SpecMain/index.js";

// We need to patch the JSON.stringify in order for BigInt serialization to work.
const { stringify, parse } = jsonBigInt({ useNativeBigInt: true });

JSON.stringify = stringify;
JSON.parse = parse;

main();
