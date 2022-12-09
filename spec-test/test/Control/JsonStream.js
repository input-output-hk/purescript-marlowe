import { Duplex } from "node:stream";

export const streamFromString = (str) => () => {
  return new Duplex.from(str);
};
