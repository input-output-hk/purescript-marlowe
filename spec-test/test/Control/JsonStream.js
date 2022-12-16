import { Duplex } from "node:stream";

export const streamFromString = (str) => () => {
  return new Duplex.from(str);
};

// This is not a safe implementation, only useful for testing
class MemoryDuplex extends Duplex {
  constructor(options) {
    super(options);
    this.chunkBuffer = [];
  }

  _write(chunk, _, callback) {
    this.chunkBuffer.push(chunk);
    callback();
  }

  _read() {
    const chunk = this.chunkBuffer.pop();
    if (chunk) {
      this.push(Buffer.from(chunk, "UTF8"));
    } else if (this.writableEnded) {
      this.push(null);
    }
  }
}

export const duplexStream = () => {
  return new MemoryDuplex();
};
