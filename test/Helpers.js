export const mkRequestImpl = (url, method, headers, body) =>
  new Request(url, { method, headers, body });
