export const mkRequestImpl = (url, method, headers, body) =>
  new Request(url, { method, headers, body });

export const getSetCookieHeaders = (response) => {
  const cookies = response.headers.getSetCookie().map((c) => c.split(";")[0]);
  return cookies.join("; ");
};

export const mkHeadersImpl = (cookieString) => {
  const headers = new Headers();
  headers.set("cookie", cookieString);
  return headers;
};
