import { createAuthClient } from "better-auth/client";

export const createAuthClientImpl = (opts) => createAuthClient(opts);

export const createCookieJarFetchOptionsImpl = (auth) => {
  const cookies = new Map();
  return {
    customFetchImpl: async (url, init) => {
      const headers = new Headers(init?.headers || {});
      if (cookies.size > 0) {
        const parts = [...cookies.entries()].map(([k, v]) => `${k}=${v}`);
        const existing = headers.get("cookie") || "";
        if (existing) parts.unshift(existing);
        headers.set("cookie", parts.join("; "));
      }
      if (!headers.has("origin")) headers.set("origin", auth.options.baseURL);
      const response = await auth.handler(
        new Request(url, { ...init, headers }),
      );
      for (const sc of response.headers.getSetCookie()) {
        const [nameValue] = sc.split(";");
        const eqIdx = nameValue.indexOf("=");
        if (eqIdx > 0) {
          cookies.set(
            nameValue.slice(0, eqIdx).trim(),
            nameValue.slice(eqIdx + 1).trim(),
          );
        }
      }
      return response;
    },
  };
};

export const normalizeErrorImpl = (err) => ({
  message: err?.message ?? "",
  status: err?.status ?? 0,
  statusText: err?.statusText ?? "",
});

export const clientSignUpEmailImpl = (client, body) =>
  client.signUp.email(body);

export const clientSignInEmailImpl = (client, body) =>
  client.signIn.email(body);

export const clientGetSessionImpl = (client) => client.getSession();

export const clientSignOutImpl = (client) => client.signOut();
