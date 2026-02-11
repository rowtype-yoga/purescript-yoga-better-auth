import { betterAuth } from "better-auth";

export const betterAuthImpl = (opts) => betterAuth(opts);

export const handlerImpl = (auth, request) => auth.handler(request);

export const getSessionImpl = (auth, opts) => auth.api.getSession(opts);

export const signInEmailImpl = (auth, body) =>
  auth.api.signInEmail({ body });

export const signUpEmailImpl = (auth, body) =>
  auth.api.signUpEmail({ body });

export const signOutImpl = (auth, opts) => auth.api.signOut(opts);
