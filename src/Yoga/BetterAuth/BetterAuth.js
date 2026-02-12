import { betterAuth } from "better-auth";

export const betterAuthImpl = (opts) => betterAuth(opts);

export const handlerImpl = (auth, request) => auth.handler(request);

export const apiImpl = (auth) => auth.api;

export const getSessionImpl = (api, opts) => api.getSession(opts);

export const signInEmailImpl = (api, opts) => api.signInEmail(opts);

export const signUpEmailImpl = (api, opts) => api.signUpEmail(opts);

export const signOutImpl = (api, opts) => api.signOut(opts);
