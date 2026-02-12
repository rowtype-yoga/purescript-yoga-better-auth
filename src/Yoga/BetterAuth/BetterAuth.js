import { betterAuth } from "better-auth";
import { getMigrations } from "better-auth/db";
import pg from "pg";

export const betterAuthImpl = (opts) => betterAuth(opts);

export const pgPoolImpl = (connectionString) =>
  new pg.Pool({ connectionString });

export const runMigrationsImpl = (auth) =>
  getMigrations(auth.options).then(({ runMigrations }) => runMigrations());

export const handlerImpl = (auth, request) => auth.handler(request);

export const apiImpl = (auth) => auth.api;

export const getSessionImpl = (api, opts) => api.getSession(opts);

export const signInEmailImpl = (api, opts) => api.signInEmail(opts);

export const signUpEmailImpl = (api, opts) => api.signUpEmail(opts);

export const signOutImpl = (api, opts) => api.signOut(opts);
