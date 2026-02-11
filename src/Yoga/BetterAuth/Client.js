import { createAuthClient } from "better-auth/client";

export const createAuthClientImpl = (opts) => createAuthClient(opts);

export const clientSignInEmailImpl = (client, body) =>
  client.signIn.email(body);

export const clientSignUpEmailImpl = (client, body) =>
  client.signUp.email(body);

export const clientSignOutImpl = (client) => client.signOut();

export const clientGetSessionImpl = (client) => client.getSession();
