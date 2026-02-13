import { betterAuth } from "better-auth";

export const betterAuthWithDatabaseImpl = (database, config) =>
  betterAuth({ ...config, database });
