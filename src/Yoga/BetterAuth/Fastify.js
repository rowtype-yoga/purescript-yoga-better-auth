export const registerAuthImpl = (auth, app, opts) => {
  const basePath = opts.basePath || "/api/auth";
  const routePath = basePath + "/*";

  app.route({
    method: ["GET", "POST"],
    url: routePath,
    async handler(request, reply) {
      const url =
        request.protocol + "://" + request.hostname + request.url;
      const headers = new Headers();
      for (const [key, value] of Object.entries(request.headers)) {
        if (value != null) {
          if (Array.isArray(value)) {
            value.forEach((v) => headers.append(key, v));
          } else {
            headers.set(key, value);
          }
        }
      }
      const requestInit = {
        method: request.method,
        headers,
      };
      if (request.method !== "GET" && request.method !== "HEAD") {
        requestInit.body = JSON.stringify(request.body);
        headers.set("content-type", "application/json");
      }
      const req = new Request(url, requestInit);
      const response = await auth.handler(req);
      reply.status(response.status);
      response.headers.forEach((value, key) => {
        reply.header(key, value);
      });
      const text = await response.text();
      return reply.send(text);
    },
  });
};
