import { fetch } from "../output/Kotolab.HP.API.Handler/index";

export default {
  /**
   * 
   * @param {Request} req 
   * @returns 
   */
  async fetch(req, env, ctx) {
    const url = new URL(req.url)
    if (url.pathname.startsWith("/public/")) {
      url.pathname = url.pathname.replace(/^\/public/, "");
    }
    return fetch(new Request(url.toString(), req), env, ctx);
  },
};