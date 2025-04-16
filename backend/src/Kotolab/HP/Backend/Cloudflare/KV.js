/**
 * 
 * @param {import("@cloudflare/workers-types").KVNamespace} kv 
 */
export const _getJson = async (kv, key) => {
  console.log(key)
  return await kv.get(key, { type: "json" })
}