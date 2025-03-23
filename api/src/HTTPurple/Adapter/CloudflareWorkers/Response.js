/**
 * @param {Request} req
 * @returns 
 */
export const mkResponseImpl = (body, opts) => {
  return new Response(body, opts)
}