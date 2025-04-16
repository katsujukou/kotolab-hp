export const methodImpl = req => req.method
export const urlImpl = req => req.url
/** @param {Request} req  */
export const headersImpl = req => req.headers
/** @param {Request} req  */
export const bodyTextImpl = req => () => req.text()
