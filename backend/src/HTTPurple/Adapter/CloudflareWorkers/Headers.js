export const newImpl = (init) => {
  return new Headers(init);
}

/** @param {Headers} headers */
export const appendImpl = (name, value, headers) => {
  return headers.append(name, value)
}
/**
 * @param {Headers} headers
 */
export const entries = (headers) => {
  const hds = {}
  for (const [k, v] of headers.entries()) {
    if (hds[k]) {
      hds[k].push(v)
    }
    else {
      hds[k] = [k, v]
    }
  }
  return Object.values(hds)
}