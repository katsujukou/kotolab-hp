import Dayjs from "dayjs";

export const now = () => {
  return Dayjs();
}

/** @param {Dayjs.Dayjs} d */
export const dayImpl = (d) => d.day();
/** @param {Dayjs.Dayjs} d */
export const dateImpl = (d) => d.date();
/** @param {Dayjs.Dayjs} d */
export const monthImpl = (d) => d.month() + 1;
/** @param {Dayjs.Dayjs} d */
export const yearImpl = (d) => d.year();

export const parseImpl = (Nothing, Just, str) => {
  const parsed = Dayjs(str);
  if (parsed.isValid()) {
    return Just(parsed);
  }
  return Nothing;
}

/**
 * 
 * @param {string} f 
 * @param {Dayjs.Dayjs} d 
 * @returns 
 */
export const formatImpl = (f, d) => d.format(f);

export const showImpl = (d) => `${d}`;

export const eqImpl = d1 => d2 => JSON.stringify(d1) == JSON.stringify(d2) 