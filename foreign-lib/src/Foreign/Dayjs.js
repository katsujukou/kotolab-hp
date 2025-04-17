import Dayjs from "dayjs";
import utc from "dayjs/plugin/utc";
import tiemzone from "dayjs/plugin/timezone";

Dayjs.extend(utc);
Dayjs.extend(tiemzone);

export const now = () => {
  return Dayjs();
}

/** @param {Dayjs.Dayjs} d */
export const dayImpl = (d) => d.tz("Asia/Tokyo").day();
/** @param {Dayjs.Dayjs} d */
export const dateImpl = (d) => d.tz("Asia/Tokyo").date();
/** @param {Dayjs.Dayjs} d */
export const monthImpl = (d) => d.tz("Asia/Tokyo").month() + 1;
/** @param {Dayjs.Dayjs} d */
export const yearImpl = (d) => d.tz("Asia/Tokyo").year();

export const parseImpl = (Nothing, Just, str) => {
  const parsed = Dayjs(str);
  if (parsed.isValid()) {
    return Just(parsed);
  }
  return Nothing;
}

/** @param {Dayjs.Dayjs} d  */
export const unsafeSetImpl = (d, method, val) => d[method](val);

/**
 * 
 * @param {string} f 
 * @param {Dayjs.Dayjs} d 
 * @returns 
 */
export const formatImpl = (f, d) => d.tz("Asia/Tokyo").format(f);

export const showImpl = (d) => `${d}`;

export const eqImpl = d1 => d2 => JSON.stringify(d1) == JSON.stringify(d2) 