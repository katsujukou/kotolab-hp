import { trigger } from "../../../output-es/Kotolab.HP.Worker.UpdateHackbarAttendInfo/index";


export default {
  scheduled: async (evt, env, ctx) => {
    ctx.waitUntil(trigger(evt, env, ctx));
  }
}