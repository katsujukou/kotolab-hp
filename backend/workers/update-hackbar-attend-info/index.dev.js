import { trigger } from "../../../output/Kotolab.HP.Worker.UpdateHackbarAttendInfo/index";


export default {
  scheduled: async (evt, env, ctx) => {
    console.log("!!! This is development mode!!!");
    console.log(evt, env, ctx);
    ctx.waitUntil(trigger(evt, env, ctx));
  }
}