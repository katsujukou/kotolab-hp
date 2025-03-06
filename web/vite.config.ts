import { defineConfig } from "vite";
import tailwindCss from "@tailwindcss/vite";

export default defineConfig({
  server: {
    port: 5173,
    strictPort: true,
    host: true,
  },
  plugins: [
    tailwindCss(),
  ]
})