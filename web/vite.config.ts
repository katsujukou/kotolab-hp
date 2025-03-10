import { defineConfig } from "vite";
import tailwindCss from "@tailwindcss/vite";
import * as path from "node:path";

export default defineConfig({
  server: {
    port: 5173,
    strictPort: true,
    host: true,
  },
  plugins: [
    tailwindCss(),
  ],
  resolve: {
    alias: {
      '@assets': path.resolve(__dirname, "assets")
    }
  }
})