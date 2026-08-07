import type { NextConfig } from "next";

const nextConfig: NextConfig = {
  // the Docker image runs `node server.js` from .next/standalone
  output: "standalone",
};

export default nextConfig;
