/** @type {import('next').NextConfig} */
const nextConfig = {
  reactStrictMode: true,
  // Self-contained server bundle for the Docker image (web/Dockerfile)
  output: "standalone",
};

module.exports = nextConfig;
