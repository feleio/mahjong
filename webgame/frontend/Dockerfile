# The player-facing Next.js app (issue #47). NEXT_PUBLIC_* values are inlined
# at build time, so the API/WS bases the *browser* uses are build args:
#   docker build --build-arg NEXT_PUBLIC_API_BASE=http://<host>:8080 \
#                --build-arg NEXT_PUBLIC_WS_BASE=ws://<host>:8080 webgame/frontend/
#
# Uses pnpm (this app has a pnpm lockfile) and Next's standalone output.
# Node 22: the pnpm release corepack pins requires >= 22.13.

FROM node:22-alpine AS deps
WORKDIR /app
RUN corepack enable
COPY package.json pnpm-lock.yaml ./
# --ignore-scripts: pnpm 10 fails the install rather than silently skipping a
# dependency's build step. The two that want one are sharp (raster image
# optimization — this app uses no next/image and ships only SVGs) and
# unrs-resolver (an ESLint resolver, not needed to build or run), so skipping
# them is deliberate rather than a workaround.
RUN pnpm install --frozen-lockfile --ignore-scripts

FROM node:22-alpine AS build
WORKDIR /app
RUN corepack enable
COPY --from=deps /app/node_modules node_modules
COPY . .
ARG NEXT_PUBLIC_API_BASE=http://localhost:8080
ARG NEXT_PUBLIC_WS_BASE=ws://localhost:8080
ENV NEXT_PUBLIC_API_BASE=$NEXT_PUBLIC_API_BASE \
    NEXT_PUBLIC_WS_BASE=$NEXT_PUBLIC_WS_BASE \
    NEXT_TELEMETRY_DISABLED=1
RUN pnpm build

FROM node:22-alpine
WORKDIR /app
ENV NODE_ENV=production NEXT_TELEMETRY_DISABLED=1
COPY --from=build /app/.next/standalone ./
COPY --from=build /app/.next/static .next/static
COPY --from=build /app/public public
EXPOSE 3000
CMD ["node", "server.js"]
