import { test, expect } from "@playwright/test";
import { mkdirSync } from "node:fs";

const shotsDir = "docs/screenshots";
mkdirSync(shotsDir, { recursive: true });

/**
 * E2E smoke covering the full host flow:
 *   • visit lobby and set a display name
 *   • create a room (host on seat 1)
 *   • set seats 2-4 to AI bots
 *   • start the game when full
 *   • play a discard turn over the WebSocket
 */
test("host creates a room with 3 AI bots and plays a turn", async ({ page }) => {
  await page.goto("/");

  await expect(page.getByRole("heading", { name: /mahjong/i })).toBeVisible();
  await page.getByPlaceholder(/e\.g\. alice/i).fill("Alice");
  await page.locator('input[value="My Mahjong Table"]').fill("E2E Test Room");
  await page.screenshot({ path: `${shotsDir}/01-lobby.png`, fullPage: true });

  await page.getByRole("button", { name: /create.*host/i }).click();
  await expect(page).toHaveURL(/\/rooms\/[0-9a-f-]+/);

  // Configure seats 2-4 (only the host can change them).
  const selects = page.locator("select");
  await expect(selects).toHaveCount(3);
  await selects.nth(0).selectOption("ai_chicken");
  await selects.nth(1).selectOption("ai_random");
  await selects.nth(2).selectOption("ai_first_felix");
  await page.screenshot({ path: `${shotsDir}/02-room-configured.png`, fullPage: true });

  const startBtn = page.getByRole("button", { name: /^start game$/i });
  await expect(startBtn).toBeEnabled();
  await startBtn.click();

  // Game view loads.
  await expect(page.getByRole("heading", { name: /^table$/i })).toBeVisible({ timeout: 15_000 });
  await expect(page.locator("text=Your hand")).toBeVisible();
  await expect(page.getByText(/your turn — pick a tile to discard/i)).toBeVisible({ timeout: 15_000 });
  await page.screenshot({ path: `${shotsDir}/03-game-prompt.png`, fullPage: true });

  // Click a tile and discard it via the websocket.
  const promptTiles = page.locator(".prompt .hand .tile.action");
  await expect(promptTiles.first()).toBeVisible();
  await promptTiles.first().click();
  await page.getByRole("button", { name: /^discard /i }).click();

  // The AI bots play their turns. Wait for at least one discard tile to appear in the central pile.
  await expect(page.locator(".discards .tile")).not.toHaveCount(0, { timeout: 15_000 });
  await page.screenshot({ path: `${shotsDir}/04-after-turn.png`, fullPage: true });
});
