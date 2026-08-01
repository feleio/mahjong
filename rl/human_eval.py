#!/usr/bin/env python
"""Champion-vs-human evaluation over recorded server games (issue #30 step 3.1).

Reads finished games from the server's Postgres `game_records` table (written
by GameRecorder since PR #31), recomputes per-seat money with the engine's
payout rule, and reports money/game, win rate and deal-in rate per seat kind —
the first benchmark of the champion outside the bot family.

Payout rule (mirrors WinnersInfo.winnersBalance in Flow.scala):
  self-win:    winner receives 1.5x scoreMap[score], each other seat pays 0.5x
  discard win: each winner receives scoreMap[score], the discarder pays the sum
  draw:        everyone 0

Usage:
  python human_eval.py                     # tables with >=1 human AND >=1 champion
  python human_eval.py --tables human      # any table with >=1 human seat
  python human_eval.py --tables all        # every finished recorded game
  python human_eval.py --json              # machine-readable dump

DB connection defaults to the repo compose Postgres (localhost:5434), override
with MAHJONG_DB_HOST/PORT/NAME/USER/PASSWORD.
"""

import argparse
import json
import math
import os
import sys
from collections import defaultdict

import psycopg2

# Mirrors scoreMap in src/main/resources/application.conf — keep in sync.
SCORE_MAP = {3: 8, 4: 16, 5: 24, 6: 32, 7: 48, 8: 64, 9: 96, 10: 128}


def score_money(score: int) -> int:
    return SCORE_MAP[max(3, min(10, score))]


def seat_money(outcome: dict) -> list:
    """Per-seat money for one game, engine payout rule. outcome is outcome_json."""
    money = [0, 0, 0, 0]
    if outcome.get("drawn"):
        return money
    winners = outcome.get("winners", [])
    if not winners:
        return money
    if outcome.get("isSelfWin"):
        amount = score_money(winners[0]["score"])
        w = winners[0]["seat"]
        for i in range(4):
            money[i] = amount * 3 // 2 if i == w else -amount // 2
    else:
        loser = outcome.get("loserSeat")
        for w in winners:
            money[w["seat"]] += score_money(w["score"])
        if loser is not None:
            money[loser] = -sum(score_money(w["score"]) for w in winners)
    return money


def load_games(conn, tables: str):
    """Yield (game_id, seats, outcome) for finished games matching the filter."""
    with conn.cursor() as cur:
        cur.execute(
            "SELECT id, seats_json, outcome_json FROM game_records "
            "WHERE status = 'finished' AND outcome_json IS NOT NULL "
            "ORDER BY started_at"
        )
        for game_id, seats_json, outcome_json in cur:
            seats = json.loads(seats_json)
            kinds = {s["kind"] for s in seats}
            if tables == "champion-vs-human" and not ({"human", "ai_champion"} <= kinds):
                continue
            if tables == "human" and "human" not in kinds:
                continue
            yield game_id, seats, json.loads(outcome_json)


def ci95(values):
    n = len(values)
    if n < 2:
        return 0.0
    mean = sum(values) / n
    var = sum((v - mean) ** 2 for v in values) / (n - 1)
    return 1.96 * math.sqrt(var / n)


def main():
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("--tables", choices=["champion-vs-human", "human", "all"],
                    default="champion-vs-human",
                    help="which recorded tables to include (default: champion-vs-human)")
    ap.add_argument("--json", action="store_true", help="emit raw JSON instead of a report")
    args = ap.parse_args()

    conn = psycopg2.connect(
        host=os.environ.get("MAHJONG_DB_HOST", "localhost"),
        port=int(os.environ.get("MAHJONG_DB_PORT", "5434")),
        dbname=os.environ.get("MAHJONG_DB_NAME", "mahjong"),
        user=os.environ.get("MAHJONG_DB_USER", "mahjong"),
        password=os.environ.get("MAHJONG_DB_PASSWORD", "mahjong"),
    )

    # per seat-kind and per human-name accumulators of per-game rows
    by_kind = defaultdict(lambda: {"money": [], "wins": 0, "self_wins": 0,
                                   "deal_ins": 0, "draws": 0})
    by_human = defaultdict(lambda: {"money": [], "wins": 0, "deal_ins": 0})
    n_games = 0

    for _game_id, seats, outcome in load_games(conn, args.tables):
        n_games += 1
        money = seat_money(outcome)
        drawn = bool(outcome.get("drawn"))
        winner_seats = {w["seat"] for w in outcome.get("winners", [])}
        loser = outcome.get("loserSeat")
        for s in seats:
            i, kind = s["index"], s["kind"]
            agg = by_kind[kind]
            agg["money"].append(money[i])
            agg["wins"] += i in winner_seats
            agg["self_wins"] += (i in winner_seats) and bool(outcome.get("isSelfWin"))
            agg["deal_ins"] += (loser == i) and not drawn
            agg["draws"] += drawn
            if kind == "human":
                h = by_human[s.get("name", f"seat{i}")]
                h["money"].append(money[i])
                h["wins"] += i in winner_seats
                h["deal_ins"] += (loser == i) and not drawn

    conn.close()

    if n_games == 0:
        print(f"No finished recorded games match --tables {args.tables}. "
              "Play some games on the dogfood deploy first (docs/DEPLOY.md).")
        sys.exit(1)

    def stats(agg):
        n = len(agg["money"])
        mean = sum(agg["money"]) / n
        return {
            "seat_games": n,
            "money_per_game": round(mean, 3),
            "money_ci95": round(ci95(agg["money"]), 3),
            "win_rate": round(agg["wins"] / n, 4),
            "self_win_rate": round(agg.get("self_wins", 0) / n, 4),
            "deal_in_rate": round(agg["deal_ins"] / n, 4),
            "draw_rate": round(agg.get("draws", 0) / n, 4),
        }

    result = {
        "tables": args.tables,
        "games": n_games,
        "by_kind": {k: stats(v) for k, v in sorted(by_kind.items())},
        "by_human": {k: stats(v) for k, v in sorted(by_human.items())},
    }

    if args.json:
        print(json.dumps(result, indent=2))
        return

    print(f"Recorded-game eval — tables: {args.tables}, games: {n_games}\n")
    hdr = f"{'seat kind':<20}{'n':>6}{'money/game':>14}{'win%':>8}{'self%':>8}{'deal-in%':>10}{'draw%':>8}"
    print(hdr)
    print("-" * len(hdr))
    for kind, s in result["by_kind"].items():
        print(f"{kind:<20}{s['seat_games']:>6}"
              f"{s['money_per_game']:>+9.2f}±{s['money_ci95']:<4.2f}"
              f"{100 * s['win_rate']:>7.1f}{100 * s['self_win_rate']:>8.1f}"
              f"{100 * s['deal_in_rate']:>10.1f}{100 * s['draw_rate']:>8.1f}")
    if result["by_human"]:
        print("\nPer human player:")
        for name, s in result["by_human"].items():
            print(f"  {name:<18}{s['seat_games']:>4} games  "
                  f"{s['money_per_game']:>+8.2f}±{s['money_ci95']:.2f}/game  "
                  f"win {100 * s['win_rate']:.1f}%  deal-in {100 * s['deal_in_rate']:.1f}%")
    if args.tables == "champion-vs-human":
        champ = result["by_kind"].get("ai_champion")
        if champ:
            print(f"\nHeadline: champion {champ['money_per_game']:+.2f}"
                  f" ± {champ['money_ci95']:.2f} $/game over"
                  f" {champ['seat_games']} seat-games on human tables.")


if __name__ == "__main__":
    main()
