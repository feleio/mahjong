"use client";

import { useState } from "react";
import { ClientAction, Prompt } from "@/lib/types";
import Tile from "./Tile";

interface Props {
  prompt: Prompt;
  onAct:  (a: ClientAction) => void;
}

export default function PromptPanel({ prompt, onAct }: Props) {
  const [picked, setPicked] = useState<string | null>(null);

  switch (prompt.kind) {
    case "self_win":
      return (
        <div className="prompt">
          <strong>Self-draw win</strong> {prompt.tile && <>with <Tile tile={prompt.tile} small /></>} for score {prompt.score}. Take it?
          <div className="row" style={{ marginTop: 8 }}>
            <button onClick={() => onAct({ kind: "self_win", yes: true })}>Yes, win</button>
            <button className="ghost" onClick={() => onAct({ kind: "self_win", yes: false })}>Pass</button>
          </div>
        </div>
      );
    case "win":
      return (
        <div className="prompt">
          <strong>Win</strong> on opponent's discard {prompt.tile && <Tile tile={prompt.tile} small />} for score {prompt.score}.
          <div className="row" style={{ marginTop: 8 }}>
            <button onClick={() => onAct({ kind: "win", yes: true })}>Yes, win</button>
            <button className="ghost" onClick={() => onAct({ kind: "win", yes: false })}>Pass</button>
          </div>
        </div>
      );
    case "kong":
      return (
        <div className="prompt">
          <strong>Kong</strong> on {prompt.tile && <Tile tile={prompt.tile} small />}?
          <div className="row" style={{ marginTop: 8 }}>
            <button onClick={() => onAct({ kind: "kong", yes: true })}>Kong</button>
            <button className="ghost" onClick={() => onAct({ kind: "kong", yes: false })}>Pass</button>
          </div>
        </div>
      );
    case "pong":
      return (
        <div className="prompt">
          <strong>Pong</strong> on {prompt.tile && <Tile tile={prompt.tile} small />}?
          <div className="row" style={{ marginTop: 8 }}>
            <button onClick={() => onAct({ kind: "pong", yes: true })}>Pong</button>
            <button className="ghost" onClick={() => onAct({ kind: "pong", yes: false })}>Pass</button>
          </div>
        </div>
      );
    case "self_kong": {
      const opts = prompt.selfKongTiles ?? [];
      return (
        <div className="prompt">
          <strong>Self-kong</strong>: pick a tile to kong, or pass.
          <div className="row" style={{ marginTop: 8 }}>
            {opts.map((t) => (
              <button key={t} onClick={() => onAct({ kind: "self_kong", tile: t })}>
                Kong {t}
              </button>
            ))}
            <button className="ghost" onClick={() => onAct({ kind: "self_kong" })}>Pass</button>
          </div>
        </div>
      );
    }
    case "chow": {
      const positions = (prompt.chowPositions ?? []) as Array<"LEFT" | "MIDDLE" | "RIGHT">;
      return (
        <div className="prompt">
          <strong>Chow</strong> on {prompt.tile && <Tile tile={prompt.tile} small />}?
          <div className="row" style={{ marginTop: 8 }}>
            {positions.map((p) => (
              <button key={p} onClick={() => onAct({ kind: "chow", chowPos: p })}>
                Chow {p}
              </button>
            ))}
            <button className="ghost" onClick={() => onAct({ kind: "chow" })}>Pass</button>
          </div>
        </div>
      );
    }
    case "discard": {
      const tiles = prompt.handTiles ?? [];
      return (
        <div className="prompt">
          <strong>Your turn — pick a tile to discard.</strong>
          <div className="hand" style={{ marginTop: 8 }}>
            {tiles.map((t, i) => (
              <Tile
                key={`${t}-${i}`}
                tile={t}
                onClick={() => setPicked(t)}
                highlight={picked === t}
              />
            ))}
          </div>
          <div className="row" style={{ marginTop: 8 }}>
            <button
              disabled={!picked}
              onClick={() => picked && onAct({ kind: "discard", tile: picked })}
            >
              Discard {picked ?? "…"}
            </button>
          </div>
        </div>
      );
    }
    default:
      return null;
  }
}
