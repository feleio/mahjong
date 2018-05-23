#!/usr/bin/env python3

import json
from http.server import BaseHTTPRequestHandler, HTTPServer, SimpleHTTPRequestHandler
from urllib.parse import urlparse, parse_qs
from src.main.python.main import State, win, draw, pong
from src.main.python.player import Human, RandomP


def get_params(path):
    params_temp = parse_qs(urlparse(path).query)
    params = {}
    for param in params_temp:
        params[param] = params_temp[param][0]
    return params


css = """
<head>
<style>
.button input[type="button"] {
    color:#08233e;
    font:2.4em Futura, ‘Century Gothic’, AppleGothic, sans-serif;
    font-size:70%;
    padding:14px;
    background:url(overlay.png) repeat-x center #ffcc00;
    background-color:rgba(255,204,0,1);
    border:1px solid #ffcc00;
    -moz-border-radius:10px;
    -webkit-border-radius:10px;
    border-radius:10px;
    border-bottom:1px solid #9f9f9f;
    -moz-box-shadow:inset 0 1px 0 rgba(255,255,255,0.5);
    -webkit-box-shadow:inset 0 1px 0 rgba(255,255,255,0.5);
    box-shadow:inset 0 1px 0 rgba(255,255,255,0.5);
    cursor:pointer;
    display:block;
    width:100%;
    }
    </style>
    </head>
"""


# HTTPRequestHandler class
class testHTTPServer_RequestHandler(SimpleHTTPRequestHandler):

    def __init__(self):
        self.state = State()
        self.event = None
        self.players = [Human()] + [RandomP()] * 3
        self.won = False
        self.loop_yield = None

    def loop(self):
        global won
        for nx_ev_act, _, nx_ev_in, nx_ev_pid in self.state.next(self.event):
            if nx_ev_pid == 0:
                yield nx_ev_act, _, nx_ev_in, nx_ev_pid
            if nx_ev_act == win:
                if self.players[nx_ev_pid].on_win(self.state):
                    won = True
                    break
            elif nx_ev_act == draw:
                discarded = self.players[nx_ev_pid].on_draw(self.state, (nx_ev_act, _, nx_ev_in, nx_ev_pid))
                if discarded is not None:
                    assert self.state.hands[nx_ev_pid][discarded] > 0 or discarded == nx_ev_in
                    event = (nx_ev_act, discarded, nx_ev_in, nx_ev_pid)
                    for player in self.players:
                        player.on_event(self.state, event)
                    yield event
                    break
            elif nx_ev_act == pong:
                discarded = self.players[nx_ev_pid].on_pong(self.state, (nx_ev_act, _, nx_ev_in, nx_ev_pid))
                if discarded is not None:
                    assert self.state.hands[nx_ev_pid][discarded] > 0
                    event = (nx_ev_act, discarded, nx_ev_in, nx_ev_pid)
                    for player in self.players:
                        player.on_event(self.state, event)
                    yield event
                    break
            elif nx_ev_act == self.chow:
                discarded = self.players[nx_ev_pid].on_chow(self.state, (nx_ev_act, _, nx_ev_in, nx_ev_pid))
                if discarded is not None:
                    assert self.state.hands[nx_ev_pid][discarded] > 0
                    event = (nx_ev_act, discarded, nx_ev_in, nx_ev_pid)
                    for player in self.players:
                        player.on_event(self.state, event)
                    yield event
                    break

    def do_GET(self):
        # Send response status code
        self.send_response(200)

        # Send headers
        self.send_header('Access-Control-Allow-Origin', '*')
        self.send_header('Content-type', 'text/html')
        self.send_header('Cache-Control', 'no-cache, no-store, must-revalidate')
        self.end_headers()

        params = get_params(self.path)

        if self.loop_yield is None:
            self.loop_yield = self.loop()

        l = next(self.loop_yield, None)

        if l is None:
            self.loop_yield = self.loop()
            l = next(self.loop_yield, None)

        def on_draw(self, state, event: (int, List[int], int, int)) -> int:
            input_str = input(f"on_draw{state.symbol(event[3])} *{symbol[event[2]]}")
            if input_str:
                return int(symbol.index(input_str))
            return event[2]

        def on_win(self, state) -> bool:
            return bool(input(f"on_win{state.symbol()}"))

        def on_pong(self, state, event: (int, List[int], int, int)) -> int:
            input_str = input(f"on_pong{state.symbol(event[3])} !{symbol[event[2]]}")
            if input_str:
                return int(symbol.index(input_str))
            return None

        def on_chow(self, state, event: (int, List[int], int, int)) -> int:
            input_str = input(f"on_chow{state.symbol(event[3])} !{symbol[event[2]]}")
            if input_str:
                return int(symbol.index(input_str))
            return None

        def on_kong(self, state, event: (int, List[int], int, int)) -> int:
            return int(symbol.index(input(f"on_kong{state.symbol(event[3])} !{symbol[event[2]]}")))

        def on_event(self, state, event: (int, List[int], int, int)) -> None:
            print(f"on_event{event} {symbol[event[1]]}")


            # if 'discard' in params:
        # global event
        # event = state.next(players, event)
        # for player in players:
        #     player.on_event(state, event)

        # else:
        #     response = ""
        # Write content as utf-8 data
        # self.wfile.write(bytes(response, "utf8"))




        """
        <form action=""><input type="submit" name="1" value="123"></form>
        """

        self.wfile.write(bytes(f"{css}HELLO <b>asd</b>", "utf8"))
        print(get_params(self.path))
        return


def run():
    print('starting server...')
    server_address = ('0.0.0.0', 8001)

    httpd = HTTPServer(server_address, testHTTPServer_RequestHandler)
    print('running server...')
    httpd.serve_forever()


run()
