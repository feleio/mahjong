#!/usr/bin/env python3

import json
from http.server import BaseHTTPRequestHandler, HTTPServer, SimpleHTTPRequestHandler
from urllib.parse import urlparse, parse_qs
# from src.main.python.main import State
# from src.main.python.player import Human, RandomP

# state = State()
# event = None
# players = [Human()] + [RandomP()] * 3


def get_params(path):
    params_temp = parse_qs(urlparse(path).query)
    params = {}
    for param in params_temp:
        params[param] = params_temp[param][0]
    return params


# HTTPRequestHandler class
class testHTTPServer_RequestHandler(SimpleHTTPRequestHandler):

  # GET
  def do_GET(self):
        # Send response status code
        self.send_response(200)

        # Send headers
        self.send_header('Access-Control-Allow-Origin', '*')
        self.send_header('Content-type','text/html')
        self.end_headers()

        params = get_params(self.path)
        # if 'discard' in params:
            # global event
            # event = state.next(players, event)
            # for player in players:
            #     player.on_event(state, event)

        # else:
        #     response = ""
        # Write content as utf-8 data
        # self.wfile.write(bytes(response, "utf8"))
        self.wfile.write(bytes("HELLO <b>asd</b>", "utf8"))
        print(get_params(self.path))
        return

def run():
  print('starting server...')
  server_address = ('0.0.0.0', 8001)

  httpd = HTTPServer(server_address, testHTTPServer_RequestHandler)
  print('running server...')
  httpd.serve_forever()


run()
