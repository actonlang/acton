#!/usr/bin/env python3

import json
import locale
import logging
import os
import random
import re
import select
import socket
import subprocess
import sys
import time

ACTONDB="../dist/bin/actondb"
BASEPORT=32001


def get_db_args(base_port, replication_factor):
    return [item for sublist in map(lambda x: ("--rts-ddb-host", x), [f"127.0.0.1:{base_port+idx}" for idx in range(replication_factor)]) for item in sublist]


class MonSock:
    """Communicate with the monitor interface of actondb or RTS using netstrings
    encoded payloads
    """
    def __init__(self, address):
        self.address = address
        self.sock = None
        self.buf = b""

    def connect(self):
        self.sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        try:
            self.sock.connect(self.address)
        except Exception as exc:
            pass

    def cmd(self, cmd, retries=5):
        if self.sock is None:
            self.connect()

        # Simple netstrings implementation, which also assumes that there is
        # only one response to our query
        try:
            self.sock.send(f"{len(cmd)}:{cmd},".encode('utf-8'))
            while True:
                try:
                    colpos = self.buf.index(b':')
                except ValueError as exc:
                    # Not enough data, read some and try again
                    recv = self.sock.recv(1024)
                    if len(recv) == 0:
                        raise ConnectionError("RTS hung up")
                    self.buf += recv
                    continue
                length = int(self.buf[0:colpos])
                start = colpos + 1
                end = start + length
                if len(self.buf) < end:
                    # Not enough data, read some and try again
                    recv = self.sock.recv(1024)
                    if len(recv) == 0:
                        raise ConnectionError("RTS hung up")
                    self.buf += recv
                    continue
                res = self.buf[start:end]
                self.buf = self.buf[end+1:] # +1 to skip the ,
                return json.loads(res.decode("utf-8"))
        except Exception as exc:
            self.connect()
            if retries > 0:
                time.sleep(0.01)
                return self.cmd(cmd, retries-1)
            else:
                raise ConnectionError("Unable to get data from acton rts")


class Db:
    """A single DB node
    """
    def __repr__(self):
        return f"Db{self.idx}"

    def __init__(self, idx, base_port=32000):
        self.idx = idx
        self.name = f"Db{idx}"
        self.port = base_port + self.idx
        self.seed_port = base_port + 100
        self.gossip_port = self.seed_port + self.idx
        self.mon_sock = f"db{self.idx}_mon"
        self.logfile = open(f"db{self.idx}.log", "w")
        self.p = None
        self.running = False

    def start(self):
        cmd = [ACTONDB, "--mon-socket-path", self.mon_sock,
                "-p", str(self.port), "-m", str(self.gossip_port),
                "-s", f"127.0.0.1:{self.seed_port}"]
        self.p = subprocess.Popen(cmd, stdout=self.logfile, stderr=self.logfile)
        for i in range(9999):
            if i > 100:
                raise Exception("Unable to get membership")
            try:
                self.get_membership()
                break
            except:
                time.sleep(0.01)


    def stop(self):
        self.p.kill()
        self.p.wait() # wait for the child process to exit


    def get_vc(self):
        data = self.get_membership()
        return data["membership"]["view_id"]

    def get_membership(self):
        ms = MonSock(self.mon_sock)
        return ms.cmd("membership")



class DbCluster:
    def __init__(self, num=3, base_port=None):
        self.num = num
        self.base_port = base_port
        if not self.base_port:
            # compute random base port between 10000 to 60000 in increments of
            # 200 ports, which allows us to run up to 100 DB nodes per test
            self.base_port = random.randint(50, 300) * 200

    def start(self):
        """Start up a cluster of num nodes and ensure that memberships look alright
        """
        log = logging.getLogger()

        log.debug("Starting database servers")
        self.dbs=[Db(0, self.base_port)]
        self.dbs[0].start()

        allgood = True

        for i in range(1, self.num):
            log.debug(f"Starting instance {i}")
            # Create & start new instance
            dbn = Db(i, self.base_port)
            dbn.start()
            self.dbs.append(dbn)

            # Now go through all existing nodes and ensure all have new membership
            expected = None
            for db in self.dbs:
                mbs = db.get_vc()
                #log.debug(f"Membership info for {db}: {mbs}")
                if expected is None:
                    log.debug(f"Using Membership info from {db} as expected: {mbs}")
                    expected = mbs
                else:
                    if mbs == expected:
                        log.debug(f"Membership info for {db} is as expected: {mbs}")
                        pass
                    else:
                        log.error(f"Membership info for {db} seems incorrect: {mbs}")
                        allgood = False
        return allgood


    def stop(self):
        log.debug("Stopping database servers")
        for dbn in self.dbs:
            dbn.stop()
        return True


def test_app_recovery(base_port, replication_factor):
    cmd = ["./test_db_recovery", "--rts-verbose", "--rts-ddb-replication", str(replication_factor)] + get_db_args(base_port, replication_factor)


    def so1(line, p, s):
        log.info(f"App output: {line}")
        m = re.match("COUNT: (\d+)", line)
        if m:
            log.debug(f"Got count: {m.group(1)}")
            if int(m.group(1)) != s["i"]:
                log.error(f"Unexpected output from app, got {line} but expected {i}")
                return True
            if s["i"] == 3:
                log.debug("Waiting somewhat")
                time.sleep(0.1)
                log.debug("Killing application")
                p.terminate()
            s["i"] += 1

        return False

    def so2(line, p, s):
        log.info(f"App output: {line}")
        m = re.match("COUNT: (\d+)", line)
        if m:
            log.debug(f"Got count: {m.group(1)}")
            if int(m.group(1)) == s["i"]:
                log.info(f"App resumed perfectly at {s['i']}")
            elif int(m.group(1)) == s["i"]-1:
                log.info(f"Got higher than {s['i']-1}, deemed ok but seems we failed to snapshot last count?")
            else:
                raise ValueError(f"Unexpected output from app, got {line} but expected {s['i']}")
            s["i"] += 1
        return False

    state = {
        "i": 1
    }

    p, s = run_cmd(cmd, so1, stderr_checker, state=state)

    p, s = run_cmd(cmd, so2, stderr_checker, state=state)


    if p.returncode == 0:
        log.debug("Application exited successfully")
        return True
    else:
        log.error(f"Non-0 return code: {p.returncode}")
        return False


def stderr_checker(line, p, s):
    log.info(f"App stderr: {line}")

    if re.search("Assertion", line):
        raise ValueError(f"Got an assertion: {line}")

    if re.search("ERROR", line):
        raise ValueError(f"ERROR: {line}")

    if re.search("No quorum", line):
        raise ValueError(f"DB Quorum error: {line}")

    return False


def run_cmd(cmd, cb_so=None, cb_se=None, cb_end=None, state=None):
    log.debug(f"Starting application: {' '.join(cmd)}")
    p = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE, universal_newlines=True)
    done = False
    while not done:
        readfds = [p.stdout.fileno(), p.stderr.fileno()]
        rds, _, _ = select.select(readfds, [], [])
        for rd in rds:
            if rd == p.stdout.fileno():
                line = p.stdout.readline().strip()
                if cb_so:
                    cb_so(line, p, state)
            elif rd == p.stderr.fileno():
                line = p.stderr.readline().strip()
                if cb_se:
                    cb_se(line, p, state)

        if p.poll() != None:
            log.info("End of process...")
            break

    o, e = p.communicate()
    log.info(f"p.communicate(): {o}  {e}")

    if cb_end:
        cb_end(p, state)

    return p, state


def test_app(base_port, replication_factor):
    cmd = (["./test_db_app", "--rts-verbose", "--rts-ddb-replication", str(replication_factor)] +
           get_db_args(base_port, replication_factor))

    def so(line, p, s):
        log.info(f"App output: {line}")
        return False


    state = {}

    p, s = run_cmd(cmd, so, stderr_checker, state=state)

    if p.returncode == 0:
        log.debug("application exited successfully")
        return True
    else:
        log.error(f"Non-0 return code: {p.returncode}")
        return False



if __name__ == '__main__':
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("--db-nodes", type=int, default=3)
    parser.add_argument("--replication-factor", type=int)
    parser.add_argument("--repeat", type=int, default=1)
    parser.add_argument("--test-app", action="store_true")
    parser.add_argument("--test-recovery", action="store_true")
    args = parser.parse_args()

    if args.replication_factor is None:
        args.replication_factor = args.db_nodes

    # set logging format
    LOG_FORMAT = "%(asctime)s: %(module)-10s %(levelname)-8s %(message)s"
    # setup basic logging
    logging.basicConfig(format=LOG_FORMAT)

    log = logging.getLogger()
    log.setLevel(logging.INFO)
    log.setLevel(logging.DEBUG)

    allgood = True

    for i in range(args.repeat):
        log.info(f"-- Round {i}")

        dbc = DbCluster(args.db_nodes)

        if not dbc.start():
            allgood = False

        if args.test_app:
            try:
                if not test_app(dbc.base_port, args.replication_factor):
                    allgood = False
            except Exception as exc:
                log.error(exc)
                allgood = False

        if args.test_recovery:
            try:
                if not test_app_recovery(dbc.base_port, args.replication_factor):
                    allgood = False
            except Exception as exc:
                log.exception(exc)
                allgood = False

        if not dbc.stop():
            log.error("Something went wrong stopping DB Cluster")
            allgood = False

    if allgood:
        sys.exit(0)
    else:
        sys.exit(1)
