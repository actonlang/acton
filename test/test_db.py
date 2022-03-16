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
import unittest

ACTONDB="../dist/bin/actondb"
BASEPORT=32001

class MonIntermittentError(Exception):
    pass

class MonOtherError(Exception):
    pass

class TcpCmdError(Exception):
    pass

class TcpCmdNoResponse(Exception):
    pass

def get_db_args(base_port, replication_factor):
    return [item for sublist in map(lambda x: ("--rts-ddb-host", x), [f"127.0.0.1:{base_port+idx}" for idx in range(replication_factor)]) for item in sublist]


def mon_cmd(address, cmd, retries=5):
    buf = b""
    # Simple netstrings implementation, which also assumes that there is
    # only one response to our query
    try:
        sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        sock.connect(address)
        sock.send(f"{len(cmd)}:{cmd},".encode('utf-8'))
        while True:
            try:
                colpos = buf.index(b':')
            except ValueError as exc:
                # Not enough data, read some and try again
                recv = sock.recv(1024)
                if len(recv) == 0:
                    raise ConnectionError("remote hung up")
                buf += recv
                continue
            length = int(buf[0:colpos])
            start = colpos + 1
            end = start + length
            if len(buf) < end:
                # Not enough data, read some and try again
                recv = sock.recv(1024)
                if len(recv) == 0:
                    raise ConnectionError("remote hung up")
                buf += recv
                continue
            res = buf[start:end]
            buf = buf[end+1:] # +1 to skip the ,
            sock.close()
            return json.loads(res.decode("utf-8"))
    except (FileNotFoundError, ConnectionError):
        sock.close()
        if retries > 0:
            time.sleep(0.01)
            return mon_cmd(address, cmd, retries-1)
        else:
            raise MonIntermittentError("Unable to get data from remote")
    except Exception as exc:
        raise MonOtherError(f"Unhandled exception in mon_cmd: {exc}")


def tcp_cmd(p, port, cmd, retries=100):
    if p.poll() is not None:
        raise TcpCmdError(f"Process is dead, returncode: {p.returncode}  stdout: {p.stdout and p.stdout.read()}  stderr: {p.stderr and p.stderr.read()}")
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    try:
        s.connect(("localhost", port))
        s.send(cmd.encode("UTF-8"))
        res = s.recv(10)
        if res == b"":
            print("Weird, got no response from server, retrying...")
            raise TcpCmdNoResponse()
        s.close()
        return res.decode("utf-8")
    except (ConnectionRefusedError, ConnectionResetError, TcpCmdNoResponse) as exc:
        s.close()
        if retries == 0:
            raise exc
        st = (101-retries)*0.01
        if retries < 90:
            print(f"Sleeping {st} before next attempt...")
        time.sleep(st)
        return tcp_cmd(p, port, cmd, retries-1)



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
            except MonIntermittentError:
                time.sleep(0.01)


    def stop(self):
        self.p.kill()
        self.p.wait() # wait for the child process to exit
        self.logfile.close()


    def get_vc(self):
        data = self.get_membership()
        return data["membership"]["view_id"]

    def get_membership(self, retries=5):
        if self.p.poll() is not None:
            raise Exception(f"ActonDB {self.idx} is dead, returncode: {self.p.returncode}, see db{self.idx}.log file")

        try:
            return mon_cmd(self.mon_sock, "membership", retries=5)
        except MonOtherError as exc:
            if retries > 0:
                time.sleep(0.01)
                return self.get_membership(retries=retries-1)
            else:
                raise exc




class DbCluster:
    def __init__(self, num=3, base_port=None):
        self.log = logging.getLogger()
        self.num = num
        self.base_port = base_port
        if not self.base_port:
            # compute random base port between 10000 to 60000 in increments of
            # 200 ports, which allows us to run up to 100 DB nodes per test
            self.base_port = random.randint(50, 100) * 200

    def start(self):
        """Start up a cluster of num nodes and ensure that memberships look alright
        """

        self.log.debug("Starting database servers")
        self.dbs=[Db(0, self.base_port)]
        self.dbs[0].start()

        allgood = True

        for i in range(1, self.num):
            self.log.debug(f"Starting instance {i}")
            # Create & start new instance
            dbn = Db(i, self.base_port)
            dbn.start()
            self.dbs.append(dbn)

            # Now go through all existing nodes and ensure all have new membership
            expected = None
            for db in self.dbs:
                mbs = db.get_vc()
                if expected is None:
                    self.log.debug(f"Using Membership info from {db} as expected: {mbs}")
                    expected = mbs
                else:
                    if mbs == expected:
                        self.log.debug(f"Membership info for {db} is as expected: {mbs}")
                        pass
                    else:
                        self.log.error(f"Membership info for {db} seems incorrect: {mbs}")
                        allgood = False

        return allgood


    def check_membership(self):
        allgood = True

        expected = None
        for db in self.dbs:
            mbs = db.get_vc()
            if expected is None:
                self.log.info(f"Using Membership info from {db} as expected: {mbs}")
                expected = mbs
            else:
                if mbs == expected:
                    self.log.info(f"Membership info for {db} is as expected: {mbs}")
                    pass
                else:
                    self.log.error(f"Membership info for {db} seems incorrect: {mbs} vs {expected}")
                    allgood = False

        return allgood


    def stop(self):
        self.log.debug("Stopping database servers")
        for dbn in self.dbs:
            try:
                dbn.stop()
            except:
                print("Unable to stop {dbn}")
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
    log = logging.getLogger()
    log.info(f"App stderr: {line}")

    if re.search("Assertion", line):
        raise ValueError(f"Got an assertion: {line}")

    if re.search("ERROR", line):
        raise ValueError(f"ERROR: {line}")

    if re.search("No quorum", line):
        raise ValueError(f"DB Quorum error: {line}")

    return False


def run_cmd(cmd, cb_so=None, cb_se=None, cb_end=None, state=None):
    log = logging.getLogger()
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




class TestDbApps(unittest.TestCase):
    replication_factor = 3
    dbc = None
    p = None
    p2 = None

    def setUp(self):
        self.dbc = DbCluster(self.replication_factor)
        self.dbc.start()

    def tearDown(self):
        try:
            dbm = self.dbc.check_membership()
            if not dbm:
                print(f"DB membership status on shutdown: {dbm}, check the log files")
        except Exception as exc:
            print(f"Got exception during check membership on shutdown: {exc}")

        if self.p:
            try:
                self.p.kill()
                self.p.wait()
            except:
                pass
        if self.p2:
            try:
                self.p2.kill()
                self.p2.wait()
            except:
                pass
        self.dbc.stop()


    def test_app(self):
        cmd = ["./test_db_app", "--rts-verbose",
               "--rts-ddb-replication", str(self.replication_factor)
               ] + get_db_args(self.dbc.base_port, self.replication_factor)
        self.p = subprocess.run(cmd, capture_output=True, timeout=3)

        if self.p.returncode != 0:
            print(self.p.returncode)
            print(self.p.stdout)
            print(self.p.stderr)
        self.assertEqual(self.p.returncode, 0)


    def test_app_resume_tcp_server(self):
        app_port = self.dbc.base_port+199
        cmd = ["./rts/ddb_test_server", str(app_port), "--rts-verbose",
               "--rts-ddb-replication", str(self.replication_factor)
               ] + get_db_args(self.dbc.base_port, self.replication_factor)
        self.p = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        self.assertEqual(tcp_cmd(self.p, app_port, "GET"), "0")
        tcp_cmd(self.p, app_port, "INC")
        self.assertEqual(tcp_cmd(self.p, app_port, "GET"), "1")
        self.p.terminate()
        self.p.communicate()
        self.p = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        self.assertEqual(tcp_cmd(self.p, app_port, "GET"), "1")
        self.p.terminate()
        self.p.communicate()


    def test_app_resume_tcp_client(self):
        app_port = self.dbc.base_port+199
        # Start TCP server
        srv_cmd = ["./rts/ddb_test_server", str(app_port)]
        self.p2 = subprocess.Popen(srv_cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE)

        cmd = ["./rts/ddb_test_client", str(app_port), "--rts-verbose",
               "--rts-ddb-replication", str(self.replication_factor)
               ] + get_db_args(self.dbc.base_port, self.replication_factor)
        self.p = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        time.sleep(0.1)
        self.p.terminate()

        tcp_cmd(self.p2, app_port, "INC")

        self.p = subprocess.run(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE, timeout=1)
        self.assertEqual(self.p.returncode, 0)



class TestDbAppsNoQuorum(unittest.TestCase):
    def test_app(self):
        cmd = ["./test_db_app", "--rts-verbose",
               "--rts-ddb-replication", "3",
               "--rts-ddb-host", "localhost",
               "--rts-ddb-host", "localhost",
               "--rts-ddb-host", "localhost"
               ]
        p = subprocess.run(cmd, capture_output=True, timeout=20)
        self.assertTrue(re.search(r"No quorum", p.stderr.decode("utf-8")))
        # TODO: really should not be 0, the program should not return at all! It
        # should just halt until it reaches quorum again, which it will never
        # do, thus should pause forever and we should get a timeout exception
        self.assertEqual(p.returncode, 0)
        # TODO: we should not see the normal program output since the program
        # should not be able to make progress without DB quorum
        #self.assertFalse(re.search(r"In final method", p.stdout.decode("utf-8")))

    # This is what the above test case should look like when RTS is working
    # properly!
    @unittest.skip("RTS does not properly pause under lack of quorum")
    def test_app_proper(self):
        cmd = ["./test_db_app", "--rts-verbose",
               "--rts-ddb-replication", "3",
               "--rts-ddb-host", "localhost",
               "--rts-ddb-host", "localhost",
               "--rts-ddb-host", "localhost"
               ]
        with self.assertRaises(SomeException) as cm:
            subprocess.run(cmd, capture_output=True, timeout=20)

        exc = cm.exception
        self.assertTrue(re.search(r"No quorum", exc.stderr.decode("utf-8")))
        self.assertFalse(re.search(r"In final method", exc.stdout.decode("utf-8")))


if __name__ == '__main__':
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("--repeat", type=int, default=1)
    parser.add_argument("--replication-factor", type=int, default=3)
    args, unknown = parser.parse_known_args()

    TestDbApps.replication_factor = args.replication_factor

    allgood = True
    for i in range(args.repeat):
        r = unittest.main(argv=[sys.argv[0]] + unknown, exit=False)
        if len(r.result.errors) > 0 or len(r.result.failures) > 0:
            allgood = False

    if allgood:
        sys.exit(0)
    else:
        sys.exit(1)
