#!/usr/bin/env python3

import logging
import os
import locale
import re
import subprocess
import sys
import time
import select

ACTONDB="../dist/bin/actondb"
BASEPORT=32001

def parse_membership(line):
    m = re.match(".*Membership_agreement_msg.type=(?P<type>[^,]+), ack_status=(?P<ack_status>[^,]+), nonce=(?P<nonce>[0-9]+), Membership\((?P<membership>(Node\(.*?\))+), , VC\(.*?\)\), local_view_disagrees=(?P<local_view_disagrees>.)", line)
    membership = []
    if m is not None:
        for node in re.findall(r"Node\((.*?)\)(, )?", m.group('membership')):
            res = dict(map(lambda x: x.split('='), node[0].split(', ')))
            membership.append(res)
        return {
            'type': m.group('type'),
            'ack_status': m.group('ack_status'),
            'nonce': m.group('nonce'),
            'local_view_disagrees': m.group('local_view_disagrees'),
            'membership': membership
        }
    raise ValueError("Unable to parse Membership agreement")



class Db:
    def __repr__(self):
        return f"Db{self.idx}"

    def __init__(self, idx):
        self.idx = idx
        self.name = f"Db{idx}"
        #print(f"Starting {self.name}")
        self.p = None
        self.running = False

    def start(self):
        args = [ACTONDB, "-p", f"32{self.idx:03d}", "-m", f"34{self.idx:03d}",
                "-s", "127.0.0.1:34000"]
        self.p = subprocess.Popen(
            [ACTONDB, "-p", f"32{self.idx:03d}", "-m", f"34{self.idx:03d}", "-s", "127.0.0.1:34000"],
            stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        os.set_blocking(self.p.stdout.fileno(), False)

        while True:
            line = self.p.stdout.readline().decode(locale.getpreferredencoding(False)).strip()
            if re.match("SERVER: Started", line):
                self.running = True
                break


    def stop(self):
        self.p.kill()
        self.p.wait() # wait for the child process to exit


    def await_membership(self):
        # Try reading & looking for our expected result for some time before
        # giving up
        st = time.time()
        lines = []
        # Read in all buffered lines
        while True:
            line = self.p.stdout.readline().decode(locale.getpreferredencoding(False)).strip()
            if re.match("SERVER: Installed new agreed", line):
                return parse_membership(line)


class App:
    def __repr__(self):
        return f"App[{self.name}]"

    def __init__(self, app_path, app_args=None):
        self.path = app_path
        if app_args is not None:
            self.args = app_args
        else:
            self.args = []
        self.name = "fjong"
        self.p = None

    def start(self):
        cmd = [self.path, "--rts-ddb-host", "localhost", "--rts-ddb-replication", "1", "--rts-verbose", *self.args]
        print("RUNNING:", cmd)
        self.p = subprocess.Popen(cmd, stdin=subprocess.PIPE,
                                  stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        #os.set_blocking(self.p.stdout.fileno(), False)

    def wait(self):
        print("Waiting for application...")
        while True:
            print("fjong..")
            line = self.p.stdout.readline().decode(locale.getpreferredencoding(False)).strip()
            print(line)
            if line == '':
                break
        self.p.wait()

    def stop(self):
        pass


class DbCluster:
    def __init__(self, num=3):
        self.num = num

    def start(self):
        """Start up a cluster of num nodes and ensure that memberships look alright
        """
        log = logging.getLogger()

        log.debug("Starting database servers")
        self.dbs=[Db(0)]
        self.dbs[0].start()

        allgood = True

        for i in range(1, self.num):
            log.debug(f"Starting instance {i}")
            # Create & start new instance
            dbn = Db(i)
            dbn.start()
            self.dbs.append(dbn)

            # Now go through all existing nodes and ensure all have new membership
            expected = None
            for db in self.dbs:
                mbs = db.await_membership()
                #log.debug(f"Membership info for {db}: {mbs}")
                if expected is None:
                    log.debug(f"Using Membership info from {db} as expected")
                    expected = mbs
                else:
                    if mbs == expected:
                        log.debug(f"Membership info for {db} is as expected")
                        pass
                    else:
                        log.error(f"Membership info for {db} seems incorrect")
                        allgood = False
        return allgood


    def stop(self):
        log.debug("Stopping database servers")
        for dbn in self.dbs:
            dbn.stop()




if __name__ == '__main__':
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("--app")
    parser.add_argument("--db-nodes", type=int, default=3)
    parser.add_argument("--repeat", type=int, default=1)
    args = parser.parse_args()

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

        if args.app:
            cmd = [args.app, "--rts-verbose", "--rts-ddb-host", "localhost", "--rts-ddb-replication", str(args.db_nodes)]
            log.debug(f"Starting application: {' '.join(cmd)}")
            res = subprocess.run(cmd, capture_output=True)
            if re.search("No quorum", res.stderr.decode(locale.getpreferredencoding(False))):
                log.error(f"DB Quorum error: {res}")
            elif res.returncode == 0:
                log.debug("Application exited successfully")
            else:
                log.error("Non-0 return code:", res)
                allgood = False

        if not dbc.stop():
            allgood = False

    sys.exit(allgood)
