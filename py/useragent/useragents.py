#!/usr/bin/python3
'''
Reads line of the form
NNN Useragent string
and outputs a CSV with the read useragents resolved into browser and OS info.
'''
import argparse
from collections import defaultdict
import csv
from functools import reduce
import requests
import urllib.parse
import sqlite3
import sys


DB_NAME = "useragents.sqlite"

INFO_HEADERS = ("ua_type", "ua_brand", "ua_name", "ua_version", "ua_url",
                "os_name", "os_version",
                "browser_name", "browser_version",
                "engine_name", "engine_version")
DB_COLUMNS = ("useragent", ) + INFO_HEADERS
CSV_HEADERS = ("count", "error message") + INFO_HEADERS + ("original",)


def flatten(l):
    return reduce(
        lambda xs, x: (
            xs + list(flatten(x)) if isinstance(x, (list, tuple))
            else xs + [x]),
        l, [])


def dict_factory(cursor, row):
    return {col[0]: row[idx] for idx, col in enumerate(cursor.description)}


def get_useragent_info_from_api(useragent, apikey):
    headers = {
        "Accept": "application/json"
    }
    url = "https://useragentapi.com/api/v4/json/{apikey}/{agent}".format(
        apikey=apikey,
        agent=urllib.parse.quote(useragent, safe=''))
    response = requests.get(url, headers=headers, verify=True)
    if response.status_code != 200:
        raise RuntimeError("Http error {}: {}".format(
            response.status_code, response.content))
    return response.json()


def get_useragent_info_from_db(cursor, useragent):
    cursor.execute('SELECT * from useragents WHERE useragent=?', (useragent, ))
    rows = cursor.fetchall()
    assert len(rows) <= 1, useragent
    if len(rows) == 0:
        return None
    return rows[0]


INSERT_STMT = '''INSERT INTO useragents({}) VALUES ({})'''.format(
    ','.join(DB_COLUMNS), ','.join([':' + col for col in DB_COLUMNS]))


def update_useragent_info(cursor, useragent, info):
    insertdict = defaultdict(str, info)
    insertdict['useragent'] = useragent
    cursor.execute(INSERT_STMT, insertdict)


def get_info(cursor, useragent, args):

    def _error(error):
        return error, [""] * len(INFO_HEADERS)

    info = get_useragent_info_from_db(cursor, useragent)
    if info:
        return "", [info.get(field, "") for field in INFO_HEADERS]
    elif args.db_only:
        return _error("Useragent not in DB")
    else:
        info = get_useragent_info_from_api(useragent, args.apikey)
        if "data" in info:
            info_row = [
                info["data"].get(field, "") for field in INFO_HEADERS]
            if not args.no_db_update:
                update_useragent_info(cursor, useragent, info["data"])
            return "", info_row
        else:
            return _error(info["error"]["message"])


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--input", help="input file")
    parser.add_argument(
        "--output", help="output CSV name")
    parser.add_argument(
        "--db-only", default=False, action="store_true",
        help="never query external service")
    parser.add_argument(
        "--no-db-update", default=False, action="store_true",
        help="never query external service")
    args = parser.parse_args()

    conn = sqlite3.connect(DB_NAME)
    conn.row_factory = dict_factory
    if args.output:
        output_file = args.output
    elif args.input:
        output_file = args.input + ".csv"
    else:
        parser.error(
            "Need to specify an output file when reading standard input")

    if not args.db_only:
        with open('.apikey', 'rt') as f:
            args.apikey = f.readline().strip()
    with conn,\
            open(args.input, "rt") if args.input else sys.stdin as f_in,\
            open(output_file, "wt") as csvfile:
        cursor = conn.cursor()
        writer = csv.writer(csvfile)
        writer.writerow(CSV_HEADERS)
        for line in f_in:
            count, agent = line.strip().split(' ', 1)
            row = (count, get_info(cursor, agent, args), agent)
            writer.writerow(flatten(row))


if __name__ == "__main__":
    sys.exit(main())
