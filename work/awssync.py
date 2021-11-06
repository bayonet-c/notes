import sys
import os
import time
import argparse
import concurrent.futures
from datetime import datetime
from datetime import timedelta

ENVIRONMENTS = {'dev', 'qa', 'int', 'ppe', 'prod'}
REGION_DICT = {'us-east-1': 'use1', 'us-east-2': 'use2'}
THREAD_POOL_SIZE = 32
MIN_PART_LIST = []

TEMP_DATETIME = datetime.strptime("20200101000000", "%Y%m%d%H%M%S")
for i in range(0, 1440):
    minute_part = (TEMP_DATETIME + timedelta(minutes=i)).strftime( "%Y%m%d%H%M%S")[-6:-2] + '/'
    MIN_PART_LIST.append(minute_part)


class TidyupRunner:
    def __init__(self, tr, te, sr, se, date):
        self.thread_pool = concurrent.futures.ThreadPoolExecutor(THREAD_POOL_SIZE)
        self.all_tasks = []
        self.bucket_s = "a205143-{}-{}-ingest-normalisedjournal/".format(sr, se)
        self.bucket_t = "a205143-{}-{}-ingest-normalisedjournal/".format(tr, te)
        self.prefix = date

    def sync_journal(self, target_dir):
        journal_key = "{}/{}".format(self.prefix, target_dir)
        try:
            cmd = "aws s3 sync s3://" + self.bucket_s + journal_key + " s3://" + self.bucket_t + journal_key
            print ("cmd = {}".format(cmd))
            tmp = os.popen(cmd).readlines()
            #print (tmp)
            return True
        except Exception:
            print ("Journal syning failed against and directory {}".format(target_dir))
            return False

    def tidyup(self):
        print("Started: " + time.strftime("%Y-%m-%dT%H-%M-%S", time.localtime(time.time())))
        self.all_tasks = [self.thread_pool.submit(self.sync_journal, MIN_PART_LIST[i]) for i in range(0, 1440)]
        try:
            for response in concurrent.futures.as_completed(all_tasks):
                try:
                    if response.result() is False:
                        return False
                except Exception as exc:
                    return False
            print ("Accomplished: " + time.strftime("%Y-%m-%dT%H-%M-%S", time.localtime(time.time())))
            return True
        except Exception:
            return False


def setup_args():
    parser = argparse.ArgumentParser("Tidy up the corrupted output S3 bucket of the specified Normalizer cluster.\n")
    parser.add_argument('-tr', type=str, required=True, help='AWS target region name.')
    parser.add_argument('-te', type=str, required=True, help='Target environment name.')
    parser.add_argument('-sr', type=str, required=True, help='AWS source region name.')
    parser.add_argument('-se', type=str, required=True, help='source environment name.')
    parser.add_argument('-d', type=str, required=True, help='End date of the corrupted data.')
    return parser.parse_args()


def main():
    try:
        args = setup_args()
        runner = TidyupRunner(args.tr, args.te, args.sr, args.se, args.d)
        assert runner.tidyup()
        return 0
    except Exception:
        return 1


if __name__ == "__main__":
    sys.exit(main())
