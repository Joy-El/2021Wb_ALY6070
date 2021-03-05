#!/usr/bin/env python3

import csv
import argparse

def get_arguments():
    """Parse command line arguments."""
    parser = argparse.ArgumentParser(description=
                                     """Filters global.health data to a specific field:value set.""")
    parser.add_argument("-d", "--data",
                        help="path to global.health raw data (unzipped)",
                        required=True)
    parser.add_argument("-f", "--field",
                        help="Header name of field to filter on (case sensitive)",
                        required=True)
    parser.add_argument("-m", "--match",
                        help="Field values matching this will be kept (case sensitive)",
                        required=True)
    parser.add_argument("-o", "--output",
                        help="Name for output file",
                        required=True)
    return(parser.parse_args())


def get_field_position(header, field):
    """Identify index for field of interest."""
    return [i for i, h in enumerate(header) if h == field]


def parse_globaldothealth(args):
    """Create subset of the global.health raw data matching a filter set."""
    with open(args.data) as csvfile:
        linereader = csv.reader(csvfile, delimiter=',', quotechar='"')

        # get and process header line
        header = linereader.__next__()
        field_number = get_field_position(header, args.field)[0] # only take first value right now

        with open(args.output, 'w') as outfile:
            outwriter = csv.writer(outfile, delimiter=',', quotechar='"')
            outwriter.writerow(header)

            for row in linereader:
                if row[field_number] == args.match:
                    outwriter.writerow(row)


if __name__ == "__main__":
    arguments = get_arguments()
    parse_globaldothealth(arguments)
