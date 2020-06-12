#! /usr/bin/perl -wnl

BEGIN {
    $Usage = "Usage: $0 'RE' [file ...]";

    @ARGV > 0 or warn "$Usage\n" and exit 31;    # 31 means no arg

    $pattern = shift;    # remove arg1 and load into $pattern

    defined $pattern and $pattern ne "" or 
        warn "$Usage\n" and exit 27;    # arg1 undefined, or empty
}
# Now -n loop takes input from files named in @ARGV, or from STDIN

/$pattern/ and print;    # if match, print record
