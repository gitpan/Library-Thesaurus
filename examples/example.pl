#!/usr/bin/perl -w
use Library::Thesaurus;
use Data::Dumper;

$thesaurus = thesaurusLoad('thesaurus');
print Dumper($thesaurus->depth_first("_top_",3,"NT","BT"));


