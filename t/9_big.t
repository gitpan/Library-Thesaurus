# -*- cperl -*-
use Data::Dumper;
use strict;
use Test;

my $NT;
my $INT;

BEGIN {
  my $ntestes = 5;
  $NT = 100000;
  $INT = 1000;
  plan tests => ($NT/$INT)*2 + $ntestes
}

# Check module loadability
use Library::Thesaurus;
my $loaded = 1;
ok(1);

my $t = thesaurusNew();
my $i;
for $i (0..$NT) {
  ok(1) if !($i%$INT);
  $t -> addTerm("termo[${i}]");

}

$t->storeOn("/tmp/_${$}_");
ok(1);

undef($t); ### THis frees the memory??

my $the = thesaurusRetrieve("/tmp/_${$}_");
ok(1);

unlink("/tmp/_${$}_");

for $i (0..$NT) {
  ok(1) if !($i%$INT);
  last unless $the -> isdefined("termo[${i}]");

}
