use ExtUtils::MakeMaker;
WriteMakefile(
	      'NAME'	     => 'Library::Thesaurus',
	      'VERSION_FROM' => 'Thesaurus.pm',
	      'PREREQ_PM'    => {
				 'Library::MLang' => '0.03',
				 'Storable' => '1.0006',
				},
	     );
