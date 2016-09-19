-module(pet).

-export([inject/0]).

inject() ->
    DogCommand = {enum, #{
    	id => 'DogCommand',
    	description => "Things you can command your dog to do",
    	values => #{
    		'SIT' => #{ value => 1, description => "Command your dog to sit" },
    		'DOWN' => #{ value => 2, description => "Command your dog 'down'"},
    		'HEEL' => #{ value => 3, description => "Command your dog to heel" }
    }}},
    ok = graphql:insert_schema_definition(DogCommand),
    
    Pet = {interface, #{
    	id => 'Pet',
    	description => "Things that are Pets",
    	fields => #{
    		name => #{ type => 'string!', description => "The name of the pet" }}
    }},
    ok = graphql:insert_schema_definition(Pet),
    
    Sentient = {interface, #{
    	id => 'Sentient',
    	description => "Things that are sentient beings",
    	fields => #{
    		name => #{ type => 'string!', description => "The name of the sentient" }}
    }},
    ok = graphql:insert_schema_definition(Sentient),
    
    Dog = {object, #{
    	id => 'Dog',
    	description => "Dogs",
    	interfaces => ['Pet'],
    	fields => #{
    		name => #{ type => 'string!', description => "Name of the dog" },
    		nickname => #{ type => 'string', description => "Nickname of the dog, if any" },
    		barkVolume => #{ type => int, description => "Bark volume in dB" },
    		doesKnowCommand => #{
    			type => 'bool!',
    			description => "Does the dog know a specific command?",
    			args => #{
    				dogCommand => #{
    					type => 'DogCommand!',
    					description => "The dog command we want to ask for" }}
    		},
    		isHouseTrained => #{
    			type => 'bool!',
    			description => "Is the dog house trained",
    			args => #{
    				atOtherHomes => #{
    					type => 'bool',
    					description => "Is the query including other homes?" }}
    		},
    		owner => #{
    			type => 'Human',
    			description => "The owner of the dog"
    		}
    }}},
    ok = graphql:insert_schema_definition(Dog),
    
    Alien = {object, #{
    	id => 'Alien',
    	description => "Facehuggers big bro",
    	interfaces => ['Sentient'],
    	fields => #{
    		name => #{ type => 'string!', description => "The name of the alien" },
    		homePlanet => #{ type => 'string', description => "The home planet of the alien, if any" }
    	}
    }},
    ok = graphql:insert_schema_definition(Alien),
    
    Human = {object, #{
    	id => 'Human',
    	description => "Things which gets eaten by aliens",
    	interfaces => ['Sentient'],
    	fields => #{
    		name => #{ type => 'string!', description => "The name of the human" },
    		pets => #{ type => ['Pet'], description => "The pets the human owns" }
    	}
    }},
    ok = graphql:insert_schema_definition(Human),

    CatCommand = {enum, #{
    	id => 'CatCommand',
    	description => "Commands for cats",
    	values => #{
    		'JUMP' => #{ value => 1, description => "The cat jumps, presumably be means of a laser pointer" }
    	}
    }},
    ok = graphql:insert_schema_definition(CatCommand),
    
    Cat = {object, #{
    	id => 'Cat',
    	description => "Overlords of the humans",
    	fields => #{
    		name => #{
    			type => 'string!', description => "The name of the cat" },
    		nickname => #{
    			type => 'string', description => "The nickname of the cat, if any" },
    		doesKnowCommand => #{
    			type => 'bool!',
    			description => "Does the cat know of a specific command type",
    			args => #{
    				catCommand => #{
    					type => 'CatCommand!',
    					description => "The CatCommand we are asking if it knows" }
    			}
    		},
    		meowVolume => #{
    			type => 'int',
    			description => "The volume of a meow, in dB"
    		}
    	}
    }},
    ok = graphql:insert_schema_definition(Cat),
    
    CatOrDog = {union, #{
    	id => 'CatOrDog',
    	description => "Cats or Dogs",
    	types => ['Cat', 'Dog']
    }},
    ok = graphql:insert_schema_definition(CatOrDog),
    
    DogOrHuman = {union, #{
    	id => 'DogOrHuman',
    	description => "Humans or Dogs",
    	types => ['Human', 'Dog']
    }},
    ok = graphql:insert_schema_definition(DogOrHuman),
    
    HumanOrAlien = {union, #{
    	id => 'HumanOrAlien',
    	description => "Humans or Aliens",
    	types => ['Human', 'Alien']
    }},
    ok = graphql:insert_schema_definition(HumanOrAlien),
    
    Arguments = {object, #{
    	id => 'Arguments',
    	description => "Testing complex argument specifications",
    	fields => #{
    		multipleReqs => #{
    			type => 'int!',
    			description => "Test multiple required args",
    			args => #{
    				x => #{ type => 'int!', description => "The X arg"},
    				y => #{ type => 'int!', description => "The Y arg"}
    			}},
    		booleanArgField => #{
    			type => 'bool',
    			description => "Test of a boolean arg field",
    			args => #{
    				booleanArg => #{ type => 'bool', description => "The Bool Arg" }
    			}},
    		floatArgField => #{
    			type => 'float',
    			description => "Test of a float arg field",
    			args => #{
    				floatArg => #{type => 'float', description => "The Float Arg" }
    			}},
    		intArgField => #{
    			type => 'int',
    			description => "Test of a int arg field",
    			args => #{
    				intArg => #{type => 'int', description => "The Int Arg" }
    			}},
    		nonNullBooleanArgField => #{
    			type => 'bool!',
    			description => "Test of non-null args",
    			args => #{
    				nonNullBooleanArg => #{
    					type => 'bool!',
    					description => "The non-null bool arg" }
    			}},
		booleanListArgField => #{
			type => [bool],
			description => "Test lists of bools",
			args => #{
				booleanListArg => #{ type => {non_null, [bool]}, description => "The list of bools"}
			}}
    }}},
    ok = graphql:insert_schema_definition(Arguments),

    QueryRoot = {object, #{
    	id => 'QueryRoot',
    	description => "Root Query of the pet schema",
    	fields => #{
    		arguments => #{ type => 'Arguments', description => "More complicated Argument cases" },
    		dog => #{ type => 'Dog', description => "Query of dogs" },
    		human => #{ type => 'Human', description => "Query of humans" },
    		pet => #{ type => 'Pet', description => "Query of pets" },
    		catOrDog => #{ type => 'CatOrDog', description => "Query of cats or dogs" }
    	}
    }},
    ok = graphql:insert_schema_definition(QueryRoot),
    
    Schema = {root, #{
    	query => 'QueryRoot',
    	interfaces => ['Sentient', 'Pet']
    }},
    ok = graphql:insert_schema_definition(Schema),
    ok.
