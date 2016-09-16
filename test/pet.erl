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
    graphql_schema:insert_new(DogCommand),
    
    Pet = {interface, #{
    	id => 'Pet',
    	description => "Things that are Pets",
    	fields => #{
    		name => #{ type => 'string!', description => "The name of the pet" }}
    }},
    graphql_schema:insert_new(Pet),
    
    Sentient = {interface, #{
    	id => 'Sentient',
    	description => "Things that are sentient beings",
    	fields => #{
    		name => #{ type => 'string!', description => "The name of the sentient" }}
    }},
    graphql_schema:insert_new(Sentient),
    
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
    graphql_schema:insert_new(Dog),
    
    Alien = {object, #{
    	id => 'Alien',
    	description => "Facehuggers big bro",
    	interfaces => ['Sentient'],
    	fields => #{
    		name => #{ type => 'string!', description => "The name of the alien" },
    		homePlanet => #{ type => 'string', description => "The home planet of the alien, if any" }
    	}
    }},
    graphql_schema:insert_new(Alien),
    
    Human = {object, #{
    	id => 'Human',
    	description => "Things which gets eaten by aliens",
    	interfaces => ['Sentient'],
    	fields => #{
    		name => #{ type => 'string!', description => "The name of the human" },
    		pets => #{ type => ['Pet'], description => "The pets the human owns" }
    	}
    }},
    graphql_schema:insert_new(Human),

    CatCommand = {enum, #{
    	id => 'CatCommand',
    	description => "Commands for cats",
    	values => #{
    		'JUMP' => #{ value => 1, description => "The cat jumps, presumably be means of a laser pointer" }
    	}
    }},
    graphql_schema:insert_new(CatCommand),
    
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
    graphql_schema:insert_new(Cat),
    
    CatOrDog = {union, #{
    	id => 'CatOrDog',
    	description => "Cats or Dogs",
    	types => ['Cat', 'Dog']
    }},
    graphql_schema:insert_new(CatOrDog),
    
    DogOrHuman = {union, #{
    	id => 'DogOrHuman',
    	description => "Humans or Dogs",
    	types => ['Human', 'Dog']
    }},
    graphql_schema:insert_new(DogOrHuman),
    
    HumanOrAlien = {union, #{
    	id => 'HumanOrAlien',
    	description => "Humans or Aliens",
    	types => ['Human', 'Alien']
    }},
    graphql_schema:insert_new(HumanOrAlien),
    
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
    graphql_schema:insert_new(Arguments),

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
    graphql_schema:insert_new(QueryRoot),
    
    Schema = {root, #{
    	query => 'QueryRoot',
    	interfaces => ['Sentient', 'Pet']
    }},
    graphql_schema:insert_new(Schema),
    ok.
