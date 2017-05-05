interface Node {
	id: Id!
}

union Thing = Item | Monster

+description(text: """
How to represent "colors" in the output""")
scalar ColorType

+description(text: "Represents a color in the system")
scalar Color

+description(text: "How to enter monster stats")
input StatsInput {

	+description(text: "The attack value of the monster")
	attack: Int!
	
	+description(text: "When the monster yells, what does it yell?")
	yell: String
	
	+requiresAuthentication
	+description(text: "How good is the monster at shell scripting?")
	shellScripting: Int
}

type Stats {
	attack: Int!
	yell: String
	shellScripting: int
}

enum Mood {
	TRANQUIL
	DODGY
	AGGRESSIVE
}

enum Property {
	BEAST
	MECH
	DRAGON
	PIRATE
	MURLOC
	TOTEM
	DEMON
}

type Item implements Node {
	id: Id!
	name: String
	description: String
	weight: String
	weightSum: String
    foo: Boolean
	contents: [Thing]
}

type Monster implements Node {
	id: Id!
	name: String
	color(colorType: ColorType): Color
	hitpoints: Int!
	inventory: [Thing]
	hp: Int!
	mood: Mood
	plushFactor: Float!
	stats(minAttack: Int! = 0): [Stats]
	properties: [Property]
}
