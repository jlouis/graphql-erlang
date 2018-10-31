interface Node {
  id: Id!
}

union Thing = Item | Monster

"""
How to represent colors in the output
"""
scalar ColorType

"Represents a color in the system"
scalar Color

"How to enter monster stats"
input StatsInput {

  "The attack value of the monster"
  attack: Int!
	
  "When the monster yells, what does it yell?"
  yell: String
	
  "How good is the monster at shell scripting?"
  shellScripting: Int @requiresAuthentication
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
  BEAST
}

enum Property {
  BEAST
  MECH
  DRAGON
  PIRATE @deprecated(reason: "I am a deprecation on an enum")
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
  mood: Mood @fieldDefDirective(myarg: "Hello World")
  plushFactor: Float!
  stats(minAttack: Int! = 0): [Stats]
  properties: [Property]
}

directive @execDefDirective on
  | FIELD
  | FRAGMENT_SPREAD

directive @fieldDefDirective(myarg: String) on FIELD_DEFINITION
