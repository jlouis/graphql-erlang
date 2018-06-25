type Coord {
    latitude: Float!
    longitude: Float!
}

type Geo {
    hash: String
}

union Loc = Coord | Geo | Coord
