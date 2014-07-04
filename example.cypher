WITH
[
    {
        boatClass: "LM1x",
        date: "2017-04-01 17:02:00 +0200",
        event: "Example Championships",
        positions: [
            {
                boat: "DEN",
                country: "DEN",
                crew: [
                    {
                        name: "Jansen, Mike",
                        position: "Bow"
                    }
                ],
                details: [
                    {
                        distance: "500m",
                        position: "1",
                        time: "01:52.310"
                    },
                    {
                        distance: "1000m",
                        position: "1",
                        time: "03:48.760"
                    },
                    {
                        distance: "1500m",
                        position: "1",
                        time: "05:40.890"
                    },
                    {
                        distance: "2000m",
                        position: "1",
                        time: "07:30.930"
                    }
                ],
                rank: 1,
                time: "07:30.930"
            },
            {
                boat: "GER",
                country: "GER",
                crew: [
                    {
                        name: "Meier, Hans",
                        position: "Bow"
                    }
                ],
                details: [
                    {
                        distance: "500m",
                        position: "3",
                        time: "01:57.450"
                    },
                    {
                        distance: "1000m",
                        position: "2",
                        time: "03:52.300"
                    },
                    {
                        distance: "1500m",
                        position: "2",
                        time: "05:48.180"
                    },
                    {
                        distance: "2000m",
                        position: "2",
                        time: "07:39.140"
                    }
                ],
                rank: 2,
                time: "07:39.140"
            },
            {
                boat: "SUI",
                country: "SUI",
                crew: [
                    {
                        name: "Keller, Fritz",
                        position: "Bow"
                    }
                ],
                details: [
                    {
                        distance: "500m",
                        position: "2",
                        time: "01:52.270"
                    },
                    {
                        distance: "1000m",
                        position: "3",
                        time: "03:53.100"
                    },
                    {
                        distance: "1500m",
                        position: "3",
                        time: "05:49.810"
                    },
                    {
                        distance: "2000m",
                        position: "3",
                        time: "07:50.420"
                    }
                ],
                rank: 3,
                time: "07:50.420"
            }
        ],
        race: "FA",
        raceType: "final",
        venueCity: "Madrid",
        venueCountry: "ESP",
        year: "2017"
    },
    {
        boatClass: "LM1x",
        date: "2017-04-01 17:37:00 +0200",
        event: "Example Championships",
        positions: [
            {
                boat: "POR",
                country: "POR",
                crew: [
                    {
                        name: "Sanchez, Rodrigo",
                        position: "Bow"
                    }
                ],
                details: [
                    {
                        distance: "500m",
                        position: "1",
                        time: "01:47.200"
                    },
                    {
                        distance: "1000m",
                        position: "1",
                        time: "03:40.290"
                    },
                    {
                        distance: "1500m",
                        position: "1",
                        time: "05:35.280"
                    },
                    {
                        distance: "2000m",
                        position: "1",
                        time: "07:32.730"
                    }
                ],
                rank: 1,
                time: "07:32.730"
            },
            {
                boat: "SLO",
                country: "SLO",
                crew: [
                    {
                        name: "Novak, Ivan",
                        odfid: "25145",
                        position: "Bow"
                    }
                ],
                details: [
                    {
                        distance: "500m",
                        position: "2",
                        time: "01:51.200"
                    },
                    {
                        distance: "1000m",
                        position: "2",
                        time: "03:42.110"
                    },
                    {
                        distance: "1500m",
                        position: "2",
                        time: "05:38.150"
                    },
                    {
                        distance: "2000m",
                        position: "2",
                        time: "07:35.230"
                    }
                ],
                rank: 2,
                time: "07:35.230"
            },
            {
                boat: "ESP",
                country: "ESP",
                crew: [
                    {
                        name: "Martinez, Juan",
                        position: "Bow"
                    }
                ],
                details: [
                    {
                        distance: "500m",
                        position: "3",
                        time: "01:52.190"
                    },
                    {
                        distance: "1000m",
                        position: "3",
                        time: "03:42.980"
                    },
                    {
                        distance: "1500m",
                        position: "3",
                        time: "05:42.410"
                    },
                    {
                        distance: "2000m",
                        position: "3",
                        time: "07:36.110"
                    }
                ],
                rank: 3,
                time: "07:36.110"
            }
        ],
        race: "FB",
        raceType: "final",
        venueCity: "Madrid",
        venueCountry: "ESP",
        year: "2017"
    }
]
as races
WITH races, 
EXTRACT(i in RANGE(0,LENGTH(races)-2) | {src: races[i], dest: races[i+1]}) as finalOrder,
HEAD(races) AS common,
HEAD(HEAD(races).positions).details as checkpoints 
MERGE (s:Season {year: TOINT(common.year)})
MERGE (e:Event {name: common.event, type: common.eventType})
MERGE (venueCountry:Country {name: common.venueCountry})
MERGE (venueCity:City {name: common.venueCity})
CREATE UNIQUE e-[:VENUE_CITY]->venueCity-[:PART_OF]->venueCountry
CREATE UNIQUE s-[:TAKES_PLACE]->e
CREATE e-[:RACES]->(r:Race {name: common.boatClass})
WITH distinct r, common, races, finalOrder, checkpoints
FOREACH (cp IN checkpoints | 
  CREATE r-[:CHECKPOINTS]->(c:Checkpoint {distance: cp.distance})
)
FOREACH (race in races | 
 CREATE r-[:FINALS]->(fi:Final {name: race.race, date: race.date})
  FOREACH (b IN race.positions | 
   MERGE (country:Country {name: b.country}) 
   CREATE fi-[:BOATS]->(boat:Boat {name: b.boat, position: b.rank, time: b.time})-[:FOR]->country
   FOREACH (c IN b.crew | 
     MERGE (p:Person {name: c.name})
     CREATE UNIQUE boat-[:MEMBER]->p
   )
   FOREACH (d in b.details |
     CREATE UNIQUE r-[:CHECKPOINTS]->(c:Checkpoint {distance: d.distance})
     CREATE boat-[:MEASUREMENT {position: TOINT(d.position), time: d.time}]->c
   )
 )
)
FOREACH (p IN finalOrder | 
 CREATE UNIQUE r-[:FINALS]->(f1:Final {name: p.src.race, date: p.src.date})
 CREATE UNIQUE r-[:FINALS]->(f2:Final {name: p.dest.race, date: p.dest.date})
 CREATE UNIQUE f1-[:NEXT_FINAL]->f2)
WITH r, checkpoints, LAST(checkpoints).distance as finish, 
EXTRACT(i in RANGE(0,LENGTH(checkpoints)-2) | {src: checkpoints[i], dest: checkpoints[i+1]}) as checkpointOrder
MATCH r-[:CHECKPOINTS]-(c:Checkpoint {distance: finish})
SET c :Finish
FOREACH (cp IN checkpointOrder | 
 CREATE UNIQUE r-[:CHECKPOINTS]->(src:Checkpoint {distance: cp.src.distance})
 CREATE UNIQUE r-[:CHECKPOINTS]->(dest:Checkpoint {distance: cp.dest.distance})
 CREATE UNIQUE src-[:NEXT_CHECKPOINT]->dest
);