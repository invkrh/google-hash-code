package f2018;

import java.util.*;

public class Solver {
  static Output solve(Input city) {
    // TODO
    // BuildingProject[] bps = city.buildProjects();
    // bps[0].hp();

    ArrayList<Building> buildings = new ArrayList<>();
    buildings.add(new Building(0, 0, 0));
    buildings.add(new Building(1, 3, 0));
    buildings.add(new Building(2, 0, 2));
    buildings.add(new Building(0, 0, 5));
    return new Output(4, buildings.toArray(new Building[0]));
  }
}
