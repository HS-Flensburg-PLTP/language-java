enum Day {
    MONDAY, TUESDAY, WEDNESDAY, THURSDAY, FRIDAY, SATURDAY, SUNDAY
}

enum Suit {
    Diamonds, Hearts, Spades, Clubs
}

class JsonElement {
    String getAsString() { return ""; }
}

public class CaseTest {
    void switchStatementEmpty() {
        switch (0) {}
    }
    void switchStatementOld(Day day) {
        switch (day) {
            case MONDAY, FRIDAY, SUNDAY:
                System.out.println("hello");
                System.out.println(6);
            case TUESDAY: {
                System.out.println(7);
            }
            case THURSDAY, SATURDAY: System.out.println(8);
            default: System.out.println(9);
        }
    }
    void switchStatement(Day day) {
        switch (day) {
            case MONDAY, FRIDAY, SUNDAY ->
                System.out.println(6);
            case TUESDAY                -> {
                System.out.println(7);
            }
            case THURSDAY, SATURDAY -> System.out.println(8);
            default -> System.out.println(9);
        }
    }

    void switchExpression(Day day) {
        System.out.println(switch (day) {
            case MONDAY, FRIDAY, SUNDAY -> 6;
            case TUESDAY                -> 7;
            case THURSDAY, SATURDAY     -> {
                System.out.println("hello");
                System.out.println("World");
                throw new RuntimeException("error");
            }
            default              -> 9;
        });
    }

    public static Suit fromJSON(JsonElement json) {
        return switch (json.getAsString()) {
          case "Karo" -> Suit.Diamonds;
          case "Herz" -> Suit.Hearts;
          case "Pick" -> Suit.Spades;
          case "Kreuz" -> Suit.Clubs;
          default -> throw new IllegalStateException("Invalid suit: " + json);
        };
      }
}
