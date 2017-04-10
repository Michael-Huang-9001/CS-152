import java.util.*;

class Statement {
	private String conclusion;
	private List<String> conditions;

	public Statement(String conclusion, String... conditions) {
		this.conclusion = conclusion;
		this.conditions = Arrays.asList(conditions);
	}

	// matches
	public boolean matches(String goal) {
		if (this.conclusion.equals(goal)) {
			return true;
		} else {
			return false;
		}
	}

	public List<String> getConditions() {
		return conditions;
	}
}

public class Engine {
	private List<Statement> kbase = new LinkedList<Statement>();
	private Scanner scanner = new Scanner(System.in);

	public void add(Statement s) {
		kbase.add(s);
	}

	public boolean execute(List<String> goals) {
		boolean result = false;
		// 1. if no goals return true
		if (goals.isEmpty()) {
			return true;
		}
		// 2. traverse kbase looking for statements that match first goal
		for (Statement s : kbase) {
			// 3. each time one is found recursively execute with goals2 = goals
			// - 1st goal + conditions
			if (s.matches(goals.get(0))) {
				List<String> goals2 = new LinkedList<String>(goals);
				goals2.remove(0);
				for (String con : s.getConditions()) {
					goals2.add(con);
				}
				result = execute(goals2);
				// 4. if true is ever returned, stop iteration and return true
				if (result) {
					break;
				}
			}
		}
		// 5. if nothing works, return false
		return result;
	}

	public void repl() {
		List<String> goals = new LinkedList<String>();
		while (true) {
			System.out.print("?- ");
			String query = scanner.next();
			if (query.equals("quit")) {
				System.out.println("bye");
				break;
			}
			goals.clear();
			goals.add(query);
			boolean result = execute(goals);
			System.out.println(result);

		}
	}

	public static void main(String[] args) {
		Engine engine = new Engine();

		engine.add(new Statement("d"));
		engine.add(new Statement("f"));
		engine.add(new Statement("a", "b", "c"));
		engine.add(new Statement("b", "d", "e"));
		engine.add(new Statement("a", "d", "f"));

		engine.add(new Statement("homerIsMale"));
		engine.add(new Statement("bartIsMale"));
		engine.add(new Statement("homerIsParentOfBart"));
		engine.add(new Statement("homerIsFatherOfBart", "homerIsMale", "homerIsParentOfBart"));

		engine.repl();
	}

}