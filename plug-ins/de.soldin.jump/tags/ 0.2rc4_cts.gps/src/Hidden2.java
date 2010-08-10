
public class Hidden2 {

	public Hidden2() {
		super();
		System.out.println("Hidden2_sys");
		System.out.println( Hidden2.class.getClassLoader().getClass().getName()) ;
	}

}
