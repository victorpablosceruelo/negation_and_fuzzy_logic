package auxiliar;

import java.util.HashMap;

public class StoreHouse {

	private static HashMap<String, Object> storeHouse = new HashMap<String, Object>();

	/**
	 * This method allows to store in the storeHouse an object.
	 * @param key
	 * @param object
	 */
	public static void store(String key, Object object) {
		storeHouse.put(key, object);
	}

	/**
	 * This method allow to retrieve from the storeHouse an object
	 * @param key
	 * @return
	 */
	public static Object retrieve(String key) {
		return storeHouse.get(key);
	}
}
