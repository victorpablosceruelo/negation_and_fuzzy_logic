package storeHouse;

import java.util.HashMap;

public class CacheStoreHouse {

	private static HashMap<String, HashMap<String, Object>> storeHouseStringKeys = new HashMap<String, HashMap<String, Object>>();

	/**
	 * This method allows to store in the storeHouse an object.
	 * 
	 * @param className
	 * @param key
	 * @param object
	 * @throws Exception
	 */
	public static void store(@SuppressWarnings("rawtypes") Class classObject, String key, Object object) throws Exception {
		if (classObject == null)
			throw new Exception("class cannot be null.");
		String className = classObject.getName();
		if (className == null)
			throw new Exception("className cannot be null.");
		if (key == null)
			throw new Exception("key cannot be null.");
		if (object == null)
			throw new Exception("object cannot be null.");

		HashMap<String, Object> storeHouseAux = storeHouseStringKeys.get(className);
		if (storeHouseAux == null) {
			storeHouseAux = new HashMap<String, Object>();
			storeHouseStringKeys.put(className, storeHouseAux);
		}
		storeHouseAux.put(key, object);

	}

	/**
	 * This method allow to retrieve from the storeHouse an object
	 * 
	 * @param className
	 * @param key
	 * @return returns the stored object.
	 * @throws Exception
	 */
	public static Object retrieve(@SuppressWarnings("rawtypes") Class classObject, String key) throws Exception {
		if (classObject == null)
			throw new Exception("class cannot be null.");
		String className = classObject.getName();
		if (className == null)
			throw new Exception("className cannot be null.");
		if (key == null)
			throw new Exception("key cannot be null.");

		HashMap<String, Object> storeHouseAux = storeHouseStringKeys.get(className);
		if (storeHouseAux == null)
			throw new Exception("No storeHouse for the class.");
		Object object = storeHouseAux.get(key);
		if (object == null)
			throw new Exception("No object for the key. key: " + key);
		return object;
	}

	/**
	 * Removes the object stored in the storeHouse
	 * 
	 * @param className
	 * @param key
	 * @throws Exception
	 */
	public static void remove(String className, String key) throws Exception {
		if (className == null)
			throw new Exception("className cannot be null.");
		if (key == null)
			throw new Exception("key cannot be < 0.");

		HashMap<String, Object> storeHouseAux = storeHouseStringKeys.get(className);
		if (storeHouseAux == null)
			throw new Exception("No storeHouse for the class.");

		Object object = storeHouseStringKeys.get(key);
		if (object != null)
			storeHouseAux.put(key, null);
	}
}
