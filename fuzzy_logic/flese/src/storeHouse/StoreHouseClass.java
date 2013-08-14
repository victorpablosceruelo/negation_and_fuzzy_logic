package storeHouse;

import java.util.ArrayList;
import java.util.HashMap;

public class StoreHouseClass {

	private static HashMap<String, HashMap<String, Object>> storeHouseStringKeys = new HashMap<String, HashMap<String, Object>>();
	private static HashMap<String, ArrayList<Object>> storeHouseIntKeys = new HashMap<String, ArrayList<Object>>();

	/**
	 * This method allows to store in the storeHouse an object.
	 * @param className
	 * @param key
	 * @param object
	 * @throws Exception 
	 */
	public static void store(String className, String key, Object object) throws Exception {
		if (className == null) throw new Exception("className cannot be null.");
		if (key == null) throw new Exception("key cannot be null.");
		if (object == null) throw new Exception("object cannot be null.");
		
		HashMap<String, Object> storeHouseAux = storeHouseStringKeys.get(className);
		if (storeHouseAux == null) {
			storeHouseAux = new HashMap<String, Object>();
			storeHouseStringKeys.put(className, storeHouseAux);
		}
		storeHouseAux.put(key, object);
		
	}

	/**
	 * This method allow to retrieve from the storeHouse an object
	 * @param className	
	 * @param key
	 * @return returns the stored object.
	 * @throws Exception 
	 */
	public static Object retrieve(String className, String key) throws Exception {
		if (className == null) throw new Exception("className cannot be null.");
		if (key == null) throw new Exception("key cannot be null.");

		HashMap<String, Object> storeHouseAux = storeHouseStringKeys.get(className);
		if (storeHouseAux == null) throw new Exception("No storeHouse for the class.");
		Object object = storeHouseStringKeys.get(key);
		if (object == null) throw new Exception("No object for the key. key: " + key);
		return object;
	}

	/**
	 * Removes the object stored in the storeHouse
	 * @param className
	 * @param key
	 * @throws Exception
	 */
	public static void remove(String className, String key) throws Exception {
		if (className == null) throw new Exception("className cannot be null.");
		if (key == null) throw new Exception("key cannot be < 0.");

		HashMap<String, Object> storeHouseAux = storeHouseStringKeys.get(className);
		if (storeHouseAux == null) throw new Exception("No storeHouse for the class.");

		Object object = storeHouseStringKeys.get(key);
		if (object != null) storeHouseAux.put(key, null);		
	}
	
	/**
	 * This method allows to store in the storeHouse an object.
	 * @param className
	 * @param key
	 * @param object
	 * @throws Exception 
	 */
	public static void store(String className, int key, Object object) throws Exception {
		if (className == null) throw new Exception("className cannot be null.");
		if (key < 0) throw new Exception("key cannot be < 0.");
		if (object == null) throw new Exception("object cannot be null.");
		
		ArrayList<Object> storeHouseAux = storeHouseIntKeys.get(className);
		if (storeHouseAux == null) {
			storeHouseAux = new ArrayList<Object>();
			storeHouseIntKeys.put(className, storeHouseAux);
		}
		while (key < storeHouseAux.size()) {
			storeHouseAux.add(null);
		}
		storeHouseAux.add(key, object);
		
	}

	/**
	 * This method allow to retrieve from the storeHouse an object
	 * @param className	
	 * @param key
	 * @return returns the stored object.
	 * @throws Exception 
	 */
	public static Object retrieve(String className, int key) throws Exception {
		if (className == null) throw new Exception("className cannot be null.");
		if (key < 0) throw new Exception("key cannot be < 0.");

		ArrayList<Object> storeHouseAux = storeHouseIntKeys.get(className);
		if (storeHouseAux == null) throw new Exception("No storeHouse for the class.");
		
		Object object = null;
		if (key < storeHouseAux.size()) object = storeHouseAux.get(key);
		if (object == null) throw new Exception("No object for the key.");
		return object;
	}
	
	/**
	 * Removes the object stored in the storeHouse
	 * @param className
	 * @param key
	 * @throws Exception
	 */
	public static void remove(String className, int key) throws Exception {
		if (className == null) throw new Exception("className cannot be null.");
		if (key < 0) throw new Exception("key cannot be < 0.");

		ArrayList<Object> storeHouseAux = storeHouseIntKeys.get(className);
		if (storeHouseAux == null) throw new Exception("No storeHouse for the class.");

		Object object = null;
		if (key < storeHouseAux.size()) object = storeHouseAux.get(key);
		if (object != null) storeHouseAux.add(key, null);		
	}

}










