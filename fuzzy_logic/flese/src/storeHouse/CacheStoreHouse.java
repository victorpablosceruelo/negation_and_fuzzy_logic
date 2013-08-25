package storeHouse;

import java.util.HashMap;

public class CacheStoreHouse {

	private static HashMap<String, HashMap<String, HashMap<String, HashMap<String, Object>>>> storeHouse = new HashMap<String, HashMap<String, HashMap<String, HashMap<String, Object>>>>();

	/**
	 * This method allows to store in the storeHouse an object.
	 * 
	 * @param keyLevel1
	 * @param keyLevel2
	 * @param keyLevel3
	 * @param keyLevel4
	 * @param object
	 * @throws CacheStoreHouseException
	 */
	public synchronized static void store(@SuppressWarnings("rawtypes") Class keyLevel1, String keyLevel2, String keyLevel3,
			String keyLevel4, Object object) throws CacheStoreHouseException {

		String className = getKeyLevel1(keyLevel1);

		boolean resetLevel2 = false;
		boolean resetLevel3 = false;
		boolean resetLevel4 = false;

		if ((keyLevel2 == null) || ("".equals(keyLevel2))) {
			resetLevel2 = true;
		}

		if ((keyLevel3 == null) || ("".equals(keyLevel3))) {
			resetLevel3 = true;
		}

		if ((keyLevel4 == null) || ("".equals(keyLevel4))) {
			resetLevel4 = true;
		}

		// Retrieve or create the level 2 storeHouse.
		HashMap<String, HashMap<String, HashMap<String, Object>>> storeHouseL2 = storeHouse.get(className);
		if ((storeHouseL2 == null) || resetLevel2) {
			storeHouseL2 = new HashMap<String, HashMap<String, HashMap<String, Object>>>();
			storeHouse.put(className, storeHouseL2);

			if (resetLevel2)
				return;
		}

		// Retrieve or create the level 3 storeHouse.
		HashMap<String, HashMap<String, Object>> storeHouseL3 = storeHouseL2.get(keyLevel2);
		if ((storeHouseL3 == null) || (resetLevel3)) {
			storeHouseL3 = new HashMap<String, HashMap<String, Object>>();
			storeHouseL2.put(keyLevel2, storeHouseL3);

			if (resetLevel3)
				return;
		}

		// Retrieve or create the level 4 storeHouse.
		HashMap<String, Object> storeHouseL4 = storeHouseL3.get(keyLevel3);
		if ((storeHouseL4 == null) || (resetLevel4)) {
			storeHouseL4 = new HashMap<String, Object>();
			storeHouseL3.put(keyLevel3, storeHouseL4);

			if (resetLevel4)
				return;
		}
		// Save the object in the level 3 storeHouse.
		storeHouseL4.put(keyLevel4, object);
	}

	/**
	 * This method allow to retrieve from the storeHouse an object
	 * 
	 * @param keyLevel1
	 * @param keyLevel2
	 * @param keyLevel3
	 * @param keyLevel4
	 * @return returns the stored object.
	 * @throws CacheStoreHouseException
	 */
	public static Object retrieve(@SuppressWarnings("rawtypes") Class keyLevel1, String keyLevel2, String keyLevel3, String keyLevel4)
			throws CacheStoreHouseException {

		String className = getKeyLevel1(keyLevel1);

		HashMap<String, HashMap<String, HashMap<String, Object>>> storeHouseL2 = storeHouse.get(className);
		if (storeHouseL2 == null)
			throw new CacheStoreHouseException("No level 2 storeHouse for the key.");

		HashMap<String, HashMap<String, Object>> storeHouseL3 = storeHouseL2.get(keyLevel2);
		if (storeHouseL3 == null)
			throw new CacheStoreHouseException("No level 3 storeHouse for the key.");

		HashMap<String, Object> storeHouseL4 = storeHouseL3.get(keyLevel3);
		if (storeHouseL4 == null)
			throw new CacheStoreHouseException("No level 4 storeHouse for the key.");

		Object object = storeHouseL4.get(keyLevel4);
		if (object == null)
			throw new CacheStoreHouseException("No object for the key. key: " + keyLevel4);
		return object;
	}

	/**
	 * Removes the object stored in the storeHouse
	 * 
	 * @param keyLevel1
	 * @param keyLevel2
	 * @param keyLevel3
	 * @param keyLevel4
	 * @throws CacheStoreHouseException
	 */
	public static void remove(@SuppressWarnings("rawtypes") Class keyLevel1, String keyLevel2, String keyLevel3, String keyLevel4)
			throws CacheStoreHouseException {

		store(keyLevel1, keyLevel2, keyLevel3, keyLevel4, null);
	}

	/**
	 * Gets the key from the class object
	 * 
	 * @param keyLevel1
	 * @return returns the key
	 * @throws CacheStoreHouseException
	 */
	private static String getKeyLevel1(@SuppressWarnings("rawtypes") Class keyLevel1) throws CacheStoreHouseException {
		if (keyLevel1 == null)
			throw new CacheStoreHouseException("keyLevel1 cannot be null.");
		String className = keyLevel1.getName();
		if (className == null)
			throw new CacheStoreHouseException("keyLevel1.getName cannot be null.");
		if ("".equals(className))
			throw new CacheStoreHouseException("keyLevel1.getName cannot be empty string.");
		return className;
	}

	// --------------
	// --------------
	// --------------
}
