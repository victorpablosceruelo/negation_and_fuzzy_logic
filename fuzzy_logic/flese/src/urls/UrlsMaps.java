package urls;

import storeHouse.CacheStoreHouse;
import constants.KPages;

public class UrlsMaps {

	private static boolean loaded = false;

	public static UrlMap getUrlMap(String key) throws Exception {
		if (!loaded) {
			load(KPages.pagesList);
		}
		String auxKey = key.toUpperCase(); // HashMap keys in uppercase.
		return (UrlMap) CacheStoreHouse.retrieve(UrlsMaps.class, auxKey);
	}

	private static void load(UrlMap[] pagesList) throws Exception {

		for (int i = 0; i < pagesList.length; i++) {
			storeMapping(pagesList[i]);
		}
	}

	private static void storeMapping(UrlMap urlMap) throws Exception {
		String key = urlMap.getKeyString();
		String auxKey = key.toUpperCase(); // HashMap keys in uppercase.
		CacheStoreHouse.store(UrlsMaps.class, auxKey, urlMap);
	}

}

/* --- */
