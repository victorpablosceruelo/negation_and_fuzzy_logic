package urls;

import storeHouse.CacheStoreHouse;
import storeHouse.CacheStoreHouseException;
import constants.KConstants;
import constants.KUrls;

public class UrlsMaps {

	private static boolean loaded = false;

	private static String getManager(UrlMap urlMap) {
		String manager = urlMap.getManager();
		return manager;
	}

	private static String getOp(UrlMap urlMap) {
		String op = urlMap.getOp();
		return op;
	}

	public static UrlMap getUrlMap(UrlMap urlMap) throws UrlMapException {
		String manager = getManager(urlMap);
		String op = getOp(urlMap);

		if (!loaded) {
			load(KUrls.urlsList());
		}

		try {
			urlMap = (UrlMap) CacheStoreHouse.retrieve(UrlsMaps.class, manager, manager, op);
		} catch (CacheStoreHouseException e) {
			e.printStackTrace();
			urlMap = null;
		}
		if (urlMap == null)
			throw new UrlMapException("urlMap cannot be null.");
		return urlMap;
	}

	private static void load(UrlMap[] pagesList) {

		for (int i = 0; i < pagesList.length; i++) {
			UrlMap page = pagesList[i];
			try {
				storeMapping(page);
			} catch (CacheStoreHouseException e) {
				e.printStackTrace();
			}
		}
	}

	private static void storeMapping(UrlMap urlMap) throws CacheStoreHouseException {
		String manager = getManager(urlMap);
		String op = getOp(urlMap);

		CacheStoreHouse.store(UrlsMaps.class, manager, manager, op, urlMap);
	}
}

/* --- */
