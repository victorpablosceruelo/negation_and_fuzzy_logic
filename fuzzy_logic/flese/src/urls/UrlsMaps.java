package urls;

import storeHouse.CacheStoreHouse;
import storeHouse.CacheStoreHouseException;
import constants.KUrls;

public class UrlsMaps {

	private static boolean loaded = false;

	private static String getManager(UrlMap urlMap) {
		String manager = urlMap.getManager();
		if ((manager == null) || ("".equals(manager)))
			manager = "defaultManager";	
		return manager;
	}
	
	private static String getOp(UrlMap urlMap) {
		String op = urlMap.getOp();
		if ((op == null) || ("".equals(op)))
				op = "defaultOp";
		return op;
	}
	
	public static UrlMap getUrlMap(UrlMap urlMap) throws CacheStoreHouseException {
		String manager = getManager(urlMap);
		String op = getOp(urlMap);
		
		if (!loaded) {
			load(KUrls.urlsList());
		}

		return (UrlMap) CacheStoreHouse.retrieve(UrlsMaps.class, manager, manager, op);
	}

	private static void load(UrlMap[] pagesList) throws CacheStoreHouseException {

		for (int i = 0; i < pagesList.length; i++) {
			storeMapping(pagesList[i]);
		}
	}

	private static void storeMapping(UrlMap urlMap) throws CacheStoreHouseException {
		String manager = getManager(urlMap);
		String op = getOp(urlMap);
		
		CacheStoreHouse.store(UrlsMaps.class, manager, manager, op, urlMap);
	}
}

/* --- */
