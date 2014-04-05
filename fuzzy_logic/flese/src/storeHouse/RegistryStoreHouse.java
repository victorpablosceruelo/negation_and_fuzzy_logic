package storeHouse;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Set;

import auxiliar.RegistryEntry;

public class RegistryStoreHouse {
	
	private HashMap<String, RegistryEntry> registryEntries = null;

	public RegistryStoreHouse() {
		resetRegistryEntries();
	}

	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

	public void resetRegistryEntries() {
		this.registryEntries = null;
	}
	
	public void addRegistryEntry(RegistryEntry registryEntry) {
		if (this.registryEntries == null) {
			this.registryEntries = new HashMap<String, RegistryEntry>();
		}
		this.registryEntries.put(registryEntry.getDateOut(), registryEntry);
	}
	
	public String getRegistryEntries() {
		if (this.registryEntries == null) {
			return "";
		}
		
		Set<String> keysSet = this.registryEntries.keySet();
		String [] keys = keysSet.toArray(new String[keysSet.size()]);
		Arrays.sort(keys, Collections.reverseOrder());
		
		StringBuilder registryEntriesSB = new StringBuilder(); 
		registryEntriesSB.append("<tr>");
		registryEntriesSB.append("<th>");
		registryEntriesSB.append("Time In");
		registryEntriesSB.append("</th>");
		registryEntriesSB.append("<th>");
		registryEntriesSB.append("Time Out");
		registryEntriesSB.append("</th>");
		registryEntriesSB.append("<th>");
		registryEntriesSB.append("Manager");
		registryEntriesSB.append("</th>");
		registryEntriesSB.append("<th>");
		registryEntriesSB.append("Operation");
		registryEntriesSB.append("</th>");
		registryEntriesSB.append("<th>");
		registryEntriesSB.append("Info");
		registryEntriesSB.append("</th>");
		registryEntriesSB.append("<th>");
		registryEntriesSB.append("Next Step");
		registryEntriesSB.append("</th>");
		registryEntriesSB.append("</tr>");
		
		for (int i=0; i<keys.length; i++) {
			RegistryEntry registryEntry = this.registryEntries.get(keys[i]);
			registryEntriesSB.append("<tr>");
			registryEntriesSB.append("<td>");
			registryEntriesSB.append(registryEntry.getDateIn());
			registryEntriesSB.append("</td>");
			registryEntriesSB.append("<td>");
			registryEntriesSB.append(registryEntry.getDateOut());
			registryEntriesSB.append("</td>");
			registryEntriesSB.append("<td>");
			registryEntriesSB.append(registryEntry.getManager());
			registryEntriesSB.append("</td>");
			registryEntriesSB.append("<td>");
			registryEntriesSB.append(registryEntry.getOp());
			registryEntriesSB.append("</td>");
			registryEntriesSB.append("<td>");
			registryEntriesSB.append(registryEntry.getMsg());
			registryEntriesSB.append("</td>");
			registryEntriesSB.append("<td>");
			registryEntriesSB.append(registryEntry.getNextStep());
			registryEntriesSB.append("</td>");
			registryEntriesSB.append("</tr>");
		}
		return registryEntriesSB.toString();
	}
	
}
