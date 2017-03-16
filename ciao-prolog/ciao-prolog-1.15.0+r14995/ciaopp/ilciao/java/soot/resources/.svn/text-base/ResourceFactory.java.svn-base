package soot.resources;

import soot.BlockMethod;
import soot.Scene;
import soot.SootClass;
import soot.jimple.InvokeStmt;
import soot.resources.annotations.Resources;
import soot.util.SootUtil;

import java.util.*;


public class ResourceFactory {

  private Map<Resource, ResourceFunction> resources;
  private static ResourceFactory instance = null;

  private ResourceFactory() {
    resources = new HashMap<Resource, ResourceFunction>();
    reloadResources();
  }

  public static ResourceFactory getInstance() {
    if (instance == null) {
      instance = new ResourceFactory();
    }
    return instance;
  }

  /**
   * Resources are retrieved from the @Resources annotation in the main class.
   * The Size resource does not need to be indicated but it is always included.
   *
   * @see soot.resources.annotations.Resources
   * @see soot.resources.annotations.Size
   */
  private void reloadResources() {
    SootClass mainClass = Scene.v().getMainClass();
    Resources analyzeResources = (Resources) SootUtil.getAnnotation(mainClass, Resources.class);
    if (analyzeResources == null) {
      throw new RuntimeException(
          "There is no annotation containing which resources to analyze in " + "class " +
              mainClass.getName());
    } else {
      List<Resource> resources = new ArrayList<Resource>();
      resources.addAll(Arrays.asList(analyzeResources.value()));
      resources.add(Resource.ANY_RESOURCE);
      resources.add(Resource.SIZE); //size is always a resource
      for (Resource resource : resources) {
        ResourceFunction resourceFunction = resource.getResourceFunction();
        this.resources.put(resource, resourceFunction);
      }
    }
  }

  public String getCost(String resourceName, BlockMethod blockMethod) {
    ResourceFunction resourceFunction = getResourceFunction(resourceName);
    return resourceFunction.getCost(blockMethod);
  }

  public String getCost(String resourceName, InvokeStmt invokeStmt) {
    ResourceFunction resourceFunction = getResourceFunction(resourceName);
    return resourceFunction.getCost(invokeStmt);
  }

  private ResourceFunction getResourceFunction(String resourceName) {
    for (Resource resource : resources.keySet()) {
      if (resource.name().equals(resourceName)) {
        return resources.get(resource);
      }
    }
    throw new IllegalArgumentException("The specified resource name:" + resourceName +
        " is not included in the list of resources to analyze: " + getResourceNames());
  }

  public Collection<String> getResourceNames() {
    List<String> resourceNames = new ArrayList<String>();
    for (Resource resource : resources.keySet()) {
      if (resource != Resource.SIZE && resource != Resource.ANY_RESOURCE) {
        resourceNames.add(resource.name());
      }
    }
    return resourceNames;
  }
}
