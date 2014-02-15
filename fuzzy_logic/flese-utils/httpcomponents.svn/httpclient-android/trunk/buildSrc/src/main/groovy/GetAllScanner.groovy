

import org.reflections.scanners.AbstractScanner

class GetAllScanner extends AbstractScanner {

    @Override
    public void scan(final Object cls) {
        String className = getMetadataAdapter().getClassName(cls)
        getStore().put(className, className)
    }

}
