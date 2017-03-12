package misc

/**
  * Created by Michael on 06/06/2016.
  */
class ResourceManagement
{
    /**
      * From the man himself :P
      *
      * "using <resourceName> { <resource> => { ... block of code ... } }
      *
      * @param resource
      * @param block
      * @tparam T
      */
    def using[T <: { def close() }]
    (resource: T)
    (block: T => Unit)
    {
        try {
            block(resource)
        } finally {
            if (resource != null) resource.close()
        }
    }
}
