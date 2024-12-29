case class Query(query: String, value: String)
Object Query {
    def apply(query: String) = new {
        def === (value: String) = Query(query, value)
}