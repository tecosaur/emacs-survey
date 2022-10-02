using SearchLight
using Genie

function Genie.Renderer.Json.JSON3.StructTypes.StructType(::Type{T}) where {T<:SearchLight.AbstractModel}
  Genie.Renderer.Json.JSON3.StructTypes.Struct()
end

function Genie.Renderer.Json.JSON3.StructTypes.StructType(::Type{SearchLight.DbId})
  Genie.Renderer.Json.JSON3.StructTypes.Struct()
end

SearchLight.Configuration.load(context = @__MODULE__)
SearchLight.connect()

if !(SearchLight.config.db_migrations_table_name in
      SearchLight.query("SELECT name FROM sqlite_master  WHERE type='table'").name)
    SearchLight.Migration.create_migrations_table()
    SearchLight.Migrations.last_up() # probably a good idea
end
