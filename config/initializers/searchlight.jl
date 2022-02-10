using SearchLight

try
    SearchLight.Configuration.load()

    if SearchLight.config.db_config_settings["adapter"] !== nothing
        eval(Meta.parse("using SearchLight$(SearchLight.config.db_config_settings["adapter"])"))
        SearchLight.connect()

        if !(SearchLight.config.db_migrations_table_name in
             SearchLight.query("SELECT name FROM sqlite_master  WHERE type='table'").name)
            SearchLight.Migration.create_migrations_table()
            SearchLight.Migrations.last_up() # probably a good idea
        end

    end
catch ex
    @error ex
end
