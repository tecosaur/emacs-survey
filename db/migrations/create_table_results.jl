module CreateTableResults

import SearchLight.Migrations: create_table, column, columns, primary_key, add_index, drop_table, add_indices

function up()
  create_table(:results) do
    [
      primary_key()
      column(:column_name, :column_type)
      columns([
        :column_name => :column_type
      ])
    ]
  end

  add_index(:results, :column_name)
  add_indices(results, :column_name_1, :column_name_2)
end

function down()
  drop_table(:results)
end

end
