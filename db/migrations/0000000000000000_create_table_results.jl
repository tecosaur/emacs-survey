module CreateTableResults

import SearchLight.Migrations: create_table, column, columns, primary_key, add_index, drop_table, add_indices

function up()
    create_table(:surveys) do
        [
            primary_key(:id),
            column(:name, :text),
            column(:repr, :text, not_null=true)
        ]
    end

    create_table(:questions) do
        [
            column(:survey, :integer, not_null=true),
            column(:id, :text, not_null=true),
            column(:type, :text, not_null=true),
            column(:prompt, :text, not_null=true),
            column(:input, :text, not_null=true)
        ]
    end

    create_table(:responses) do
        [
            column(:survey, :integer, not_null=true),
            column(:id, :integer, not_null=true),
            column(:exip, :integer, not_null=true),
            column(:started, :text, not_null=true),
            column(:completed, :text),
            column(:page, :integer, not_null=true),
        ]
    end

    create_table(:results) do
        [
            column(:survey, :integer, not_null=true),
            column(:response, :integer, not_null=true),
            column(:question, :integer, not_null=true),
            column(:value, :text, not_null=true)
        ]
    end

    add_index(:questions, :survey)
    add_index(:results, :survey)
    add_indices(:results, [:response, :question])
end

function down()
    drop_table(:surveys)
    drop_table(:questions)
    drop_table(:results)
end

end
