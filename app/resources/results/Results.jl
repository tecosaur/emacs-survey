module Results

import SearchLight: AbstractModel, DbId
import Base: @kwdef

export Result

@kwdef mutable struct Result <: AbstractModel
  id::DbId = DbId()
end

end
