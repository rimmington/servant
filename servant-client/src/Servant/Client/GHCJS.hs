module Servant.Client.GHCJS (setRQFormData) where

import JavaScript.FormData (FormDataProp)
import Servant.Common.Req (Req (..))
import Servant.Client.PerformRequest.GHCJS.Types (RequestBody (RequestBodyFormProps))

-- TODO: Content-Type?
setRQFormData :: [FormDataProp] -> Req -> Req
setRQFormData ps r = r { reqBody = RequestBodyFormProps ps }
