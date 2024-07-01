
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)
import Data.String (IsString(..))
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.Client (HasClient (..))
import Servant.OpenApi (HasOpenApi(..), toOpenApi)
import qualified Data.Version as Version
import Servant.Swagger.UI (SwaggerSchemaUI, swaggerSchemaUIServer)
import Data.OpenApi (ToSchema, servers, license, info, description, version, title)
import qualified Data.OpenApi as OpenApi
import Control.Concurrent.MVar (MVar, newMVar, modifyMVar, readMVar)
import System.Environment (getEnv)
import Control.Monad.IO.Class (liftIO)
import Control.Lens
import qualified Data.Text as T
import GHC.Generics
import GHC.TypeLits
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import qualified Data.UUID as UUID

-- Define a simple data type
data User = User
  { userId :: UUID
  , name   :: String
  , age    :: Int
  } deriving (Generic, Show)

instance ToJSON User
instance FromJSON User
instance ToSchema User

-- Define the API type
type API = "users" :> OperationId "getUsers" :> Get '[JSON] [User]
      :<|> "users" :> OperationId "insertUser" :> ReqBody '[JSON] User :> Post '[JSON] User
      :<|> "users" :> OperationId "updateUser" :> Capture "id" UUID :> ReqBody '[JSON] User :> Put '[JSON] User
      :<|> "users" :> OperationId "deleteUser" :> Capture "id" UUID :> Delete '[JSON] NoContent

-- Define the API with Swagger endpoints
type APIWithSwagger = API
      :<|> SwaggerSchemaUI "swagger-ui" "openapi.json"


data OperationId (name :: Symbol)

instance HasServer subApi ctx => HasServer (OperationId name :> subApi) ctx where
  type ServerT (OperationId name :> subApi) m = ServerT subApi m
  route _ = route (Proxy @subApi)
  hoistServerWithContext _ = hoistServerWithContext (Proxy @subApi)

instance (HasOpenApi subApi, KnownSymbol name) => HasOpenApi (OperationId name :> subApi) where
  toOpenApi _ = toOpenApi (Proxy @subApi) & OpenApi.allOperations . OpenApi.operationId ?~ apiName
    where
      apiName = T.pack $ symbolVal (Proxy @name)

instance HasClient m api => HasClient m (OperationId name :> api) where
  type Client m (OperationId name :> api) = Client m api
  clientWithRoute pm Proxy = clientWithRoute pm (Proxy :: Proxy api)
  hoistClientMonad pm _ = hoistClientMonad pm (Proxy :: Proxy api)

-- Handlers for the API
getUsers :: MVar [User] -> Handler [User]
getUsers usersVar = liftIO $ readMVar usersVar

addUser :: MVar [User] -> User -> Handler User
addUser usersVar newUser = liftIO $ do
    newId <- nextRandom
    let userWithId = newUser { userId = newId }
    modifyMVar usersVar $ \users -> return (userWithId : users, userWithId)

updateUser :: MVar [User] -> UUID -> User -> Handler User
updateUser usersVar uid updatedUser = liftIO $ do
    modifyMVar usersVar $ \users ->
        let newUsers = map (\user -> if userId user == uid then updatedUser { userId = uid } else user) users
        in return (newUsers, updatedUser { userId = uid })

deleteUser :: MVar [User] -> UUID -> Handler NoContent
deleteUser usersVar uid = liftIO $ do
    modifyMVar usersVar $ \users ->
        let newUsers = filter (\user -> userId user /= uid) users
        in return (newUsers, NoContent)

-- Implement the server
server :: OpenApi.Server -> MVar [User] -> Server APIWithSwagger
server virtualHost usersVar = (getUsers usersVar
              :<|> addUser usersVar
              :<|> updateUser usersVar
              :<|> deleteUser usersVar)
              :<|> swaggerSchemaUIServer (toOpenApi (Proxy :: Proxy API)
                  & info.title        .~ "OpenAPI AI API"
                  & info.version      .~ "1.0"
                  & info.description  ?~ "This is an API for AI with OpenAPI"
                  & info.license      ?~ "MIT"
                  & servers           .~ [virtualHost])

-- Create the application
app :: OpenApi.Server -> MVar [User] -> Application
app virtualHost usersVar = serve (Proxy :: Proxy APIWithSwagger) (server virtualHost usersVar)

-- Main entry point
main :: IO ()
main = do
    initialUsers <- mapM (\(name, age) -> nextRandom >>= \uid -> return (User uid name age)) [("Alice", 30), ("Bob", 25)]
    usersVar <- newMVar initialUsers
    virtualHost <- getEnv "VIRTUAL_HOST"
    port <- read <$> getEnv "PORT"  -- Assuming PORT is a number
    run port (app (fromString virtualHost) usersVar)

