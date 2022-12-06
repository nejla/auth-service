# Using auth-service with header signing

When writing a backend service we will want to access information about the
authenticated user. To enable this, auth-service provides a library,
`auth-service-core`.

## Adding auth-service integration to the backend

To add auth-service, first we need to add the repositories to the `stack.yaml`:

```yaml
extra-deps:
  - git: git@git.nejla.com:nejla-ab/auth-service.git
    commit: c55a328bd8bbaee461e1e0c49ecf87eb88ec731a
    subdirs:
      - auth-service-core
  - git: git@git.nejla.com:nejla-ab/signed-auth.git
    commit: 094579cbf2b6017d2f4b601c2ec871958407d401
```

Replacing the commits with the respective commits we want to use (or a tag).

Next we need to add the `auth-service-core` library to the haskell project by
including it as a dependency either in the cabal file or `stack.yaml`:

```yaml
dependencies:
  - auth-service-core
```

## API for internal requests

Your application might want to talk to auth-service e.g. to retrieve user
information. To that end, auth-service contains an internal API

To use the API, you need to declare an API secret. First, generate a random key, e.g.
```
dd if=/dev/random of=/dev/stdout bs=16 count=1 | base64
```
then set the `SERVICE_TOKEN` environment variable in auth-service:

```yaml
auth-service:
    environment:
      - SERVICE_TOKEN=Q0rp0I5B5VChHu40i47YRA
```

### API definition

All API endpoints require that the `X-Token` is set to the secret generated in the previous section

####  `/service/users/by-uid`
  * Takes 1..n query parameters `uid={uid}` and returns  user information for these users

  Example: `/service/users/by-uid?uid=68917a28-d8c5-42df-88e2-db97b881321b&uid=0867dc87-ec9a-4e14-8d7a-9e6e260c6390`

  * Returns a list user objects

  * `id`: ID of the found user
  * `info` (optional): User info object if the user exists, absent otherwise

  User info:
  * `id`: ID of the user
  * `email`: registered email address
  * `name`: Full name
  * `phone` (optional): Phone number registered for TFA
  * `instances`: List of instance IDs the user has access to
  * `roles`: List of user's roles
  * `deactivate` (optional): Timestamp when the user was deactivated

### Using servant-client

You can connect to the API using servant client:

```
import qualified Servant.Client       as Client
import           AuthService.Api
import qualified AuthService.Types    as AuthService

-- Extract the API endpoints from the API definition
getUsersC = Client.client (Proxy :: Proxy ServiceAPI)
```

You can then use
[Client.client](https://hackage.haskell.org/package/servant-client-0.18.3/docs/Servant-Client.html#v:client)
to interact with the endpoint; don't forget to pass the secret as the first argument.

## Resolving credentials in the backend

To resolve credentials in our backend, we first include auth-service-core in our
haskell sources:

```haskell
import           AuthService.SignedHeaders (AuthHeader)
import qualified AuthService.SignedHeaders as Auth
```

Now we can use `resolveAuthHeader` to resolve authentication credentials. It
acts as a middleware for our WAI-application.

We need to pass it an `AuthContext` containing the public key to verify the
header signature, a *nonce frame* (created by `newFrame`) and a logging function.

To get the public key We use `readPublicKeyDer`, e.g. by reading it from an environment variable. It expects the key to be base64-encoded DER (like we produced earlier with openssl).

Example:

```haskell
import qualified AuthService.SignedHeaders   as Auth
import qualified System.Posix.Env.ByteString as Env

getHeaderPublicKey :: IO Auth.PublicKey
getHeaderPublicKey = do
  mbKeyTxt <- Env.getEnv "SIGNED_HEADER_PUBLIC_KEY"
  case mbKeyTxt of
    Nothing -> do
      hPutStrLn stderr $ "SIGNED_HEADER_PUBLIC_KEY must be set"
      exitFailure
    Just keyTxt -> case Auth.readPublicKeyDer keyTxt of
      Left e -> do
        hPutStrLn stderr $ "Could not parse public key" <> e
        exitFailure
      Right key -> return key

```

Unlike other middlewares (of type
`Middleware`) `resolveAuthHeader` passed the resolved information as a parameter to the next
`Application`. We can then use the resolved user information e.g. in logging or
pass it to servant where we can capture them using `AuthJWS`


Here's an example:


```haskell

import           AuthService.SignedHeaders (AuthHeader)
import qualified AuthService.SignedHeaders as Auth

serveApp ::
     Auth.Frame
  -> Updates
  -> App.ConnectionPool
  -> Auth.PublicKey
  -> LogFun
  -> Application
serveApp frame updates pool pubKey logfun =
  -- On every request, check for X-Auth header and parse it, checking
  -- the signature and nonce. Passes on the resolved header
  -- If Resolution fails, 403 is returned
  Auth.resolveAuthHeader authCtx $ \authHeader ->
    -- Log the basic request data using the user data from the resolved
    -- auth header
    Auth.logRequestBasic authCtx authHeader $
    -- Pass the user information to servant
    serveWithContext api (ctx authHeader frame)
      $ handler updates pool conf
  where
    -- The required context to resolve the authentication header.
    -- It requires the public key to verify the signature,
    -- a nonce-frame (for checking that nonces are unique)
    -- and a log function
    authCtx = Auth.AuthContext (pubKey)
                               frame
                               (logFun)

    ctx authHeader frame = authHeader :. EmptyContext

-- Define your servant API...
api = _

-- And your handler
handler = _
```

## Using auth-service credentials in servant

We can use the "AuthCredentials" combinator to retrieve the credentials. It
accepts either an `'AuthOptional` parameter, in which case the handler should
expect a `Maybe AuthHeader` or `'AuthRequired`, which will automatically reject
requests without credentials with status code 403 and always passes `AuthHeader`
to the corresponding handler.

```haskell
type myAPI = AuthCredentials 'AuthOptional AuthHeader
           :> [...]

```

## Logging user requests

We can use `logRequestBasic` to logbasic facts about an HTTP requests including
resolved authentication details as json

Fields:
  * time: ISO 861 time when the request was received
  * path: Request path
  * user: Information about the user making the request (if available)
  * reponse_status: Numeric HTTP response status
  * response_time_ms: Number of milliseconds taken to formulate a response

User has the following fields:
  * name: Name of the user
  * email: Email address
  * id: Unique user ID of the user
