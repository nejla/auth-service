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

## Creating header signing keys

We need a ed25519 keypair to sign and verify headers. You can create it using
`openssl` and `base64` like this (make sure to set the right permissiosn):

```sh
openssl genpkey -algorithm ed25519 -outform der | base64 > ed25519.priv.der
chmod 0600 ed25519.priv.der
```

and

```sh
base64 -d ed25519.priv.der \
  | openssl pkey -inform der -pubout -outform der \
  | base64 > ed25519.pub.der
```

When starting the auth-service container, we need to mount the private key to
`/run/secrets/header_signing_private_key` (or set
`SIGNED_HEADERS_PRIVATE_KEY_PATH` to the path where we put it)
The key has to be passed as a file and not an environment variable to avoid it leaking.

The public key does not have to be secured and can be passed to the backend
e.g. as an environment variable.


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
