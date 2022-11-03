# SAML

## Getting started

* To start an example keycloak instance, run `docker-compose up -d` in the SAML folder

## Keycloak configuration
### Creating client
 * Configure -> Clients -> Create
 * Client id: authservice (or the name of the SP)
   * This has to match the `SAML_AUDIENCE` setting
 * Client protocol: SAML

### Minimal changes from default settings
 * root url (e.g. `http://localhost:8000/` for the example)
 * Under Keys:
 * Client Signature required: OFF
   * wai-saml2 doesn't handle client requests
   * Not implemented in auth-service
   * Leads to error: "Invalid requester"
 * Encrypt Assertions: ON
   * Required by wai-saml2
   * See https://github.com/mbg/wai-saml2/issues/5
   * See next section

### Generating and installing encryption key
  * This generates the RSA encryption key pair belonging to the client (auth-service)
  * generate the private key pair: `openssl genrsa -out key.pem 4096`
  * generate the certificate: `openssl req -new -x509 -key key.pem -out cert.pem -days 360`
  * Copy private key, store in `SAML/config/{instance name}/key.pem`
  * Go to client settings, keys, under "Encryption keys config" enable "encrypt assertions"
  * Import the certificate

### Getting the realm certificate
  * Realm Settings
  * Tab "Keys" -> "Active"
  * Under "Algorithm": `RS256`, "Use": `Sig`
  * Chose "Certificate", copy/paste and save under `SAML/config/{instance name}/certcertificate.pem`
  * Add PEM markers:
```
-----BEGIN CERTIFICATE-----
<copied certificate data>
-----END CERTIFICATE-----
```

### Setting up user attributes
auth-service requires some attributes to function, you can set them up in keycloak:
  * under clients -> auth-service -> Mappers, then for each of them create
  * name: email
    * Mapper type: User Property
    * Property: email
    * SAML Attribute name: email
  * name: User name
    * Mapper type: Javascript Mapper
    * Script: `user.getFirstName() + " " + user.getLastName()`
    * SAML Attribute name: name
  * Make sure that all users have email and name set (keycloak admin by default does not)

## Configuring auth-service
The following configuration options are required
  * `SAML_CONFIG_PATH`: Path to the configuration directory

The directory should contain a number of sub-directories, one for each instance that should have SAML enabled.
  * The names of the subdirectories are not relevant, you can choose them freely
  * Each directory should contain the following three files
  * `key.pem`: client's PEM-encoded private RSA key, used to decrypt SAML assertions.
  * `certificate.pem`: IdP's PEM-encoded X509 signing certificate. Used to check signed assertions. (the "realm certificate")
  * `config`: a simple "key=value" encoded configuration for the instance, with the following fields:
    * `audience`: the audience for SAML assertions (has to match the client name in keycloak or other IdPs)
    * `idp_request_url`: The IdP's URL for authentication requests
    * `instance`: The auth-service instance this configuration applies to. Please note that each instance can only have **one** SAML configuration, multiple configurations will overwrite each other in an unspecified order.
    * `redirect_after_login`: URL to redirect after SAML login succeeds

Example for the `config` file:
```
audience=authservice
idp_request_url=http://localhost:8070/realms/master/protocol/saml
instance=657b5108-7559-4b8e-a643-dd0cc29b9e34
redirect_after_login=/index.html
```

## Start auth-service
  * E.g. `make up && docker-compose logs --tail=50 --follow`

## Testing SAML SSO:

If the example server is running, you can navigate your browser to
`http://localhost:8000/api/sso/login` to test the SSO login
