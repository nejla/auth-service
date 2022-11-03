CREATE TABLE assertion_id
  ( "value" text PRIMARY KEY );

CREATE TABLE sso_token
  ( "token" text PRIMARY KEY
  , "user_id" text NOT NULL
  , "email" text NOT NULL
  , "name" text NOT NULL
  , "instance_id" uuid NOT NULL
  , "created" TIMESTAMP WITH TIME ZONE NOT NULL
  , "expires" TIMESTAMP WITH TIME ZONE
  , "last_use" TIMESTAMP WITH TIME ZONE
  , "deactivated" TIMESTAMP WITH TIME ZONE
  );

ALTER TABLE sso_token ADD CONSTRAINT "sso_tokenfk_instance"
  FOREIGN KEY("instance_id") REFERENCES "instance"("uuid");

CREATE TABLE sso_token_role
  ( "token" text
  , "role" text
  , PRIMARY KEY ("token", "role")
  );

ALTER TABLE sso_token_role ADD CONSTRAINT "sso_token_rolefk_token"
  FOREIGN KEY("token") REFERENCES "sso_token"("token") ON DELETE CASCADE;
