CREATE TABLE "user"
  (
     "uuid"          UUID NOT NULL,
     "email"         VARCHAR NOT NULL,
     "name"          VARCHAR NOT NULL,
     "password_hash" BYTEA NOT NULL,
     "phone"         VARCHAR NULL,
     PRIMARY KEY ("uuid")
  );

ALTER TABLE "user"
  ADD CONSTRAINT "unique_user_email" UNIQUE("email");

CREATE TABLE "user_role"
  (
     "user" UUID NOT NULL,
     "role" VARCHAR NOT NULL,
     PRIMARY KEY ("user", "role")
  );

ALTER TABLE "user_role"
  ADD CONSTRAINT "unique_user_role" UNIQUE("user", "role");

ALTER TABLE "user_role"
  ADD CONSTRAINT "user_rolefk_user" FOREIGN KEY("user") REFERENCES "user"("uuid"
  );

CREATE TABLE "instance"
  (
     "uuid" UUID NOT NULL,
     "name" VARCHAR NOT NULL,
     PRIMARY KEY ("uuid")
  );

ALTER TABLE "instance"
  ADD CONSTRAINT "unique_instance_name" UNIQUE("name");

CREATE TABLE "user_otp"
  (
     "id"          SERIAL8 PRIMARY KEY UNIQUE,
     "user"        UUID NOT NULL,
     "password"    VARCHAR NOT NULL,
     "created"     TIMESTAMP WITH TIME zone NOT NULL,
     "deactivated" TIMESTAMP WITH TIME zone NULL
  );

ALTER TABLE "user_otp"
  ADD CONSTRAINT "user_otpfk_u_ser" FOREIGN KEY("user") REFERENCES "user"("uuid"
  );

CREATE TABLE "user_instance"
  (
     "user"        UUID NOT NULL,
     "instance_id" UUID NOT NULL,
     PRIMARY KEY ("user", "instance_id")
  );

ALTER TABLE "user_instance"
  ADD CONSTRAINT "user_instancefk_user" FOREIGN KEY("user") REFERENCES "user"(
  "uuid");

ALTER TABLE "user_instance"
  ADD CONSTRAINT "user_instancefk_instance" FOREIGN KEY("instance_id")
  REFERENCES "instance"("uuid");

CREATE TABLE "token"
  (
     "id"          SERIAL8 PRIMARY KEY UNIQUE,
     "token"       VARCHAR NOT NULL,
     "user"        UUID NOT NULL,
     "created"     TIMESTAMP WITH TIME zone NOT NULL,
     "expires"     TIMESTAMP WITH TIME zone NULL,
     "last_use"    TIMESTAMP WITH TIME zone NULL,
     "deactivated" TIMESTAMP WITH TIME zone NULL
  );

ALTER TABLE "token"
  ADD CONSTRAINT "unique_token" UNIQUE("token");

ALTER TABLE "token"
  ADD CONSTRAINT "tokenfk_user" FOREIGN KEY("user") REFERENCES "user"("uuid");

CREATE TABLE "password_reset_token"
  (
     "id"      SERIAL8 PRIMARY KEY UNIQUE,
     "token"   VARCHAR NOT NULL,
     "user"    UUID NOT NULL,
     "created" TIMESTAMP WITH TIME zone NOT NULL,
     "expires" TIMESTAMP WITH TIME zone NOT NULL,
     "used"    TIMESTAMP WITH TIME zone NULL
  );

ALTER TABLE "password_reset_token"
  ADD CONSTRAINT "unique_password_reset_token" UNIQUE("token");

ALTER TABLE "password_reset_token"
  ADD CONSTRAINT "password_reset_tokenfk_user" FOREIGN KEY("user") REFERENCES
  "user"("uuid");
